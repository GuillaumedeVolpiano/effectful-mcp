-- |
-- Module      : Effectful.MCP.Server
-- Description : An MCP server of tools, over stdio
-- License     : BSD-3-Clause
--
-- A server of the Model Context Protocol which offers tools, for clients
-- of two eras, as revision 2026-07-28 allows a server to be.
--
-- * Revision 2026-07-28 is stateless: every request carries its protocol
--   version and the capabilities of the client in its @_meta@, and is
--   served on its own. The server answers @server/discover@, @tools/list@
--   and @tools/call@; a request in another version gets an
--   @UnsupportedProtocolVersionError@.
--
-- * Earlier revisions (2025-11-25 and before) start with @initialize@.
--   That request selects their semantics for the connection: from then
--   on, requests without the @_meta@ of the new revision are served as
--   those revisions describe (@tools/list@, @tools/call@, @ping@).
--
-- Both are served at once on one connection: what a request carries tells
-- which it is.
--
-- Requests are served concurrently, each in a thread of its own, as
-- Effectful forks threads: a tool that changes a local @State@ does so in
-- its own copy. @notifications/cancelled@ stops a request, which then gets
-- no answer.
module Effectful.MCP.Server
  ( -- * Configuration
    ServerConfig (..)
  , serverConfig
  , Implementation (..)
  , implementation
  , protocolVersion
  , legacyVersions

    -- * Serving
  , serveStdio
  , serve
  , handleRequest
  , handleLegacyRequest
  ) where

import Control.Monad (forM_, void)
import Data.Aeson (Object, ToJSON (..), Value (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Concurrent.Async (Async, async, cancel, race, waitCatch)
import Effectful.Concurrent.MVar (newMVar, withMVar)
import Effectful.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Effectful.Exception (trySync)
import Effectful.MCP.JsonRpc
import Effectful.MCP.Tool (Tool (..), errorResult, toolDefinition, toolResultValue)
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream qualified as Stream
import System.IO qualified as IO

-- | The stateless revision of the protocol this server speaks.
protocolVersion :: Text
protocolVersion = "2026-07-28"

-- | The earlier revisions this server speaks to a client that starts with
-- @initialize@, the latest first. For a server that only offers tools,
-- they differ by fields that older clients leave aside.
legacyVersions :: [Text]
legacyVersions = ["2025-11-25", "2025-06-18", "2025-03-26", "2024-11-05"]

-- | The name and version of a server, as it reports them.
data Implementation = Implementation
  { name :: Text
  , version :: Text
  , title :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | A name and a version, without title. Field names such as @name@ are
-- shared with 'Tool', which makes record syntax ambiguous without
-- @DuplicateRecordFields@.
implementation :: Text -> Text -> Implementation
implementation name version = Implementation {name, version, title = Nothing}

instance ToJSON Implementation where
  toJSON Implementation {name, version, title} =
    object (["name" .= name, "version" .= version] <> catMaybes [("title" .=) <$> title])

-- | What a server is: who it says it is, what it offers and how it
-- behaves. Start from 'serverConfig'.
data ServerConfig es = ServerConfig
  { info :: Implementation
  , instructions :: Maybe Text
  -- ^ Guidance for the model on how to use the server.
  , tools :: [Tool es]
  -- ^ Listed in this order.
  , cacheTtlMs :: Int
  -- ^ How long a client may keep the discovery and the list of tools, in
  -- milliseconds.
  , shutdownGrace :: Int
  -- ^ Once the input ends, how long the requests still running may take to
  -- finish, in microseconds, before they are stopped.
  }

-- | A server with no instructions, a cache of five minutes and two seconds
-- to finish at shutdown.
serverConfig :: Implementation -> [Tool es] -> ServerConfig es
serverConfig info tools =
  ServerConfig {info, instructions = Nothing, tools, cacheTtlMs = 300_000, shutdownGrace = 2_000_000}

-- | Serve on the standard input and output: one message per line. Ends
-- when the input ends.
serveStdio :: (IOE :> es, Concurrent :> es) => ServerConfig es -> Eff es ()
serveStdio config = do
  liftIO $ do
    IO.hSetBinaryMode IO.stdin True
    IO.hSetBinaryMode IO.stdout True
  serve config readLine writeLine
  where
    readLine =
      liftIO $
        IO.isEOF >>= \case
          True -> pure Nothing
          False -> Just . stripReturn <$> Char8.hGetLine IO.stdin
    stripReturn line = if Char8.isSuffixOf "\r" line then ByteString.init line else line
    writeLine line = liftIO (Char8.hPutStrLn IO.stdout line >> IO.hFlush IO.stdout)

-- | A request being served: registered before it starts, so that its
-- answer can tell whether it was cancelled meanwhile.
newtype Running es = Running (Maybe (Async ()))

-- | Serve messages read one line at a time, until the input ends.
-- 'Nothing' ends the input. Answers are written whole, one at a time.
serve
  :: Concurrent :> es
  => ServerConfig es
  -> Eff es (Maybe ByteString)
  -> (ByteString -> Eff es ())
  -> Eff es ()
serve config readLine writeLine = do
  lock <- newMVar ()
  running <- newTVarIO Map.empty
  -- Whether a client has selected the earlier revisions with initialize.
  legacy <- newTVarIO False
  let send line = withMVar lock (const (writeLine line))
  Stream.fold (Fold.drainMapM (dispatch legacy running send)) $
    Stream.filter (not . Char8.all (`elem` (" \t" :: String))) (Stream.unfoldrM (const (fmap (,()) <$> readLine)) ())
  shutDown running
  where
    dispatch legacy running send line = case decodeIncoming line of
      Request requestId method params -> do
        -- The era is settled before the request starts, so that what
        -- follows an initialize is read as it should be.
        era <- atomically $ do
          if method == "initialize" then writeTVar legacy True else pure ()
          selected <- readTVar legacy
          pure (if selected && not (carriesModernMeta params) then handleLegacyRequest else handleRequest)
        start running send requestId (era config method params)
      Notification "notifications/cancelled" params ->
        forM_ (KeyMap.lookup "requestId" params >>= parseMaybe Aeson.parseJSON) (stop running)
      Notification _ _ -> pure ()
      Response -> pure ()
      Invalid requestId err -> send (encodeError requestId err)

    start running send requestId answerRequest = do
      atomically (modifyTVar' running (Map.insert requestId (Running Nothing)))
      worker <- async $ do
        outcome <- trySync answerRequest
        let answer = case outcome of
              Right (Right result) -> encodeResult requestId result
              Right (Left err) -> encodeError (Just requestId) err
              Left exception -> encodeError (Just requestId) (internalError (Text.pack (show exception)))
        stillWanted <- atomically $ do
          current <- readTVar running
          writeTVar running (Map.delete requestId current)
          pure (Map.member requestId current)
        if stillWanted then send answer else pure ()
      -- Record the thread, unless the request is already over.
      atomically $ modifyTVar' running (Map.adjust (const (Running (Just worker))) requestId)

    stop running requestId = do
      entry <- atomically $ do
        current <- readTVar running
        writeTVar running (Map.delete requestId current)
        pure (Map.lookup requestId current)
      forM_ entry $ \(Running worker) -> forM_ worker cancel

    -- Let the requests still running finish, within the grace period,
    -- then stop the others.
    shutDown running = do
      workers <- runningWorkers <$> readTVarIO running
      void (race (threadDelay config.shutdownGrace) (mapM_ waitCatch workers))
      readTVarIO running >>= mapM_ cancel . runningWorkers

    runningWorkers current = [worker | Running (Just worker) <- Map.elems current]

-- | Whether a request carries the protocol version of the stateless
-- revision, which makes it one of that revision whatever came before.
carriesModernMeta :: Object -> Bool
carriesModernMeta params = case KeyMap.lookup "_meta" params of
  Just (Object meta) -> KeyMap.member "io.modelcontextprotocol/protocolVersion" meta
  _ -> False

-- | Answer one request of the earlier revisions: a result, or a protocol
-- error. @initialize@ answers the version asked when it is one of
-- 'legacyVersions', and the latest of them otherwise, which the client
-- takes or leaves.
handleLegacyRequest :: ServerConfig es -> Text -> Object -> Eff es (Either ErrorObject Value)
handleLegacyRequest config method params = case method of
  "initialize" ->
    pure . Right . object $
      [ "protocolVersion" .= negotiated
      , "capabilities" .= object ["tools" .= object ["listChanged" .= False]]
      , "serverInfo" .= config.info
      ]
        <> catMaybes [("instructions" .=) <$> config.instructions]
  "ping" -> pure (Right (object []))
  "tools/list"
    | KeyMap.member "cursor" params -> pure (Left (invalidParams "Unknown cursor: the list has one page"))
    | otherwise -> pure (Right (object ["tools" .= map toolDefinition config.tools]))
  "tools/call" -> callToolWith object config params
  _ -> pure (Left (methodNotFound ("Method not found: " <> method)))
  where
    requested = stringField "protocolVersion" params
    negotiated = if requested `elem` legacyVersions then requested else maybe protocolVersion id (listToMaybe legacyVersions)

-- | Answer one request of the stateless revision: a result, or a protocol
-- error. An @initialize@, which belongs to the earlier revisions, is
-- answered as 'handleLegacyRequest' does.
handleRequest :: ServerConfig es -> Text -> Object -> Eff es (Either ErrorObject Value)
handleRequest config method params
  | method == "initialize" = handleLegacyRequest config method params
  | otherwise = case checkMeta params of
      Left err -> pure (Left err)
      Right () -> case method of
        "server/discover" ->
          pure . Right . complete config $
            cacheable config
              <> [ "supportedVersions" .= [protocolVersion]
                 , "capabilities" .= object ["tools" .= object []]
                 ]
              <> catMaybes [("instructions" .=) <$> config.instructions]
        "tools/list"
          | KeyMap.member "cursor" params -> pure (Left (invalidParams "Unknown cursor: the list has one page"))
          | otherwise -> pure . Right . complete config $ ("tools" .= map toolDefinition config.tools) : cacheable config
        "tools/call" -> callToolWith (complete config) config params
        _ -> pure (Left (methodNotFound ("Method not found: " <> method)))

-- | Call a tool, and wrap the fields of its result as a revision wants.
callToolWith :: ([(Aeson.Key, Value)] -> Value) -> ServerConfig es -> Object -> Eff es (Either ErrorObject Value)
callToolWith wrap config params = case KeyMap.lookup "name" params of
  Just (String toolName) -> case find (\candidate -> candidate.name == toolName) config.tools of
    Nothing -> pure (Left (invalidParams ("Unknown tool: " <> toolName)))
    Just found -> case KeyMap.lookup "arguments" params of
      Nothing -> run found (Object KeyMap.empty)
      Just arguments@(Object _) -> run found arguments
      Just _ -> pure (Left (invalidParams "arguments must be an object"))
  _ -> pure (Left (invalidParams "name must be a string"))
  where
    run found arguments = do
      outcome <- trySync (found.call arguments)
      let result = either (\exception -> errorResult ("Internal error: " <> Text.pack (show exception))) id outcome
      pure (Right (wrap (toolResultValue result)))

-- | Check the fields every request must carry, and the version.
checkMeta :: Object -> Either ErrorObject ()
checkMeta params = case KeyMap.lookup "_meta" params of
  Just (Object meta) -> case (KeyMap.lookup "io.modelcontextprotocol/protocolVersion" meta, KeyMap.lookup "io.modelcontextprotocol/clientCapabilities" meta) of
    (Just (String requested), Just (Object _))
      | requested == protocolVersion -> Right ()
      | otherwise -> Left (unsupportedProtocolVersion [protocolVersion] requested)
    (Just (String _), _) -> Left (invalidParams "Missing _meta.io.modelcontextprotocol/clientCapabilities")
    _ -> Left (invalidParams "Missing _meta.io.modelcontextprotocol/protocolVersion")
  _ -> Left (invalidParams "Missing _meta: in revision 2026-07-28 every request carries its protocol version; the earlier revisions start with initialize")

complete :: ServerConfig es -> [(Aeson.Key, Value)] -> Value
complete config fields =
  object $
    [ "resultType" .= ("complete" :: Text)
    , "_meta" .= object ["io.modelcontextprotocol/serverInfo" .= config.info]
    ]
      <> fields

cacheable :: ServerConfig es -> [(Aeson.Key, Value)]
cacheable config = ["ttlMs" .= config.cacheTtlMs, "cacheScope" .= ("public" :: Text)]

stringField :: Aeson.Key -> Object -> Text
stringField key fields = case KeyMap.lookup key fields of
  Just (String text) -> text
  _ -> ""

