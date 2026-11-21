-- | The server, fed and read through in-memory queues.
module Main (main) where

import Control.Concurrent.STM
import Data.Aeson (FromJSON (..), Value (..), object, withObject, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Concurrent.Async (wait, withAsync)
import Effectful.Exception (finally)
import Effectful.MCP.Server
import Effectful.MCP.Tool
import Test.Tasty (TestTree, defaultMain, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  localOption (mkTimeout 10_000_000) $
    testGroup
      "effectful-mcp"
      [ testGroup
          "methods"
          [ testCase "server/discover" $ do
              result <- expectResult =<< exchange (request 1 "server/discover" [])
              field "resultType" result @?= Just (String "complete")
              field "supportedVersions" result @?= Just (Aeson.toJSON ["2026-07-28" :: Text])
              field "capabilities" result @?= Just (object ["tools" .= object []])
              field "ttlMs" result @?= Just (Number 300000)
              field "cacheScope" result @?= Just (String "public")
              (field "_meta" result >>= field "io.modelcontextprotocol/serverInfo") @?= Just (object ["name" .= ("test-server" :: Text), "version" .= ("1.0" :: Text)])
          , testCase "tools/list gives the tools in order" $ do
              result <- expectResult =<< exchange (request 1 "tools/list" [])
              case field "tools" result of
                Just (Array tools) -> do
                  map (field "name") (foldr (:) [] tools) @?= map (Just . String) ["echo", "add", "fail", "wait"]
                  (field "annotations" =<< headMaybe tools) @?= Just (object ["readOnlyHint" .= True])
                other -> assertFailure ("no tools: " <> show other)
              field "ttlMs" result @?= Just (Number 300000)
          , testCase "tools/call returns text" $ do
              result <- expectResult =<< exchange (call 1 "echo" (object ["text" .= ("hello" :: Text)]))
              field "content" result @?= Just (Aeson.toJSON [object ["type" .= ("text" :: Text), "text" .= ("hello" :: Text)]])
              field "isError" result @?= Nothing
          , testCase "tools/call returns structured content, also as text" $ do
              result <- expectResult =<< exchange (call 1 "add" (object ["a" .= (2 :: Int), "b" .= (3 :: Int)]))
              field "structuredContent" result @?= Just (object ["sum" .= (5 :: Int)])
              field "content" result @?= Just (Aeson.toJSON [object ["type" .= ("text" :: Text), "text" .= ("{\"sum\":5}" :: Text)]])
          , testCase "arguments that do not decode give an error result" $ do
              result <- expectResult =<< exchange (call 1 "add" (object ["a" .= ("two" :: Text)]))
              field "isError" result @?= Just (Bool True)
          , testCase "a tool that throws gives an error result" $ do
              result <- expectResult =<< exchange (call 1 "fail" (object []))
              field "isError" result @?= Just (Bool True)
          , testCase "an image result carries its media type and its bytes in base64" $
              lookup "content" (toolResultValue (imageResult "image/png" "\137PNG"))
                @?= Just (Aeson.toJSON [object ["type" .= ("image" :: Text), "data" .= ("iVBORw==" :: Text), "mimeType" .= ("image/png" :: Text)]])
          ]
      , testGroup
          "protocol errors"
          [ testCase "unknown tool" $ exchange (call 1 "nope" (object [])) >>= expectError (-32602)
          , testCase "missing _meta" $
              exchange (line (object ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int), "method" .= ("tools/list" :: Text)])) >>= expectError (-32602)
          , testCase "unsupported version" $ do
              response <- exchange (requestIn "2025-11-25" 1 "tools/list" [])
              expectError (-32022) response
              (field "error" response >>= field "data") @?= Just (object ["supported" .= ["2026-07-28" :: Text], "requested" .= ("2025-11-25" :: Text)])
          , testCase "a request of an earlier revision before any initialize" $
              exchange (legacy 1 "tools/call" ["name" .= ("echo" :: Text), "arguments" .= object ["text" .= ("hello" :: Text)]]) >>= expectError (-32602)
          , testCase "unknown method" $ exchange (request 1 "prompts/list" []) >>= expectError (-32601)
          , testCase "malformed JSON, without id" $ do
              response <- exchange "{not json"
              expectError (-32700) response
              field "id" response @?= Nothing
          , testCase "batch" $ exchange "[]" >>= expectError (-32600)
          ]
      , testGroup
          "earlier revisions"
          [ testCase "initialize answers the version asked, the capabilities and the server" $ do
              result <- expectResult =<< exchange (initialize 1 "2025-06-18")
              result
                @?= object
                  [ "protocolVersion" .= ("2025-06-18" :: Text)
                  , "capabilities" .= object ["tools" .= object ["listChanged" .= False]]
                  , "serverInfo" .= object ["name" .= ("test-server" :: Text), "version" .= ("1.0" :: Text)]
                  ]
          , testCase "initialize answers its latest earlier revision to a version it does not know" $ do
              answers <- mapM (\asked -> (field "protocolVersion" =<<) . field "result" <$> exchange (initialize 1 asked)) ["2025-11-25", "2024-11-05", "1900-01-01", "2026-07-28"]
              answers @?= map (Just . String) ["2025-11-25", "2024-11-05", "2025-11-25", "2025-11-25"]
          , testCase "after initialize, the tools are listed and called as those revisions do" $ do
              responses <- session $ \push pull -> do
                push (initialize 1 "2025-11-25")
                _ <- pull
                push (line (object ["jsonrpc" .= ("2.0" :: Text), "method" .= ("notifications/initialized" :: Text)]))
                push (legacy 2 "tools/list" [])
                listed <- pull
                push (legacy 3 "tools/call" ["name" .= ("add" :: Text), "arguments" .= object ["a" .= (2 :: Int), "b" .= (3 :: Int)]])
                called <- pull
                push (legacy 4 "ping" [])
                pinged <- pull
                pure [listed, called, pinged]
              case responses of
                [listed, called, pinged] -> do
                  result <- expectResult listed
                  case result of
                    Object fields -> do
                      KeyMap.keys fields @?= ["tools"]
                      fmap (\case Array tools -> map (field "name") (foldr (:) [] tools); _ -> []) (field "tools" result)
                        @?= Just (map (Just . String) ["echo", "add", "fail", "wait"])
                    other -> assertFailure ("not an object: " <> show other)
                  sum' <- expectResult called
                  sum'
                    @?= object
                      [ "content" .= [object ["type" .= ("text" :: Text), "text" .= ("{\"sum\":5}" :: Text)]]
                      , "structuredContent" .= object ["sum" .= (5 :: Int)]
                      ]
                  field "result" pinged @?= Just (object [])
                other -> assertFailure ("expected three answers, got " <> show other)
          , testCase "after initialize, errors are those of the protocol" $ do
              responses <- session $ \push pull -> do
                push (initialize 1 "2025-11-25")
                _ <- pull
                push (legacy 2 "tools/call" ["name" .= ("nope" :: Text)])
                unknownTool <- pull
                push (legacy 3 "prompts/list" [])
                unknownMethod <- pull
                push (legacy 4 "tools/list" ["cursor" .= ("next" :: Text)])
                cursor <- pull
                pure [unknownTool, unknownMethod, cursor]
              map (\response -> field "error" response >>= field "code") responses @?= map (Just . Number) [-32602, -32601, -32602]
          , testCase "both eras are served on one connection" $ do
              responses <- session $ \push pull -> do
                push (initialize 1 "2025-11-25")
                _ <- pull
                push (request 2 "tools/list" [])
                modern <- pull
                push (legacy 3 "tools/list" [])
                earlier <- pull
                pure [modern, earlier]
              map (\response -> field "result" response >>= field "resultType") responses @?= [Just (String "complete"), Nothing]
          ]
      , testGroup
          "serving"
          [ testCase "a notification gets no answer" $ do
              responses <- session $ \push pull -> do
                push (line (object ["jsonrpc" .= ("2.0" :: Text), "method" .= ("notifications/initialized" :: Text)]))
                push (request 2 "tools/list" [])
                (: []) <$> pull
              map (field "id") responses @?= [Just (Number 2)]
          , testCase "a slow call does not hold up the others" $ do
              gate <- newEmptyTMVarIO
              responses <- sessionWith gate $ \push pull -> do
                push (call 1 "wait" (object []))
                push (call 2 "echo" (object ["text" .= ("fast" :: Text)]))
                first <- pull
                atomically (putTMVar gate ())
                second <- pull
                pure [first, second]
              map (field "id") responses @?= [Just (Number 2), Just (Number 1)]
          , testCase "a cancelled call stops and gets no answer" $ do
              gate <- newEmptyTMVarIO
              responses <- sessionWith gate $ \push pull -> do
                push (call 7 "wait" (object []))
                push (line (object ["jsonrpc" .= ("2.0" :: Text), "method" .= ("notifications/cancelled" :: Text), "params" .= object ["requestId" .= (7 :: Int)]]))
                push (call 8 "echo" (object ["text" .= ("after" :: Text)]))
                (: []) <$> pull
              map (field "id") responses @?= [Just (Number 8)]
          , testCase "calls still running when the input ends may finish" $ do
              gate <- newEmptyTMVarIO
              output <- newTQueueIO
              input <- newTQueueIO
              let config = (testConfig gate) {shutdownGrace = 5_000_000}
              runEff . runConcurrent $
                withAsync (serve config (liftIO (atomically (readTQueue input))) (liftIO . atomically . writeTQueue output)) $ \server -> do
                  liftIO . atomically $ do
                    writeTQueue input (Just (call 1 "wait" (object [])))
                    writeTQueue input Nothing
                  liftIO (atomically (putTMVar gate ()))
                  wait server
              responses <- atomically (flushTQueue output)
              map (field "id" . decode) responses @?= [Just (Number 1)]
          , testCase "calls still running after the grace period are stopped" $ do
              gate <- newEmptyTMVarIO
              output <- newTQueueIO
              input <- newTQueueIO
              stopped <- newTVarIO False
              let config = (testConfigStopping gate stopped) {shutdownGrace = 50_000}
              runEff . runConcurrent $ do
                liftIO . atomically $ do
                  writeTQueue input (Just (call 1 "wait" (object [])))
                  writeTQueue input Nothing
                serve config (liftIO (atomically (readTQueue input))) (liftIO . atomically . writeTQueue output)
              atomically (flushTQueue output) >>= (@?= [])
              readTVarIO stopped >>= (@?= True)
          ]
      ]
  where
    headMaybe = foldr (\x _ -> Just x) Nothing

-- Tools

data AddArguments = AddArguments Int Int

instance FromJSON AddArguments where
  parseJSON = withObject "AddArguments" $ \fields -> AddArguments <$> fields .: "a" <*> fields .: "b"

newtype EchoArguments = EchoArguments Text

instance FromJSON EchoArguments where
  parseJSON = withObject "EchoArguments" $ \fields -> EchoArguments <$> fields .: "text"

testConfig :: TMVar () -> ServerConfig '[Concurrent, IOE]
testConfig gate = testConfigWith gate (pure ())

-- | 'testConfig', where the waiting tool records that it was stopped.
testConfigStopping :: TMVar () -> TVar Bool -> ServerConfig '[Concurrent, IOE]
testConfigStopping gate stopped = testConfigWith gate (atomically (writeTVar stopped True))

testConfigWith :: TMVar () -> IO () -> ServerConfig '[Concurrent, IOE]
testConfigWith gate onStop =
  serverConfig
    Implementation {name = "test-server", version = "1.0", title = Nothing}
    [ (tool "echo" "Echo the text" schema (withArguments (\(EchoArguments text) -> pure (textResult text))))
        { annotations = noAnnotations {readOnlyHint = Just True}
        }
    , tool "add" "Add two numbers" schema (withArguments (\(AddArguments a b) -> pure (structuredResult (object ["sum" .= (a + b)]))))
    , tool "fail" "Throw" schema (\_ -> error "boom")
    , tool "wait" "Wait for the gate" schema $ \_ ->
        (liftIO (atomically (readTMVar gate)) >> pure (textResult "done")) `finally` liftIO onStop'
    ]
  where
    schema = object ["type" .= ("object" :: Text)]
    -- Only a call that did not finish counts as stopped.
    onStop' = atomically (tryReadTMVar gate) >>= maybe onStop (const (pure ()))

-- Driving the server

-- | Send one message, and read the one answer.
exchange :: ByteString -> IO Value
exchange message =
  session (\push pull -> push message >> (: []) <$> pull) >>= \case
    [response] -> pure response
    other -> assertFailure ("expected one answer, got " <> show other)

session :: ((ByteString -> IO ()) -> IO Value -> IO [Value]) -> IO [Value]
session script = newEmptyTMVarIO >>= \gate -> sessionWith gate script

-- | Run the server while a script talks to it, then end the input.
sessionWith :: TMVar () -> ((ByteString -> IO ()) -> IO Value -> IO [Value]) -> IO [Value]
sessionWith gate script = do
  input <- newTQueueIO
  output <- newTQueueIO
  runEff . runConcurrent $
    withAsync (serve (testConfig gate) (liftIO (atomically (readTQueue input))) (liftIO . atomically . writeTQueue output)) $ \server -> do
      responses <- liftIO (script (atomically . writeTQueue input . Just) (decode <$> atomically (readTQueue output)))
      liftIO (atomically (writeTQueue input Nothing))
      wait server
      pure responses

decode :: ByteString -> Value
decode bytes = maybe (error ("not JSON: " <> show bytes)) id (Aeson.decodeStrict bytes)

line :: Value -> ByteString
line = LazyByteString.toStrict . Aeson.encode

meta :: Text -> Value
meta version =
  object
    [ "io.modelcontextprotocol/protocolVersion" .= version
    , "io.modelcontextprotocol/clientCapabilities" .= object []
    , "io.modelcontextprotocol/clientInfo" .= object ["name" .= ("test" :: Text), "version" .= ("0" :: Text)]
    ]

request :: Int -> Text -> [(Aeson.Key, Value)] -> ByteString
request = requestIn "2026-07-28"

-- | A request as the earlier revisions write it: without the version.
legacy :: Int -> Text -> [(Aeson.Key, Value)] -> ByteString
legacy requestId method params =
  line (object ["jsonrpc" .= ("2.0" :: Text), "id" .= requestId, "method" .= method, "params" .= object params])

-- | The handshake of the earlier revisions, asking for a version.
initialize :: Int -> Text -> ByteString
initialize requestId asked =
  legacy requestId "initialize" ["protocolVersion" .= asked, "capabilities" .= object [], "clientInfo" .= object ["name" .= ("test-client" :: Text), "version" .= ("0" :: Text)]]

requestIn :: Text -> Int -> Text -> [(Aeson.Key, Value)] -> ByteString
requestIn version requestId method params =
  line (object ["jsonrpc" .= ("2.0" :: Text), "id" .= requestId, "method" .= method, "params" .= object (("_meta" .= meta version) : params)])

call :: Int -> Text -> Value -> ByteString
call requestId name arguments = request requestId "tools/call" ["name" .= name, "arguments" .= arguments]

field :: Aeson.Key -> Value -> Maybe Value
field key = \case
  Object fields -> KeyMap.lookup key fields
  _ -> Nothing

expectResult :: Value -> IO Value
expectResult response = case field "result" response of
  Just result -> pure result
  Nothing -> assertFailure ("not a result: " <> show response)

expectError :: Int -> Value -> Assertion
expectError expected response = (field "error" response >>= field "code") @?= Just (Number (fromIntegral expected))
