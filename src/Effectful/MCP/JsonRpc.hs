-- |
-- Module      : Effectful.MCP.JsonRpc
-- Description : JSON-RPC 2.0 messages, as an MCP server sees them
-- License     : BSD-3-Clause
--
-- MCP messages are JSON-RPC 2.0 messages, with two restrictions: a request
-- identifier is never @null@, and there is no batch.
module Effectful.MCP.JsonRpc
  ( -- * Incoming messages
    RequestId (..)
  , Incoming (..)
  , decodeIncoming

    -- * Outgoing messages
  , ErrorObject (..)
  , encodeResult
  , encodeError

    -- * Error codes
  , parseError
  , invalidRequest
  , methodNotFound
  , invalidParams
  , internalError
  , unsupportedProtocolVersion
  ) where

import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Maybe (catMaybes)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import Data.Text qualified as Text

-- | A request identifier: a string or an integer.
data RequestId = RequestIdText Text | RequestIdNumber Integer
  deriving stock (Eq, Ord, Show)

instance ToJSON RequestId where
  toJSON = \case
    RequestIdText text -> String text
    RequestIdNumber number -> Number (fromInteger number)

instance FromJSON RequestId where
  parseJSON = \case
    String text -> pure (RequestIdText text)
    Number number | Right integer <- floatingOrInteger @Double number -> pure (RequestIdNumber integer)
    _ -> fail "a request id is a string or an integer"

-- | A message read from the client.
data Incoming
  = -- | A request: its identifier, its method and its parameters.
    Request RequestId Text Object
  | -- | A notification: its method and its parameters.
    Notification Text Object
  | -- | A response, which a client must not send on stdio.
    Response
  | -- | A message that cannot be served, with the error to answer, and
    -- the identifier when it could be read.
    Invalid (Maybe RequestId) ErrorObject
  deriving stock (Eq, Show)

-- | Read one message.
decodeIncoming :: ByteString -> Incoming
decodeIncoming bytes = case Aeson.eitherDecodeStrict bytes of
  Left err -> Invalid Nothing (parseError ("Parse error: " <> Text.pack err))
  Right (Object fields) -> fromObject fields
  Right (Array _) -> Invalid Nothing (invalidRequest "Batches are not supported")
  Right _ -> Invalid Nothing (invalidRequest "A message is a JSON object")
  where
    fromObject fields
      | KeyMap.lookup "jsonrpc" fields /= Just (String "2.0") = Invalid identifier (invalidRequest "jsonrpc must be \"2.0\"")
      | otherwise = case (KeyMap.lookup "method" fields, KeyMap.lookup "id" fields) of
          (Just (String method), Nothing) -> withParams (Notification method)
          (Just (String method), Just _) -> case identifier of
            Just requestId -> withParams (Request requestId method)
            Nothing -> Invalid Nothing (invalidRequest "A request id is a string or an integer, never null")
          (Just _, _) -> Invalid identifier (invalidRequest "method must be a string")
          (Nothing, _)
            | KeyMap.member "result" fields || KeyMap.member "error" fields -> Response
            | otherwise -> Invalid identifier (invalidRequest "No method")
      where
        identifier = KeyMap.lookup "id" fields >>= parseMaybe parseJSON
        withParams build = case KeyMap.lookup "params" fields of
          Nothing -> build KeyMap.empty
          Just (Object params) -> build params
          Just _ -> Invalid identifier (invalidParams "params must be an object")

-- | A JSON-RPC error.
data ErrorObject = ErrorObject
  { code :: Int
  , message :: Text
  , errorData :: Maybe Value
  }
  deriving stock (Eq, Show)

instance ToJSON ErrorObject where
  toJSON ErrorObject {code, message, errorData} =
    object (["code" .= code, "message" .= message] <> catMaybes [("data" .=) <$> errorData])

-- | A result response, as one line without its end.
encodeResult :: RequestId -> Value -> ByteString
encodeResult requestId result =
  LazyByteString.toStrict (Aeson.encode (object ["jsonrpc" .= ("2.0" :: Text), "id" .= requestId, "result" .= result]))

-- | An error response, as one line without its end. The identifier is
-- left out when it could not be read.
encodeError :: Maybe RequestId -> ErrorObject -> ByteString
encodeError requestId err =
  LazyByteString.toStrict . Aeson.encode . object $
    ["jsonrpc" .= ("2.0" :: Text), "error" .= err] <> catMaybes [("id" .=) <$> requestId]

-- | The errors JSON-RPC defines, each with its code and a message: the
-- input is not JSON (-32700), is not a request (-32600), names a method
-- that does not exist (-32601), has parameters that do not fit (-32602),
-- or the server failed (-32603).
parseError, invalidRequest, methodNotFound, invalidParams, internalError :: Text -> ErrorObject
parseError message = ErrorObject {code = -32700, message, errorData = Nothing}
invalidRequest message = ErrorObject {code = -32600, message, errorData = Nothing}
methodNotFound message = ErrorObject {code = -32601, message, errorData = Nothing}
invalidParams message = ErrorObject {code = -32602, message, errorData = Nothing}
internalError message = ErrorObject {code = -32603, message, errorData = Nothing}

-- | The version a request asked for is not served: lists those that are.
unsupportedProtocolVersion :: [Text] -> Text -> ErrorObject
unsupportedProtocolVersion supported requested =
  ErrorObject
    { code = -32022
    , message = "Unsupported protocol version"
    , errorData = Just (object ["supported" .= supported, "requested" .= requested])
    }
