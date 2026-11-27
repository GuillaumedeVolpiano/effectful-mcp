-- |
-- Module      : Effectful.MCP.Tool
-- Description : Tools that an MCP server offers, and what they return
-- License     : BSD-3-Clause
module Effectful.MCP.Tool
  ( -- * Tools
    Tool (..)
  , tool
  , ToolAnnotations (..)
  , noAnnotations
  , withArguments

    -- * Results
  , ToolResult (..)
  , Content (..)
  , textResult
  , imageResult
  , structuredResult
  , errorResult

    -- * Encoding
  , toolDefinition
  , toolResultValue
  ) where

import Data.Aeson (FromJSON, ToJSON (..), Value (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseEither, parseJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Effectful

-- | A tool: what the client lists, and what runs when it is called.
data Tool es = Tool
  { name :: Text
  -- ^ Letters, digits, @_@, @-@ and @.@, at most 128 of them.
  , title :: Maybe Text
  -- ^ For display.
  , description :: Text
  , inputSchema :: Value
  -- ^ A JSON Schema (2020-12 unless it says otherwise) of the arguments,
  -- with @"type": "object"@.
  , outputSchema :: Maybe Value
  -- ^ A JSON Schema of the structured content, if the tool returns some.
  , annotations :: ToolAnnotations
  , call :: Value -> Eff es ToolResult
  -- ^ Receives the arguments, an object.
  }

-- | A tool with no title, no output schema and no annotation.
tool :: Text -> Text -> Value -> (Value -> Eff es ToolResult) -> Tool es
tool name description inputSchema call =
  Tool {name, title = Nothing, description, inputSchema, outputSchema = Nothing, annotations = noAnnotations, call}

-- | Hints about what a tool does. Clients treat them as untrusted unless
-- they trust the server.
data ToolAnnotations = ToolAnnotations
  { readOnlyHint :: Maybe Bool
  -- ^ The tool changes nothing.
  , destructiveHint :: Maybe Bool
  -- ^ The tool may destroy something, rather than only add.
  , idempotentHint :: Maybe Bool
  -- ^ Calling it again with the same arguments has no further effect.
  , openWorldHint :: Maybe Bool
  -- ^ The tool reaches outside entities.
  }
  deriving stock (Eq, Show)

-- | No hint at all.
noAnnotations :: ToolAnnotations
noAnnotations = ToolAnnotations Nothing Nothing Nothing Nothing

-- | Decode the arguments before running the tool. Arguments that do not
-- decode give an error result that tells the model what is wrong, so that
-- it can call again.
withArguments :: FromJSON a => (a -> Eff es ToolResult) -> Value -> Eff es ToolResult
withArguments run arguments = case parseEither parseJSON arguments of
  Right decoded -> run decoded
  Left err -> pure (errorResult ("Invalid arguments: " <> Text.pack err))

-- | What a call returns.
data ToolResult = ToolResult
  { content :: [Content]
  , structuredContent :: Maybe Value
  , isError :: Bool
  -- ^ The tool failed; the content says why, for the model to correct.
  }
  deriving stock (Eq, Show)

-- | Content of a result.
data Content
  = TextContent Text
  | -- | An image: its media type, such as @image\/jpeg@, and its bytes.
    -- They are sent in base64.
    ImageContent Text ByteString
  deriving stock (Eq, Show)

-- | A text.
textResult :: Text -> ToolResult
textResult text = ToolResult {content = [TextContent text], structuredContent = Nothing, isError = False}

-- | An image, from its media type and its bytes.
imageResult :: Text -> ByteString -> ToolResult
imageResult mimeType bytes = ToolResult {content = [ImageContent mimeType bytes], structuredContent = Nothing, isError = False}

-- | A structured result, also given as JSON text for clients that only
-- read text, as the specification recommends.
structuredResult :: ToJSON a => a -> ToolResult
structuredResult value =
  ToolResult
    { content = [TextContent (decodeUtf8 (LazyByteString.toStrict (Aeson.encode value)))]
    , structuredContent = Just (toJSON value)
    , isError = False
    }

-- | A failure the model can act on.
errorResult :: Text -> ToolResult
errorResult text = ToolResult {content = [TextContent text], structuredContent = Nothing, isError = True}

-- | The definition of a tool, as @tools/list@ gives it.
toolDefinition :: Tool es -> Value
toolDefinition Tool {name, title, description, inputSchema, outputSchema, annotations} =
  object $
    ["name" .= name, "description" .= description, "inputSchema" .= inputSchema]
      <> catMaybes
        [ ("title" .=) <$> title
        , ("outputSchema" .=) <$> outputSchema
        , ("annotations" .=) <$> annotationsValue annotations
        ]

annotationsValue :: ToolAnnotations -> Maybe Value
annotationsValue ToolAnnotations {readOnlyHint, destructiveHint, idempotentHint, openWorldHint} =
  case catMaybes
    [ ("readOnlyHint" .=) <$> readOnlyHint
    , ("destructiveHint" .=) <$> destructiveHint
    , ("idempotentHint" .=) <$> idempotentHint
    , ("openWorldHint" .=) <$> openWorldHint
    ] of
    [] -> Nothing
    hints -> Just (object hints)

-- | The fields of a @tools/call@ result, without @resultType@ and @_meta@.
toolResultValue :: ToolResult -> [(Aeson.Key, Value)]
toolResultValue ToolResult {content, structuredContent, isError} =
  ["content" .= map contentValue content]
    <> catMaybes [("structuredContent" .=) <$> structuredContent]
    <> ["isError" .= True | isError]
  where
    contentValue = \case
      TextContent text -> object ["type" .= ("text" :: Text), "text" .= text]
      ImageContent mimeType bytes ->
        object ["type" .= ("image" :: Text), "data" .= decodeUtf8 (Base64.encode bytes), "mimeType" .= mimeType]
