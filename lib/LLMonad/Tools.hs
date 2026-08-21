{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LLMonad.Tools
  ( -- * Tool Definition
    Tool (..),
    defTool,
    defToolSync,

    -- * Tool Formatting
    toolToOpenAITool,
    toolToAnthropicTool,

    -- * Tool Execution
    executeToolCall,
  )
where

import Control.Exception (SomeException, catch)
import Data.Aeson (FromJSON, ToJSON, Value, eitherDecodeStrict, encode, object, (.=))
import Data.ByteString.Lazy qualified as LB
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import LLMonad.Schema (HasSchema (..), JSONSchema, schemaToValue)
import LLMonad.Types (FunctionCall (..), ToolCall (..))

-- | Definition of an executable tool/function callable by an LLM
data Tool = Tool
  { toolName :: Text,
    toolDescription :: Text,
    toolParameters :: JSONSchema,
    toolExecute :: Text -> IO (Either Text Text)
  }

instance Show Tool where
  show Tool {..} =
    "Tool { toolName = "
      <> show toolName
      <> ", toolDescription = "
      <> show toolDescription
      <> " }"

-- | Define an asynchronous/IO tool with automatic JSON parsing and encoding
defTool ::
  forall args ret.
  (FromJSON args, ToJSON ret, HasSchema args) =>
  Text ->
  Text ->
  (args -> IO ret) ->
  Tool
defTool name desc handler =
  Tool
    { toolName = name,
      toolDescription = desc,
      toolParameters = schema @args,
      toolExecute = \jsonArgsText -> do
        case eitherDecodeStrict (encodeUtf8 jsonArgsText) of
          Left err ->
            pure $
              Left $
                "Tool argument parsing error: "
                  <> T.pack err
                  <> ". Raw input: "
                  <> jsonArgsText
          Right (args :: args) -> do
            resultOrEx <-
              (Right <$> handler args)
                `catch` (\(e :: SomeException) -> pure (Left (T.pack (show e))))
            case resultOrEx of
              Left err -> pure $ Left ("Tool execution failed: " <> err)
              Right res ->
                pure $
                  Right $
                    decodeUtf8 $
                      LB.toStrict $
                        encode res
    }

-- | Define a pure synchronous tool
defToolSync ::
  forall args ret.
  (FromJSON args, ToJSON ret, HasSchema args) =>
  Text ->
  Text ->
  (args -> ret) ->
  Tool
defToolSync name desc pureHandler =
  defTool name desc (pure . pureHandler)

-- | Convert a Tool to the OpenAI function calling schema format
toolToOpenAITool :: Tool -> Value
toolToOpenAITool Tool {..} =
  object
    [ "type" .= ("function" :: Text),
      "function"
        .= object
          [ "name" .= toolName,
            "description" .= toolDescription,
            "parameters" .= schemaToValue toolParameters
          ]
    ]

-- | Convert a Tool to the Anthropic tool schema format
toolToAnthropicTool :: Tool -> Value
toolToAnthropicTool Tool {..} =
  object
    [ "name" .= toolName,
      "description" .= toolDescription,
      "input_schema" .= schemaToValue toolParameters
    ]

-- | Execute a tool call against a map of registered tools
executeToolCall :: Map Text Tool -> ToolCall -> IO (Either Text Text)
executeToolCall registry ToolCall {..} = do
  let fname = funcName toolCallFunction
      args = funcArguments toolCallFunction
  case Map.lookup fname registry of
    Nothing ->
      pure $
        Left $
          "Tool not found: '"
            <> fname
            <> "'. Available tools: "
            <> T.intercalate ", " (Map.keys registry)
    Just tool ->
      toolExecute tool args
