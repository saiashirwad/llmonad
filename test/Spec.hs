{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy.Char8 qualified as LB
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLMonad
import System.Exit (exitFailure, exitSuccess)

-- ============================================================================
-- Test Types
-- ============================================================================

data Color = Red | Green | Blue
  deriving (Show, Eq, Generic, FromJSON, ToJSON, HasSchema)

data Product = Product
  { title :: Text,
    price :: Double,
    inStock :: Bool,
    tags :: [Text]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, HasSchema)

data AddArgs = AddArgs
  { x :: Int,
    y :: Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, HasSchema)

data AddRes = AddRes
  { sum :: Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, HasSchema)

-- ============================================================================
-- Assertions
-- ============================================================================

assert :: String -> Bool -> IO ()
assert name True = putStrLn $ "  [PASS] " <> name
assert name False = do
  putStrLn $ "  [FAIL] " <> name
  exitFailure

-- ============================================================================
-- Main Test Suite
-- ============================================================================

main :: IO ()
main = do
  putStrLn "========================================"
  putStrLn "   Running LLMonad Test Suite          "
  putStrLn "========================================"

  putStrLn "\n--- 1. Schema Derivation Tests ---"
  testSchemas

  putStrLn "\n--- 2. Message & Request Serialization Tests ---"
  testSerialization

  putStrLn "\n--- 3. Prompt & Template Combinator Tests ---"
  testPromptCombinators

  putStrLn "\n--- 4. Tool Definition & Execution Tests ---"
  testTools

  putStrLn "\n--- 5. Configuration & State Tests ---"
  testConfigAndState

  putStrLn "\n========================================"
  putStrLn "   All LLMonad tests passed!           "
  putStrLn "========================================"
  exitSuccess

-- ============================================================================
-- Test Implementations
-- ============================================================================

testSchemas :: IO ()
testSchemas = do
  -- Primitive schemas
  assert "Text schema is String" (schema @Text == SchemaString)
  assert "Int schema is Integer" (schema @Int == SchemaInteger)
  assert "Double schema is Number" (schema @Double == SchemaNumber)
  assert "Bool schema is Boolean" (schema @Bool == SchemaBoolean)
  assert "List schema is Array" (schema @[Text] == SchemaArray SchemaString)

  -- Enum sum types
  let colorSchema = schema @Color
  assert "Enum derivation creates SchemaEnum" $
    case colorSchema of
      SchemaEnum vals -> vals == ["Red", "Green", "Blue"]
      _ -> False

  -- Record product types
  let prodSchema = schema @Product
  assert "Product schema derivation creates SchemaObject with 4 fields" $
    case prodSchema of
      SchemaObject fields ->
        length fields == 4
          && map (\(n, _, _) -> n) fields == ["title", "price", "inStock", "tags"]
      _ -> False

  -- Schema serialization to JSON
  let jsonVal = schemaToValue (schema @Product)
      jsonBs = encode jsonVal
  assert "Schema converts to non-empty JSON" (LB.length jsonBs > 20)

testSerialization :: IO ()
testSerialization = do
  let uMsg = user "Hello world"
  assert "userMsg has UserRole" (messageRole uMsg == UserRole)
  assert "userMsg content matches" (messageContent uMsg == "Hello world")

  let sMsg = system "You are an assistant"
  assert "systemMsg has SystemRole" (messageRole sMsg == SystemRole)

  let tCall =
        ToolCall
          { toolCallId = "call_123",
            toolCallType = "function",
            toolCallFunction = FunctionCall "calculator" "{\"x\": 5}"
          }
      aMsg = Message AssistantRole "Using tool" Nothing Nothing (Just [tCall])
      encoded = encode aMsg
  assert "Message with ToolCall encodes and decodes" $
    case decode encoded of
      Just (m :: Message) ->
        messageContent m == "Using tool"
          && case messageToolCalls m of
            Just [tc] -> toolCallId tc == "call_123" && funcName (toolCallFunction tc) == "calculator"
            _ -> False
      Nothing -> False

testPromptCombinators :: IO ()
testPromptCombinators = do
  -- Template rendering
  let tpl = "Hello, {{user}}! Welcome to {{place}}."
      res = renderTemplate tpl [("user", "Alice"), ("place", "Haskell")]
  assert "renderTemplate replaces placeholders" (res == "Hello, Alice! Welcome to Haskell.")

  -- Prompt Monoid
  let p1 = user "Hello"
      p2 = assistant "Hi"
      pCombined = toPrompt [p1] <> toPrompt [p2]
  assert "Prompt Monoid combines message sequences" (length (unPrompt pCombined) == 2)

  -- Few-shot generation
  let examples = [("2 + 2", "4"), ("3 + 3", "6")]
      msgs = fewShot examples "4 + 4"
  assert "fewShot generates correct alternating messages" $
    length msgs == 5
      && messageRole (msgs !! 0) == UserRole
      && messageRole (msgs !! 1) == AssistantRole
      && messageContent (msgs !! 4) == "4 + 4"

testTools :: IO ()
testTools = do
  let addTool =
        defToolSync
          "add"
          "Adds two integers"
          (\(AddArgs a b) -> AddRes (a + b))

  assert "Tool parameters schema matches argument type" (toolParameters addTool == schema @AddArgs)

  -- Valid execution
  execRes <- toolExecute addTool "{\"x\": 10, \"y\": 32}"
  assert "Tool executes with valid JSON and returns correct result" $
    case execRes of
      Right outJson -> "42" `T.isInfixOf` outJson
      Left _ -> False

  -- Invalid JSON handling
  errRes <- toolExecute addTool "invalid json {"
  assert "Tool gracefully returns Left on malformed input" $
    case errRes of
      Left err -> "Tool argument parsing error" `T.isPrefixOf` err
      Right _ -> False

testConfigAndState :: IO ()
testConfigAndState = do
  let cfg = defaultConfig (openai "test-key")
  assert "OpenAI default model is gpt-4o-mini" (configModel cfg == "gpt-4o-mini")

  let modifiedCfg = withTemperature 0.2 (withModel "gpt-4o" cfg)
  assert "withModel overrides model" (configModel modifiedCfg == "gpt-4o")
  assert "withTemperature sets temperature" (configTemperature modifiedCfg == Just 0.2)

  let deepseekCfg = defaultConfig (deepseek "deepseek-key")
  assert "DeepSeek default model is deepseek-chat" (configModel deepseekCfg == "deepseek-chat")

  let anthropicCfg = defaultConfig (anthropic "anthropic-key")
  assert "Anthropic default model is claude-3-5-sonnet-20241022" (configModel anthropicCfg == "claude-3-5-sonnet-20241022")

  let openRouterCfg = defaultConfig (openrouter "openrouter-key")
  assert "OpenRouter default model is deepseek/deepseek-chat" (configModel openRouterCfg == "deepseek/deepseek-chat")

  let customOpenAICfg = defaultConfig (openaiCompatible "http://localhost:8000/v1" Nothing)
  assert "Custom OpenAI compatible provider sets base URL" (providerBaseUrl (configProvider customOpenAICfg) == "http://localhost:8000/v1")

  let customAnthropicCfg = defaultConfig (anthropicCompatible "https://custom.proxy" "secret")
  assert "Custom Anthropic compatible provider uses Anthropic protocol" (isAnthropicProtocol (configProvider customAnthropicCfg))

  -- Provider combinator overrides
  let customized = withHeader "X-Custom" "Value" (withBaseUrl "https://proxy.internal" (deepseek "key"))
  assert "withBaseUrl combinator overrides provider base URL" (providerBaseUrl customized == "https://proxy.internal")
  assert "withHeader combinator adds custom headers" (lookup "X-Custom" (providerAuthHeaders customized) == Just "Value")
