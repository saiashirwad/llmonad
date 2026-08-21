{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import LLMonad
import System.Environment (lookupEnv)

-- ============================================================================
-- Domain Types for Structured Output Extraction
-- ============================================================================

data Sentiment = Positive | Neutral | Negative
  deriving (Show, Eq, Generic, FromJSON, ToJSON, HasSchema)

data CodeReview = CodeReview
  { summary :: Text,
    sentiment :: Sentiment,
    issuesFound :: [Text],
    qualityScore :: Int -- 1 to 10
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, HasSchema)

data UserProfile = UserProfile
  { name :: Text,
    age :: Maybe Int,
    skills :: [Text],
    location :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, HasSchema)

-- ============================================================================
-- Tool Definitions for Autonomous ReAct Agent
-- ============================================================================

data CalcArgs = CalcArgs
  { operation :: Text, -- "add", "sub", "mul", "div"
    a :: Double,
    b :: Double
  }
  deriving (Show, Generic, FromJSON, ToJSON, HasSchema)

data CalcResult = CalcResult
  { calculationResult :: Double
  }
  deriving (Show, Generic, FromJSON, ToJSON, HasSchema)

calculatorTool :: Tool
calculatorTool =
  defToolSync
    "calculator"
    "Perform basic arithmetic calculations (operation: add, sub, mul, div; a: number; b: number)"
    ( \(CalcArgs op numA numB) ->
        case op of
          "add" -> CalcResult (numA + numB)
          "sub" -> CalcResult (numA - numB)
          "mul" -> CalcResult (numA * numB)
          "div" -> CalcResult (if numB /= 0 then numA / numB else 0.0)
          _ -> CalcResult 0.0
    )

data WeatherArgs = WeatherArgs
  { city :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, HasSchema)

data WeatherResult = WeatherResult
  { cityReport :: Text,
    temperatureCelsius :: Double,
    condition :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, HasSchema)

weatherTool :: Tool
weatherTool =
  defToolSync
    "get_weather"
    "Get the current weather forecast for a given city name"
    ( \(WeatherArgs cityName) ->
        WeatherResult
          { cityReport = cityName,
            temperatureCelsius = 22.5,
            condition = "Sunny and clear"
          }
    )

-- ============================================================================
-- Main Program
-- ============================================================================

main :: IO ()
main = do
  putStrLn "========================================================"
  putStrLn "   LLMonad: Type-Safe Haskell DSL for Language Models   "
  putStrLn "========================================================"

  -- Detect available API keys: DeepSeek, OpenAI, Anthropic, OpenRouter, or fallback to Ollama
  deepseekKey <- lookupEnv "DEEPSEEK_API_KEY"
  openaiKey <- lookupEnv "OPENAI_API_KEY"
  anthropicKey <- lookupEnv "ANTHROPIC_API_KEY"
  openrouterKey <- lookupEnv "OPENROUTER_API_KEY"

  let (providerDesc, config) = case (deepseekKey, openaiKey, anthropicKey, openrouterKey) of
        (Just k, _, _, _) ->
          ("DeepSeek (deepseek-chat)", defaultConfig (deepseek (T.pack k)))
        (_, Just k, _, _) ->
          ("OpenAI (gpt-4o-mini)", defaultConfig (openai (T.pack k)))
        (_, _, Just k, _) ->
          ("Anthropic (claude-3-5-sonnet)", defaultConfig (anthropic (T.pack k)))
        (_, _, _, Just k) ->
          ("OpenRouter (deepseek/deepseek-chat)", defaultConfig (openrouter (T.pack k)))
        (Nothing, Nothing, Nothing, Nothing) ->
          ("Ollama (Local at localhost:11434)", defaultConfig ollama)

  putStrLn $ "Active Provider: " <> providerDesc
  putStrLn ""

  let hasKey =
        isJust deepseekKey
          || isJust openaiKey
          || isJust anthropicKey
          || isJust openrouterKey

  if not hasKey
    then do
      putStrLn "Note: No DEEPSEEK_API_KEY, OPENAI_API_KEY, ANTHROPIC_API_KEY, or OPENROUTER_API_KEY set."
      putStrLn "Ensure Ollama is running at http://localhost:11434 or set an API key."
      putStrLn ""
    else pure ()

  -- --------------------------------------------------------------------------
  -- 1. Conversational Chat & Prompt DSL
  -- --------------------------------------------------------------------------
  putStrLn "[1] Demonstrating Monadic Chat DSL..."
  let chatProgram :: LLM ()
      chatProgram = do
        ans1 <- ask "In one short sentence, what is Haskell?"
        liftIO $ TIO.putStrLn $ "Response: " <> ans1
        ans2 <- ask "What is its biggest strength?"
        liftIO $ TIO.putStrLn $ "Follow-up: " <> ans2

  chatRes <- runLLM config chatProgram
  case chatRes of
    Left err -> putStrLn $ "Execution note (check network/API key): " <> show err
    Right () -> putStrLn "Chat completed successfully."
  putStrLn ""

  -- --------------------------------------------------------------------------
  -- 2. Type-Driven Structured Output Extraction
  -- --------------------------------------------------------------------------
  putStrLn "[2] Demonstrating Structured Output Extraction..."
  let extractionProgram :: LLM ()
      extractionProgram = do
        let profilePrompt =
              "Extract information: Alice is a 29-year-old senior Haskell engineer in Berlin specializing in compilers and distributed systems."
        profile :: UserProfile <- askStructured profilePrompt
        liftIO $ do
          putStrLn "Extracted Haskell UserProfile record:"
          print profile

        let codeSnippet =
              "Review this code: 'def div(a, b): return a / b' - lacks zero division check."
        review :: CodeReview <- askStructured codeSnippet
        liftIO $ do
          putStrLn "Extracted CodeReview record:"
          print review

  structRes <- runLLM (withTemperature 0.0 config) extractionProgram
  case structRes of
    Left err -> putStrLn $ "Extraction note: " <> show err
    Right () -> putStrLn "Structured extraction succeeded."
  putStrLn ""

  -- --------------------------------------------------------------------------
  -- 3. Autonomous ReAct Agent with Tool Execution Loop
  -- --------------------------------------------------------------------------
  putStrLn "[3] Demonstrating ReAct Agent with Tools..."
  let agentProgram :: LLM ()
      agentProgram = do
        let tools = [calculatorTool, weatherTool]
            agentPrompt =
              "What is the weather in Tokyo, and what is 42 multiplied by 137?"
        agentAns <- runAgentWith tools agentPrompt
        liftIO $ TIO.putStrLn $ "Agent Final Answer:\n" <> agentAns

  agentRes <- runLLM config agentProgram
  case agentRes of
    Left err -> putStrLn $ "Agent note: " <> show err
    Right () -> putStrLn "Agent execution finished."
  putStrLn ""

  putStrLn "========================================================"
  putStrLn "   LLMonad demonstration completed successfully!        "
  putStrLn "========================================================"
