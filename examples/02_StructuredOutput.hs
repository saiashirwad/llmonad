{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLMonad
import System.Environment (lookupEnv)

data Priority = Low | Medium | High | Critical
  deriving (Show, Eq, Generic, FromJSON, ToJSON, HasSchema)

data BugReport = BugReport
  { title :: Text,
    component :: Text,
    priority :: Priority,
    stepsToReproduce :: [Text],
    suspectedCause :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, HasSchema)

main :: IO ()
main = do
  putStrLn "=== Example 02: Structured Output Extraction ==="

  deepseekKey <- lookupEnv "DEEPSEEK_API_KEY"
  openaiKey <- lookupEnv "OPENAI_API_KEY"
  anthropicKey <- lookupEnv "ANTHROPIC_API_KEY"

  let provider = case (deepseekKey, openaiKey, anthropicKey) of
        (Just k, _, _) -> deepseek (T.pack k)
        (_, Just k, _) -> openai (T.pack k)
        (_, _, Just k) -> anthropic (T.pack k)
        (Nothing, Nothing, Nothing) -> ollama

  let config = defaultConfig provider

  let userText =
        "The login page crashes with a 500 server error whenever a user submits an email containing a '+' symbol. "
          <> "This prevents users with sub-addressed emails from signing in. "
          <> "We should fix this urgently before the marketing campaign launches tomorrow."

  result <- runLLM config $ do
    report :: BugReport <- askStructured ("Parse this issue description into a structured bug report:\n" <> userText)
    liftIO $ do
      putStrLn "Successfully extracted strongly-typed BugReport:"
      putStrLn $ "  Title:     " <> T.unpack (title report)
      putStrLn $ "  Component: " <> T.unpack (component report)
      putStrLn $ "  Priority:  " <> show (priority report)
      putStrLn $ "  Steps:     " <> show (stepsToReproduce report)
      putStrLn $ "  Cause:     " <> T.unpack (suspectedCause report)

  case result of
    Left err -> putStrLn $ "Error: " <> show err
    Right () -> putStrLn "\nExtraction finished."
