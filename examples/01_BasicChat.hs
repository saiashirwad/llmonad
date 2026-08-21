{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LLMonad
import System.Environment (lookupEnv)

main :: IO ()
main = do
  putStrLn "=== Example 01: Basic Chat ==="

  -- Auto-detect DeepSeek, OpenAI, Anthropic, or fallback to local Ollama
  deepseekKey <- lookupEnv "DEEPSEEK_API_KEY"
  openaiKey <- lookupEnv "OPENAI_API_KEY"
  anthropicKey <- lookupEnv "ANTHROPIC_API_KEY"

  let provider = case (deepseekKey, openaiKey, anthropicKey) of
        (Just k, _, _) -> deepseek (T.pack k)
        (_, Just k, _) -> openai (T.pack k)
        (_, _, Just k) -> anthropic (T.pack k)
        (Nothing, Nothing, Nothing) -> ollama

  let config = defaultConfig provider

  result <- runLLM config $ do
    response1 <- ask "What are the three most important features of Haskell?"
    liftIO $ TIO.putStrLn $ "LLM Response:\n" <> response1

    response2 <- ask "Can you summarize that in one single haiku?"
    liftIO $ TIO.putStrLn $ "\nHaiku:\n" <> response2

  case result of
    Left err -> putStrLn $ "Error: " <> show err
    Right () -> putStrLn "\nConversation finished."
