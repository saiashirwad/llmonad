{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import LLMonad
import System.Environment (lookupEnv)

main :: IO ()
main = do
  putStrLn "=== Example 04: Multi-Turn Stateful Conversation ==="

  deepseekKey <- lookupEnv "DEEPSEEK_API_KEY"
  openaiKey <- lookupEnv "OPENAI_API_KEY"
  anthropicKey <- lookupEnv "ANTHROPIC_API_KEY"

  let provider = case (deepseekKey, openaiKey, anthropicKey) of
        (Just k, _, _) -> deepseek (T.pack k)
        (_, Just k, _) -> openai (T.pack k)
        (_, _, Just k) -> anthropic (T.pack k)
        (Nothing, Nothing, Nothing) -> ollama

  let config =
        withSystemPrompt "You are an expert Haskell functional programming mentor." $
          defaultConfig provider

  result <- runLLM config $ do
    ans1 <- ask "What is a Monad in simple words?"
    liftIO $ TIO.putStrLn $ "Turn 1 Assistant:\n" <> ans1 <> "\n"

    ans2 <- ask "How is it different from an Applicative?"
    liftIO $ TIO.putStrLn $ "Turn 2 Assistant:\n" <> ans2 <> "\n"

    ans3 <- ask "Can you show a short Haskell code snippet comparing (<*>) and (>>=)?"
    liftIO $ TIO.putStrLn $ "Turn 3 Assistant:\n" <> ans3 <> "\n"

    history <- getConversation
    liftIO $ putStrLn $ "Total messages in conversation history: " <> show (length history)

  case result of
    Left err -> putStrLn $ "Error: " <> show err
    Right () -> putStrLn "\nMulti-turn conversation completed."
