{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | LLMonad Example 5: Effectful Handlers & Middleware Stacking
-- Demonstrates pluggable interpreters, transparent response caching (withCache),
-- execution tracing (withTrace), and client-side rate limiting (withRateLimit).
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Effectful
import LLMonad
import System.Environment (lookupEnv)

workflow :: (LLM :> es, IOE :> es) => Eff es ()
workflow = do
  liftIO (putStrLn "--- 1. First Round (Cache Miss) ---")
  r1 <- generateText "What is the capital of France?"
  liftIO (TIO.putStrLn ("Response 1: " <> r1))

  liftIO (putStrLn "\n--- 2. Second Round with Same Request (Cache Hit) ---")
  clearHistory
  r2 <- generateText "What is the capital of France?"
  liftIO (TIO.putStrLn ("Response 2 (cached): " <> r2))

  liftIO (putStrLn "\n--- 3. Distinct Request (Cache Miss) ---")
  clearHistory
  r3 <- generateText "What is the capital of Japan?"
  liftIO (TIO.putStrLn ("Response 3: " <> r3))

main :: IO ()
main = do
  cache <- newInMemoryCache
  limiter <- newRateLimiter 5 1.0 -- 5 requests/sec max
  let tracer :: Trace -> IO ()
      tracer = \case
        TraceRequest reqModel sys msgs ->
          putStrLn ("[TRACE] Request -> model: " ++ show reqModel ++ ", sys: " ++ show sys ++ ", msgCount: " ++ show (length msgs))
        TraceResponse txt calls usage ->
          putStrLn ("[TRACE] Response -> length: " ++ show (T.length txt) ++ ", tools: " ++ show (length calls) ++ ", usage: " ++ show usage)
        TraceToolExecuted name ok summary ->
          putStrLn ("[TRACE] Tool Executed -> " ++ T.unpack name ++ " (ok=" ++ show ok ++ "): " ++ T.unpack summary)
        TraceError err ->
          putStrLn ("[TRACE] Error -> " ++ show err)

  mKey <- lookupEnv "OPENAI_API_KEY"
  case mKey of
    Just k -> do
      let cfg = defaultConfig (openAIProvider (T.pack k)) "gpt-4o-mini"
      runEff
        . runLLMHTTP cfg
        . withRateLimit limiter
        . withCache cache
        . withTrace tracer
        $ workflow
    Nothing -> do
      putStrLn "Note: OPENAI_API_KEY not set; executing against pure in-memory mock handler with middleware.\n"
      let script =
            [ Right (textResp "The capital of France is Paris.")
            , Right (textResp "The capital of Japan is Tokyo.")
            ]
      _ <- runEff
        . runLLMMock script
        . withRateLimit limiter
        . withCache cache
        . withTrace tracer
        $ workflow
      pure ()
