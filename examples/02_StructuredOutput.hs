{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | LLMonad Example 2: Structured Output & Automatic Error Recovery
-- Demonstrates schema derivation with GHC Generics, typed askStructured,
-- and extractWithRetry error self-correction feedback loop.
module Main where

import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import GHC.Generics (Generic)
import LLMonad
import System.Environment (lookupEnv)

data Priority = Low | Medium | High | Critical
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data BugReport = BugReport
  { title :: Text
  , priority :: Priority
  , affectedModules :: [Text]
  , estimatedFixHours :: Double
  , hasReproductionSteps :: Bool
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

workflow :: (LLM :> es, IOE :> es) => Eff es ()
workflow = do
  liftIO (putStrLn "--- 1. Direct Structured Extraction (askStructured) ---")
  let issueText =
        "The HTTP connection pool leaks open file descriptors under high concurrency \
        \in LLMonad.Interpreter.HTTP and LLMonad.Internal.Http. We need immediate fix, \
        \estimated around 3.5 hours. Steps to repro: spawn 100 concurrent workers."
  bug <- askStructured @BugReport ("Extract bug report details from this text:\n" <> issueText)
  liftIO (putStrLn ("Parsed Bug Report:\n" <> show bug))

  liftIO (putStrLn "\n--- 2. Self-Correcting Error Recovery (extractWithRetry) ---")
  let noisyInput = "Fix typo in README documentation on line 12. Low priority, about 0.5 hours."
  recoveredBug <- extractWithRetry @BugReport 3 ("Extract bug report from noisy text:\n" <> noisyInput)
  liftIO (putStrLn ("Recovered Bug Report:\n" <> show recoveredBug))

main :: IO ()
main = do
  mKey <- lookupEnv "OPENAI_API_KEY"
  case mKey of
    Just k -> do
      let cfg = defaultConfig (openAIProvider (T.pack k)) "gpt-4o-mini"
      runEff (runLLMHTTP cfg workflow)
    Nothing -> do
      putStrLn "Note: OPENAI_API_KEY not set; executing against pure in-memory mock handler.\n"
      let sampleBug1 = BugReport "Connection pool leak" Critical ["LLMonad.Interpreter.HTTP", "LLMonad.Internal.Http"] 3.5 True
          sampleBug2 = BugReport "Fix typo in README" Low ["README.md"] 0.5 False
          script =
            [ Right (structuredResp (toJSON sampleBug1))
            , Right (textResp "Malformed response that violates JSON schema")
            , Right (structuredResp (toJSON sampleBug2))
            ]
      (res, _) <- runEff (runLLMMock script workflow)
      pure res
