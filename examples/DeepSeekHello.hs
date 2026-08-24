{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Effectful (Eff, IOE, (:>), liftIO, runEff)
import LLMonad
import System.Environment (lookupEnv)

workflow :: (LLM :> es, IOE :> es) => Eff es ()
workflow = do
  setSystem "Answer clearly and briefly."
  reply <- generateText "Give me one useful fact about Haskell."
  liftIO (TIO.putStrLn reply)

main :: IO ()
main = do
  key <- requireDeepSeekKey
  let config = defaultConfig (deepSeekProvider key) "deepseek-v4-flash"
  runEff (runLLMHTTP config workflow)

requireDeepSeekKey :: IO T.Text
requireDeepSeekKey = do
  value <- lookupEnv "DEEPSEEK_API_KEY"
  case value of
    Just key | not (null key) -> pure (T.pack key)
    _ -> fail "DEEPSEEK_API_KEY is not set"
