{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Effectful (Eff, runEff)
import LLMonad
import System.Environment (lookupEnv)

definition :: AgentDef T.Text T.Text
definition = textAgent "Answer clearly and briefly." id

workflow :: Agent es T.Text T.Text -> T.Text -> Eff es T.Text
workflow = invoke

main :: IO ()
main = do
  key <- requireDeepSeekKey
  let haskellAgent = bind (model (deepSeekProvider key) "deepseek-v4-flash") noTools definition
  reply <- runEff (workflow haskellAgent "Give me one useful fact about Haskell.")
  TIO.putStrLn reply

requireDeepSeekKey :: IO T.Text
requireDeepSeekKey = do
  value <- lookupEnv "DEEPSEEK_API_KEY"
  case value of
    Just key | not (null key) -> pure (T.pack key)
    _ -> fail "DEEPSEEK_API_KEY is not set"
