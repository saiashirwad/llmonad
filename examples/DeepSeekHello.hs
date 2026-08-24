{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import DeepSeek (deepSeekRuntime)
import Effectful (Eff, runEff)
import LLMonad

definition :: AgentDef T.Text T.Text
definition = textAgent "Answer clearly and briefly." id

workflow :: Agent es T.Text T.Text -> T.Text -> Eff es T.Text
workflow = invoke

main :: IO ()
main = do
    runtime <- deepSeekRuntime "deepseek-v4-flash"
    let haskellAgent = bind runtime noTools definition
    reply <- runEff (workflow haskellAgent "Give me one useful fact about Haskell.")
    TIO.putStrLn reply
