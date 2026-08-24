{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import DeepSeek (deepSeekRuntime)
import Effectful (Eff, runEff)
import LLMonad
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)

definition :: AgentDef T.Text T.Text
definition =
    withAgentOpts (defaultAgentOpts{agentMaxRounds = 16}) $
        textAgent
            "Answer questions about this project. Inspect only the files that you need, cite file paths, and stop using tools when you have enough evidence."
            id

workflow :: Agent es T.Text T.Text -> T.Text -> Eff es T.Text
workflow = invoke

main :: IO ()
main = do
    arguments <- getArgs
    projectRoot <- getCurrentDirectory
    let question = case arguments of
            [] -> "Describe the main modules and how they work together."
            _ -> T.pack (unwords arguments)
    runtime <- deepSeekRuntime "deepseek-v4-flash"
    let projectAgent =
            bind
                runtime
                readOnlyCodingToolset
                definition
    reply <- runEff . runWorldLocal projectRoot $ workflow projectAgent question
    TIO.putStrLn reply
