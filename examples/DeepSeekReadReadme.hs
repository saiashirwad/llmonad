{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import DeepSeek (deepSeekRuntime)
import Effectful (Eff, runEff)
import LLMonad
import System.Directory (getCurrentDirectory)

definition :: AgentDef T.Text T.Text
definition =
    textAgent
        "Inspect the project with the supplied read-only tools. Base each answer on file contents."
        id

workflow :: Agent es T.Text T.Text -> Eff es T.Text
workflow projectReader = invoke projectReader "Read README.md and give a short project summary."

main :: IO ()
main = do
    runtime <- deepSeekRuntime "deepseek-v4-flash"
    projectRoot <- getCurrentDirectory
    let projectReader =
            mount
                runtime
                (tools [viewFileTool])
                definition
    reply <- runEff . runWorldLocal projectRoot $ workflow projectReader
    TIO.putStrLn reply
