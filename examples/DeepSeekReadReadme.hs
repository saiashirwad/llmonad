{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Effectful (Eff, runEff)
import LLMonad
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)

definition :: AgentDef T.Text T.Text
definition =
    textAgent
        "Inspect the project with the supplied read-only tools. Base each answer on file contents."
        id

workflow :: Agent es T.Text T.Text -> Eff es T.Text
workflow projectReader = invoke projectReader "Read README.md and give a short project summary."

main :: IO ()
main = do
    key <- requireDeepSeekKey
    projectRoot <- getCurrentDirectory
    let projectReader =
            bind
                (model (deepSeekProvider key) "deepseek-v4-flash")
                (tools [viewFileTool])
                definition
    reply <- runEff . runWorldLocal projectRoot $ workflow projectReader
    TIO.putStrLn reply

requireDeepSeekKey :: IO T.Text
requireDeepSeekKey = do
    value <- lookupEnv "DEEPSEEK_API_KEY"
    case value of
        Just key | not (null key) -> pure (T.pack key)
        _ -> fail "DEEPSEEK_API_KEY is not set"
