{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Effectful (Eff, IOE, (:>), liftIO, runEff)
import LLMonad
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, lookupEnv)

workflow :: (LLM :> es, World :> es, IOE :> es) => T.Text -> Eff es ()
workflow question = do
  setSystem "Answer questions about this project. Inspect only the files that you need, cite file paths, and stop using tools when you have enough evidence."
  let options = defaultAgentOpts { agentMaxRounds = 16 }
      readOnlyTools = [viewFileTool, grepSearchTool, findByNameTool, listDirTool]
  reply <- runAgentWith options readOnlyTools question
  liftIO (TIO.putStrLn reply)

main :: IO ()
main = do
  arguments <- getArgs
  key <- requireDeepSeekKey
  projectRoot <- getCurrentDirectory
  let question = case arguments of
        [] -> "Describe the main modules and how they work together."
        _  -> T.pack (unwords arguments)
      config = defaultConfig (deepSeekProvider key) "deepseek-v4-flash"
  runEff
    . runLLMHTTP config
    . runWorldLocal projectRoot
    $ workflow question

requireDeepSeekKey :: IO T.Text
requireDeepSeekKey = do
  value <- lookupEnv "DEEPSEEK_API_KEY"
  case value of
    Just key | not (null key) -> pure (T.pack key)
    _ -> fail "DEEPSEEK_API_KEY is not set"
