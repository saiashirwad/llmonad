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
import System.Environment (lookupEnv)

workflow :: (LLM :> es, World :> es, IOE :> es) => Eff es ()
workflow = do
  setSystem "Inspect the project with the supplied read-only tools. Base each answer on file contents."
  reply <- runAgent [viewFileTool] "Read README.md and give a short project summary."
  liftIO (TIO.putStrLn reply)

main :: IO ()
main = do
  key <- requireDeepSeekKey
  projectRoot <- getCurrentDirectory
  let config = defaultConfig (deepSeekProvider key) "deepseek-v4-flash"
  runEff
    . runLLMHTTP config
    . runWorldLocal projectRoot
    $ workflow

requireDeepSeekKey :: IO T.Text
requireDeepSeekKey = do
  value <- lookupEnv "DEEPSEEK_API_KEY"
  case value of
    Just key | not (null key) -> pure (T.pack key)
    _ -> fail "DEEPSEEK_API_KEY is not set"
