{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

{- | Shared DeepSeek wiring for the examples.

Every example keys off the DEEPSEEK_API_KEY environment variable and gets
a 'ModelRuntime' in one line:

> main = do
>     runtime <- deepSeekRuntime "deepseek-v4-flash"
>     reply <- runEff . invoke $ mount runtime noTools definition
-}
module DeepSeek (deepSeekRuntime) where

import Data.Text (Text)
import Data.Text qualified as T
import Effectful (IOE, (:>))
import LLMonad
import System.Environment (lookupEnv)
import System.Exit (die)

{- | A model runtime for @modelName@, keyed by the @DEEPSEEK_API_KEY@
environment variable. Exits with instructions when the variable is unset or
empty.
-}
deepSeekRuntime :: (IOE :> es) => Text -> IO (ModelRuntime es)
deepSeekRuntime modelName = do
    mKey <- lookupEnv "DEEPSEEK_API_KEY"
    case mKey of
        Just key
            | not (null key) ->
                pure (model (deepSeekProvider (T.pack key)) (Model modelName))
        _ -> die "DEEPSEEK_API_KEY is not set. Export your key: export DEEPSEEK_API_KEY=\"sk-...\""
