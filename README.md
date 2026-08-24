# LLMonad

A typed Effectful interface for LLM calls and tool-using agents in Haskell.

## DeepSeek

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Effectful (Eff, IOE, (:>), liftIO, runEff)
import LLMonad
import System.Environment (getEnv)

hello :: (LLM :> es, IOE :> es) => Eff es ()
hello = do
  answer <- generateText "Give me one useful fact about Haskell."
  liftIO (TIO.putStrLn answer)

main :: IO ()
main = do
  key <- T.pack <$> getEnv "DEEPSEEK_API_KEY"
  let config = defaultConfig (deepSeekProvider key) "deepseek-v4-flash"
  runEff (runLLMHTTP config hello)
```

`runLLMMock` runs the same `LLM` effect without an HTTP request.

## Examples

```bash
export DEEPSEEK_API_KEY="your-api-key"

cabal run deepseek-hello
cabal run deepseek-read-readme
cabal run deepseek-ask-project -- "Where is the World effect interpreted?"
```

The project examples use `World` with read-only file tools. See
[`DeepSeekAskProject.hs`](examples/DeepSeekAskProject.hs) for the complete
effect stack.

## Development

```bash
cabal build all --ghc-options="-Wall -Werror"
cabal test -j1
```
