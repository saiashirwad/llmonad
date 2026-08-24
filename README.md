# LLMonad

LLMonad is a programmatic Haskell interface for large language models. It uses
`effectful` for model access, tools, file access, and test interpreters.

The package is a library. It does not contain a CLI or a terminal interface.

## Basic use

The effect row shows the capabilities that a program needs. The application
selects the provider and interprets the effects at its outer edge.

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

program :: (LLM :> es, IOE :> es) => Eff es ()
program = do
  setSystem "Answer clearly and briefly."
  answer <- generateText "Give me one useful fact about Haskell."
  liftIO (TIO.putStrLn answer)

main :: IO ()
main = do
  key <- T.pack <$> getEnv "DEEPSEEK_API_KEY"
  let config = defaultConfig (deepSeekProvider key) "deepseek-v4-flash"
  runEff (runLLMHTTP config program)
```

`program` does not know which provider runs it. `runLLMHTTP` is the HTTP
adapter at the `LLM` seam. Tests can use `runLLMMock` at the same seam.

## Read a project

The `World` effect gives tools controlled access to one project directory.

```haskell
program :: (LLM :> es, World :> es, IOE :> es) => Eff es T.Text
program =
  runAgent
    [viewFileTool, grepSearchTool, findByNameTool, listDirTool]
    "Find the World interpreter and explain how it works."

main :: IO ()
main = do
  key <- T.pack <$> getEnv "DEEPSEEK_API_KEY"
  root <- getCurrentDirectory
  let config = defaultConfig (deepSeekProvider key) "deepseek-v4-flash"
  runEff
    . runLLMHTTP config
    . runWorldLocal root
    $ program
```

The type of `program` states its authority. `runWorldLocal` restricts file and
process operations to the selected project root.

## DeepSeek examples

Set the key in the environment:

```bash
export DEEPSEEK_API_KEY="your-api-key"
```

Run the examples:

```bash
cabal run deepseek-hello
cabal run deepseek-read-readme
cabal run deepseek-ask-project -- "Where is the World effect interpreted?"
```

The project-reading examples use read-only tools.

## Main modules

- `LLMonad` is the main interface.
- `LLMonad.Core` defines the `LLM` effect.
- `LLMonad.Interpreter.HTTP` provides the live HTTP adapter.
- `LLMonad.Interpreter.Mock` provides the deterministic test adapter.
- `LLMonad.Agent` runs tool-use loops.
- `LLMonad.Tools.Coding` provides project tools.
- `LLMonad.World.Local` interprets project access in a local directory.

## Build and test

```bash
cabal build all --ghc-options="-Wall -Werror"
cabal test -j1
```
