# LLMonad

LLMonad is an Effectful library for typed agents, tools, and workflows.

An `AgentDef` describes an agent without selecting a model. `bind` attaches a
model and a toolset. A workflow receives configured agents, so its code stays
independent of providers and model names.

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Data.Text (Text)
import Effectful
import LLMonad

researcherDef :: AgentDef Text Text
researcherDef =
  textAgent "Inspect the project and report evidence from its files." id

reviewerDef :: AgentDef Text Text
reviewerDef =
  textAgent "Check the report for unsupported claims." id

workflow ::
  IOE :> es =>
  Agent es Text Text ->
  Agent es Text Text ->
  Eff es Text
workflow researcher reviewer = do
  (overview, tests) <-
    concurrently
      (invoke researcher "Read the main modules.")
      (invoke researcher "Read the tests.")
  invoke reviewer (overview <> "\n\n" <> tests)
```

Configure the agents at the edge of the program:

```haskell
let runtime = model (deepSeekProvider apiKey) "deepseek-v4-flash"
    researcher = bind runtime readOnlyCodingToolset researcherDef
    reviewer = bind runtime noTools reviewerDef

report <- runEff . runWorldLocal projectRoot $ workflow researcher reviewer
```

`invoke` starts with empty conversation history. Use `Session` when a
conversation must keep its history:

```haskell
session <- start researcher
first <- continue session "Read README.md."
second <- continue session "Now check the source against that description."
```

Toolsets compose with `(<>)`. Tool handlers run in `Eff`, so their required
effects stay in the type of the toolset.

```haskell
projectTools :: World :> es => Toolset es
projectTools = tools [viewFileTool, grepSearchTool] <> tools [listDirTool]
```

## Examples

```bash
export DEEPSEEK_API_KEY="your-api-key"

cabal run deepseek-hello
cabal run deepseek-read-readme
cabal run deepseek-ask-project -- "Where is the World effect interpreted?"
```

## Development

Install GHC, Cabal, and HLS with
[GHCup](https://www.haskell.org/ghcup/). Install
[DotSlash](https://dotslash-cli.com/docs/installation/) once as the runner for
project-pinned developer tools. Then fetch and verify the prebuilt tools:

```bash
make tools
```

The repository pins the Fourmolu version in `tools/fourmolu`. DotSlash keeps the
downloaded binary in its user cache and does not add Fourmolu to the user
`PATH`.

```bash
make build          # Build all components
make test           # Run all tests
make repl           # Start GHCi for the library
make format         # Format Haskell source files
make format-check   # Check the format without changes
make check          # Check the format, build, and test
```
