# LLMonad

LLMonad is an Effectful library for typed agents, tools, and workflows, built
around one guarantee: **a tool declares the effects it needs, those
requirements travel with the tool in its type, and they discharge exactly
once — at the program edge, where an interpreter picks the semantics.**

What follows from that:

- **Confinement is chosen by the interpreter, not begged for in a prompt.**
  Run under `runWorldLocal "./workspace"` and every path a tool touches
  resolves inside that root; anything else fails. Swap the interpreter and the
  same agent runs against in-memory files or a throwaway Git worktree.
- **Testing is an argument swap.** The same `mount` call that attaches a live
  provider can attach a scripted model, so the workflow your tests exercise is
  the workflow that ships — no network, no disk.
- **Workflow code never names a provider, model, or HTTP detail.** An
  `AgentDef` says what an agent does; `mount` gives that definition a model and
  a toolset; workflows are plain functions taking configured agents.

## Define the agents

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Effectful
import GHC.Generics (Generic)
import LLMonad
import System.Environment (getEnv)

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

`invoke` starts with an empty conversation. The two `invoke` calls above run
concurrently and share nothing.

## Give each agent its tools

A `Toolset` is the set of tools one agent may call. Build one with `tools`, and
merge sets with `(<>)`. Both reject duplicate tool names, so a bad assembly
fails before any tokens are spent.

```haskell
researchTools :: World :> es => Toolset es
researchTools =
  tools [viewFileTool, grepSearchTool, listDirTool]
    <> tools [countLinesTool]
```

Tool handlers run in `Eff`, so the effects a tool needs stay in the type of the
toolset. `viewFileTool` reads files through the `World` effect, which is why
`researchTools` carries `World :> es`. That constraint follows the agent
around until the program interprets `World` at the edge — which is also how
the reviewer below ends up unable to touch the disk at all.

Write your own tool by wrapping a function whose argument type derives
`FromJSON` and `ToSchema`. The derived schema is what the model sees; the
result is encoded as JSON and handed back to it.

```haskell
data CountLines = CountLines {file :: Text}
  deriving (Generic, FromJSON, ToSchema)

countLinesTool :: World :> es => Tool (Eff es)
countLinesTool =
  tool "count_lines" "Count the lines in a file" $ \(CountLines path) -> do
    contents <- readFileText (T.unpack path)
    pure (length (T.lines contents))
```

### Tools that ship with the library

| Tool             | What the model can do               |
| ---------------- | ----------------------------------- |
| `viewFileTool`   | read a file, or a line range of one |
| `grepSearchTool` | search file contents                |
| `findByNameTool` | find paths by name                  |
| `listDirTool`    | list a directory                    |
| `editFileTool`   | replace text in a file              |
| `runCommandTool` | run a command in the workspace      |

Two ready-made sets bundle them:

```haskell
readOnlyCodingToolset :: World :> es => Toolset es  -- view, grep, find, list
standardCodingToolset :: World :> es => Toolset es  -- the six above
```

Use `noTools` for an agent that only reasons over the text it is given.

## Attach models at the edge

```haskell
main :: IO ()
main = do
  key <- T.pack <$> getEnv "DEEPSEEK_API_KEY"
  let runtime = model (deepSeekProvider key) "deepseek-v4-flash"
      researcher = mount runtime researchTools researcherDef
      reviewer = mount runtime noTools reviewerDef
  report <- runEff . runWorldLocal "." $ workflow researcher reviewer
  TIO.putStrLn report
```

Three things happen in that `let`:

- Each agent gets a model. `mount` is the only place a provider is named, so
  giving the reviewer a different `runtime` mixes models in one workflow.
- Each agent gets its own tools. The researcher reads the project; the
  reviewer cannot touch the disk at all.
- `runWorldLocal "."` interprets `World`, discharging the constraint the
  toolset carried.

### Every path stays inside the root

`runWorldLocal` canonicalizes the root you give it, and every file operation
resolves through it. A tool asked for `../../etc/passwd` or an absolute path
outside the root gets an error, not your filesystem.

The same `World` effect has other interpreters, and swapping them changes
nothing above:

- `runWorldMemoryWithFiles` serves files from memory — no disk involved.
- `runWorldWorktree` runs the computation inside an ephemeral Git worktree
  that is removed afterward, returning a summary of what changed.

## Test without a provider

`mockModel` is a `ModelRuntime` backed by a script. Pair it with the
in-memory `World` and the production workflow runs in a test touching neither
network nor disk:

```haskell
report <-
  runEff . fmap fst . runWorldMemoryWithFiles [("README.md", "hello")] $
    workflow
      (mount (mockModel [Right (textResp "evidence")]) researchTools researcherDef)
      (mount (mockModel [Right (textResp "no unsupported claims")]) noTools reviewerDef)
```

Each script restarts for every invocation, which keeps independent agent calls
deterministic. Only the arguments to `mount` differ from production — the
workflow under test is the shipped code.

When a run must be inspectable after the fact, add the `Journal` effect:
`runJournalFile "session.jsonl"` persists every recorded turn and tool call.
A recording can then pin behavior as a regression test — every recorded model
turn and tool invocation plays back in order, and anything unrecorded raises
`ReplayDivergence` instead of improvising:

```haskell
let script = extractReplayScript events
runtime <- strictReplayRuntime script
replayResearchTools <- strictReplayToolset script researchTools
report <-
  runEff . fmap fst . runWorldMemoryWithFiles [] $
    workflow
      (mount runtime replayResearchTools researcherDef)
      (mount runtime noTools reviewerDef)
```

The stream is session-wide: both agents above draw from one continuous
recording, so adding a third `invoke` to the workflow names itself in the
failure rather than silently shifting every later answer.

## Keep a conversation

`invoke` forgets. `start` returns a session that remembers:

```haskell
chat <- start researcher
first <- continue chat "Read README.md."
second <- continue chat "Now check the source against that description."
```

## Ask for a record instead of text

`structuredAgent` demands JSON that fits a derived schema, and retries the
model when the reply does not decode.

```haskell
data Verdict = Verdict
  { severity :: Text
  , summary :: Text
  }
  deriving (Show, Generic, FromJSON, ToSchema)

verdictDef :: AgentDef Text Verdict
verdictDef = structuredAgent "Report the single worst problem in the file." id
```

An agent gives up after eight model rounds. Raise the ceiling per definition:

```haskell
patientResearcher :: AgentDef Text Text
patientResearcher =
  withAgentOpts defaultAgentOpts{agentMaxRounds = 16} researcherDef
```

## Wrap the model

Middleware wraps a `ModelRuntime`, so caching, tracing, and rate limits attach
to one agent rather than to the whole program. The leftmost middleware sees
traffic first.

```haskell
store <- newInMemoryCache
limiter <- newRateLimiter 5 10
let runtime =
      applyMiddleware
        (traced print <> cached "deepseek-v4-flash" store <> rateLimited limiter)
        (model (deepSeekProvider key) "deepseek-v4-flash")
```

## Examples

```bash
export DEEPSEEK_API_KEY="your-api-key"

cabal run deepseek-hello
cabal run deepseek-read-readme
cabal run deepseek-ask-project -- "Where is the World effect interpreted?"
cabal run deepseek-review-file -- src/LLMonad/Agent.hs
cabal run quasiquotes-example
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
