# LLMonad

LLMonad is an [Effectful](https://github.com/haskell-effectful/effectful)
library for typed LLM agents, tools, and workflows: agents call tools, and
workflows compose agents. It has one rule: workflow code never names a
provider, a model, or an HTTP detail. Agents are values, workflows are plain
functions over them, and a model attaches only at the program edge — so the
same workflow runs against a live provider, a scripted mock, or a recorded
session.

## The whole program at a glance

One agent, one tool, one model:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Effectful
import LLMonad
import System.Environment (getEnv)

-- The definition: what the agent is. No model appears here.
readerDef :: AgentDef Text Text
readerDef = textAgent "Inspect the project and answer from its files." id

main :: IO ()
main = do
  key <- T.pack <$> getEnv "DEEPSEEK_API_KEY"
  -- The edge: the only lines that name a provider and a model.
  let runtime = model (deepSeekProvider key) "deepseek-v4-flash"
      reader = mount runtime (tools [viewFileTool]) readerDef
  reply <- runEff . runWorldLocal "." $ invoke reader "Summarize README.md."
  TIO.putStrLn reply
```

Four names carry the program:

- `textAgent` builds an `AgentDef`: the agent's system prompt, how inputs
  become prompt text, and that it answers in plain text.
- `mount` turns a definition into a callable `Agent` by attaching a model
  runtime and a toolset. It is the only place a provider is named.
- `invoke` calls the agent once, starting from an empty conversation.
- `runEff . runWorldLocal "."` is the program edge: above it, effects exist
  only in types; here they become real work. `runWorldLocal` interprets file
  access and confines every path to the root you give it.

## The same program in a test

Swap the arguments to `mount` and the `World` interpreter; nothing else
changes. `mockModel` answers from a script, and the in-memory world serves
files from a list, so the test touches neither network nor disk:

```haskell
reply <-
  runEff . fmap fst . runWorldMemoryWithFiles [("README.md", "hello")] $
    invoke
      (mount (mockModel [Right (textResp "a greeting")]) (tools [viewFileTool]) readerDef)
      "Summarize README.md."
```

That swap is the point of the library. The rest of this README builds a
two-agent workflow and introduces each part — definitions, tools, models,
tests — in the order the code needs them.

## Define the agents

```haskell
researcherDef :: AgentDef Text Text
researcherDef =
  textAgent "Inspect the project and report evidence from its files." id

reviewerDef :: AgentDef Text Text
reviewerDef =
  textAgent "Check the report for unsupported claims." id

workflow :: Agent es Text Text -> Agent es Text Text -> Eff es Text
workflow researcher reviewer =
  invoke researcher "Read the main modules." >>= invoke reviewer
```

An `AgentDef` is one agent's specification: its system prompt, how inputs
become prompt text, and whether it answers with plain text or structured
data. It names no model; `mount` attaches one at the edge.

`Eff es` is an Effectful computation whose allowed effects are listed in
`es`. A workflow is such a computation over the agents it receives as
arguments — nothing more, so ordinary combinators apply. To research two
areas at once, replace the body with:

```haskell
(overview, tests) <-
  concurrently
    (invoke researcher "Read the main modules.")
    (invoke researcher "Read the tests.")
invoke reviewer (overview <> "\n\n" <> tests)
```

The two `invoke` calls run concurrently and share nothing: each starts from
an empty conversation. `concurrently` needs raw `IO`, so the signature gains
`IOE :> es` — read `:>` as "is available in". `IOE` is Effectful's effect
for raw `IO`.

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

`World` is LLMonad's effect for the machine: files, directories, commands.
Tool handlers run in `Eff`, so the effects a tool needs stay in the type of
the toolset. `viewFileTool` reads files through `World`, which is why
`researchTools` carries `World :> es`. The constraint follows the agent
around until the program interprets `World` at the edge, which is also why
the reviewer below never touches the disk.

Write your own tool by wrapping a function whose argument type derives
`FromJSON` and `ToSchema`. The derived schema is what the model sees; the
result is encoded as JSON and handed back to it.

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

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

`model` returns a `ModelRuntime`: one configured model, its provider, name,
and default parameters, as an ordinary value. Mount the same runtime on
several agents, wrap it in middleware (below), or swap it for `mockModel` in
tests.

What each part does:

- Each agent gets a model. `mount` is the only place a provider is named, so
  giving the reviewer a different `runtime` mixes models in one workflow.
- Each agent gets its own tools. The researcher reads the project; the
  reviewer gets no tools and only reasons over text.
- `runWorldLocal "."` interprets `World`, discharging the constraint the
  toolset carried.

### Every path stays inside the root

`runWorldLocal` canonicalizes the root you give it, and every file operation
resolves through it. A tool asked for `../../etc/passwd` or an absolute path
outside the root gets an error, not your filesystem.

The same `World` effect has other interpreters, and swapping them changes
nothing above:

- `runWorldMemoryWithFiles` serves files from memory (no disk involved).
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

- Each script restarts for every invocation, so independent agent calls stay
  deterministic.
- Only the arguments to `mount` differ from production. The workflow under
  test is the shipped code.

### Record a session

`Journal` is LLMonad's session log effect: model turns and tool calls,
recorded as they happen. Interpret it with `runJournalFile "session.jsonl"`
and every recorded turn and tool call persists to that file.

### Replay a recording as a regression test

A recording doubles as a regression test:

- every recorded model turn and tool invocation plays back, in order
- anything unrecorded raises `ReplayDivergence` instead of improvising
- the stream is session-wide: both agents draw from one continuous recording
- adding another `invoke` fails with that call named in the error, instead of
  shifting every later answer

```haskell
events <- runEff (resumeSession "session.jsonl")
let script = extractReplayScript events
runtime <- strictReplayRuntime script
replayResearchTools <- strictReplayToolset script researchTools
report <-
  runEff . fmap fst . runWorldMemoryWithFiles [] $
    workflow
      (mount runtime replayResearchTools researcherDef)
      (mount runtime noTools reviewerDef)
```

## Keep a conversation

`invoke` forgets. `start` returns a session that remembers:

```haskell
chat <- start researcher
first <- continue chat "Read README.md."
second <- continue chat "Now check the source against that description."
```

## Ask for structured output

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

## Cap the rounds

A round is one model reply plus the tool calls that reply requested. An
agent gives up after eight; raise the ceiling per definition:

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
cabal run quasiquotes-example   # prompt and makeTool Template Haskell helpers
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
