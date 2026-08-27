# LLMonad

LLMonad is an [Effectful](https://github.com/haskell-effectful/effectful)
library for typed LLM agents, tools, and workflows: agents call tools, and
workflows compose agents. Workflow code never names a provider, a model, or
an HTTP detail. A model attaches at the program edge, so the same workflow
runs against a live provider, a scripted mock, or a recorded session.

## The whole program

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Effectful
import LLMonad
import System.Environment (getEnv)

-- An AgentDef is the agent's specification: its system prompt, how inputs
-- become prompt text, and the shape of its answers (plain text here).
-- It names no model.
readerDef :: AgentDef Text Text
readerDef = textAgent "Inspect the project and answer from its files." id

main :: IO ()
main = do
  key <- T.pack <$> getEnv "DEEPSEEK_API_KEY"
  let runtime = model (deepSeekProvider key) "deepseek-v4-flash"
      -- mount attaches a model runtime and a toolset to a definition and
      -- returns a callable Agent. Only mount ever names a provider.
      reader = mount runtime (tools [viewFileTool]) readerDef
  -- The program edge: runWorldLocal interprets file access, confined to
  -- the root ".". invoke calls the agent once, from an empty conversation.
  reply <- runEff . runWorldLocal "." $ invoke reader "Summarize README.md."
  TIO.putStrLn reply
```

## The same program in a test

```haskell
-- Same definition, same call. Only the mount arguments and the World
-- interpreter changed: mockModel answers from a script, and the in-memory
-- world serves files from a list. No network, no disk.
reply <-
  runEff . fmap fst . runWorldMemoryWithFiles [("README.md", "hello")] $
    invoke
      (mount (mockModel [Right (textResp "a greeting")]) (tools [viewFileTool]) readerDef)
      "Summarize README.md."
```

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

`Eff es` is an Effectful computation whose allowed effects are listed in
`es`. A workflow is such a computation over the agents it receives as
arguments, nothing more, so ordinary combinators apply. To research two
areas at once, replace the body with:

```haskell
(overview, tests) <-
  concurrently
    (invoke researcher "Read the main modules.")
    (invoke researcher "Read the tests.")
invoke reviewer (overview <> "\n\n" <> tests)
```

The two `invoke` calls run concurrently and share nothing: each starts from
an empty conversation. `concurrently` needs raw `IO` (Effectful's `IOE`
effect), so the workflow's signature gains `IOE :> es`. Read `:>` as "is
available in".

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
      -- One runtime, mounted twice. Give the reviewer a different runtime
      -- to mix models in one workflow.
      researcher = mount runtime researchTools researcherDef
      -- No tools: the reviewer only reasons over the text it is given.
      reviewer = mount runtime noTools reviewerDef
  -- runWorldLocal interprets World, discharging the constraint that
  -- researchTools carried.
  report <- runEff . runWorldLocal "." $ workflow researcher reviewer
  TIO.putStrLn report
```

`model` returns a `ModelRuntime`: one configured model, its provider, name,
and default parameters, as an ordinary value. Mount the same runtime on
several agents, wrap it in middleware (below), or swap it for `mockModel` in
tests.

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

Each script restarts for every invocation, so independent agent calls stay
deterministic. The workflow under test is the shipped code.

### Record a session

`Journal` is LLMonad's session log effect: model turns and tool calls,
recorded as they happen. The `journaling` middleware observes an agent's
model traffic; interpret the `Journal` effect with `runJournalFile
"session.jsonl"` and every prompt, tool call, tool result, and model turn
persists to that file:

```haskell
reply <-
  runEff . runJournalFile "session.jsonl" . runWorldLocal "." $
    withRecordedTurn "turn-1" $
      invoke
        (mount (applyMiddleware journaling runtime) (tools [viewFileTool]) readerDef)
        "Summarize README.md."
```

`withRecordedTurn` brackets the turn, so even a run that dies mid-flight
leaves an audit-valid session behind. `replayAudit events` verifies any such
recording before you trust it as a replay fixture.

### Replay a recording as a regression test

```haskell
events <- runEff (resumeSession "session.jsonl")
let script = extractReplayScript events
-- The replay runtime and toolset play back recorded model turns and tool
-- results, in order. Anything unrecorded raises ReplayDivergence instead
-- of improvising.
runtime <- strictReplayRuntime script
replayResearchTools <- strictReplayToolset script researchTools
report <-
  runEff . fmap fst . runWorldMemoryWithFiles [] $
    workflow
      (mount runtime replayResearchTools researcherDef)
      (mount runtime noTools reviewerDef)
```

The recording is session-wide: both agents draw from one continuous stream.
Adding another `invoke` past the recording raises `ReplayDivergence` naming
the offending turn's ordinal — and tool drift (a different call next, or the
same call with drifted arguments) names the tool — instead of shifting every
later answer.

## Run agents against real models

Live models need budgets. The defaults hold for short loops; when an agent
researches across files, raise `agentMaxRounds` and say the cap out loud in
the system prompt:

```haskell
let opts = defaultAgentOpts{agentMaxRounds = 24}
    def = withAgentOpts opts (textAgent "Investigate at most five tool calls." id)
```

Custom tools should feed failures back instead of aborting the loop --
catch expected domain errors and return them as tool-error strings so the
model can adapt:

```haskell
tool' "repo_search" "Search project lines." $ \args ->
    fmap (either (Left . prettyWorldError) Right) $
      Ex.try @WorldError (searchFiles ...)
```

Two more operational notes: SearchFiles include\/exclude patterns carry
glob syntax (`*.hs`, `src/*.md`) and match the whole relative path -- bare
extensions like `.hs` still work as substrings. And `Journal` files are
append-only logs; use `runJournalFileTruncate` when you want a recording
that contains exactly one run.

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
