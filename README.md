# LLMonad: Coding Agent Runtime & Interactive TUI

An Effect-native autonomous coding agent runtime, execution sandboxing system, and interactive Terminal User Interface (TUI) built in Haskell on top of `effectful`, `brick`, and `vty`.

```
+-----------------------------------------------------------------------------------------+
|                                    LLMonad Runtime                                      |
+-----------------------------------------------------------------------------------------+
|                                                                                         |
|   +--------------------------+                         +----------------------------+   |
|   |   Brick + Vty TUI App    | <=== Custom Events ===> |   ReAct Agent Controller   |   |
|   |  - Streaming Chat Window |      (Tokens, Logs,     |  - Tool Call Orchestrator  |   |
|   |  - Unified Diff Renderer |       Diff Updates)     |  - Structured Extraction   |   |
|   |  - Tool Execution Log    |                         |  - Step Budget Tracker     |   |
|   |  - Interactive Prompt    |                         +----------------------------+   |
|   +--------------------------+                                        |                 |
|                |                                                      |                 |
|                v                                                      v                 |
|   +--------------------------+                         +----------------------------+   |
|   |  LLMonad.Journal Effect  |                         |    LLMonad.World Effect    |   |
|   |  - JSONL File Storage    |                         |  - Local Workspace I/O     |   |
|   |  - In-Memory Journal     |                         |  - Ephemeral Git Worktrees |   |
|   |  - Session Resume        |                         |  - Pure Virtual Memory FS  |   |
|   |  - Audit Replay Engine   |                         |  - Process & Shell Runner  |   |
|   +--------------------------+                         +----------------------------+   |
|                                                                       |                 |
|                                        +------------------------------+                 |
|                                        |                              |                 |
|                                        v                              v                 |
|                       +--------------------------------+ +--------------------------+   |
|                       |      Standard Coding Tools     | |   Subagent Delegation    |   |
|                       |  - viewFile   (Line Slicing)   | |  - Green-Thread Workers  |   |
|                       |  - editFile   (Safe Patching)  | |  - Worktree Sandboxing   |   |
|                       |  - grepSearch (Pattern Search) | |  - Tool Whitelisting     |   |
|                       |  - findByName (Glob Search)    | |  - Step Budget Limits    |   |
|                       |  - listDir    (Directory Tree) | |  - Anti-Fork-Bomb Guard  |   |
|                       |  - runCommand (Subprocesses)   | +--------------------------+   |
|                       +--------------------------------+                                |
+-----------------------------------------------------------------------------------------+
```

---

## Key Features

1. **Effect-Native Execution (`LLMonad.World`)**: All filesystem operations and process executions are mediated through an extensible `World` effect. Run natively in a local directory (`runWorldLocal`), in an ephemeral isolated Git worktree (`runWorldWorktree`), or in a pure in-memory virtual filesystem (`runWorldMemory`).
2. **Session Event Sourcing (`LLMonad.Journal`)**: Full session event sourcing capturing turn lifecycles, user messages, assistant turns, tool calls, tool results, and token metrics. Persist to line-delimited JSONL (`runJournalFile`), resume crashed sessions (`resumeSession`), or verify integrity via audit replay (`replayAudit`).
3. **Standard Coding Toolset (`LLMonad.Tools.Coding`)**: Production-ready, type-safe tools with schema generation and boundary protections (`viewFile`, `editFile`, `grepSearch`, `findByName`, `listDir`, `runCommand`).
4. **Isolated Subagent Delegation (`LLMonad.Subagent`)**: Spawn lightweight green-thread child agents (`subagentTool`) inside sandboxed Git worktrees with role-based tool restrictions and strict step budgets.
5. **Interactive Brick + Vty TUI (`llmonad-tui`)**: Terminal interface with real-time response token streaming, live unified diff visualization, tool execution logging, focus management, and session resume.
6. **Robust 5-Tier Test Architecture**: 461 automated tests verifying feature coverage, boundary conditions, cross-module interactions, real-world workloads, and adversarial stress scenarios with 100% offline determinism.

---

## Quickstart & Installation

### Prerequisites
- **GHC**: 9.6, 9.8, or 9.10
- **Cabal**: 3.10+
- **Git**: 2.30+ (for Git worktree sandboxing)

### Building the Project
```bash
# Build the library, executables, and test suite with strict warnings
cabal build all --ghc-options="-Wall -Werror"
```

### Running the Test Suite
```bash
# Execute the full 461-example offline test suite
cabal test -j1
```

### Launching the Interactive TUI
```bash
# Launch interactive TUI in the current workspace
cabal run llmonad-tui

# Launch with explicit workspace, model, and system prompt
cabal run llmonad-tui -- --workspace /path/to/project --model deepseek-chat --system "You are an expert Haskell engineer."

# Resume a previous session from a JSONL journal
cabal run llmonad-tui -- --workspace /path/to/project --resume ./session.jsonl
```

### Command Line Interface (CLI) Flags
| Flag | Description | Default |
|---|---|---|
| `-w`, `--workspace PATH` | Workspace root directory path | `.` (Current Working Directory) |
| `-m`, `--model MODEL` | Model identifier string | `deepseek-chat` |
| `-s`, `--system PROMPT` | Initial system instruction prompt | None |
| `-r`, `--resume FILE` | Resume session from persisted JSONL journal file | None |
| `-h`, `--help` | Display CLI usage information and exit | — |

---

## R1: Execution World Effect (`LLMonad.World`)

The `World` effect encapsulates all filesystem I/O and process execution into a dynamic effect, decoupling agent logic from the operating system.

### Effect Definition
```haskell
data World :: Effect where
  ReadFileText       :: FilePath -> World m Text
  ReadFileSlice      :: FilePath -> Maybe Int -> Maybe Int -> World m Text
  WriteFileText      :: FilePath -> Text -> World m ()
  DeleteFile         :: FilePath -> World m ()
  CreateDirectory    :: FilePath -> Bool -> World m ()
  ListDirectory      :: FilePath -> World m [DirEntry]
  SearchFiles        :: SearchOptions -> World m [SearchMatch]
  FindFiles          :: FindOptions -> World m [FilePath]
  DoesPathExist      :: FilePath -> World m Bool
  DoesFileExist      :: FilePath -> World m Bool
  DoesDirectoryExist :: FilePath -> World m Bool
  GetWorkspaceRoot   :: World m FilePath
  RunCommand         :: CommandSpec -> World m ProcessResult
```

### Interpreters

#### 1. Local Workspace Interpreter (`runWorldLocal`)
Executes real filesystem operations and subprocesses rooted at a designated base directory.
- **Path Traversal Containment**: Strictly validates all paths against the canonical workspace root. Rejects relative traversal (`..`), absolute paths escaping the root, and symlinks resolving outside the root with `WorldPathOutsideWorkspace`.
- **Concurrent Process Supervision**: Runs stdin writing and stdout/stderr reading concurrently via async streams to prevent OS pipe deadlocks (>64KB). Enforces process group timeouts and calls `waitForProcess` to reap child processes and prevent zombie processes.
```haskell
runWorldLocal :: IOE :> es => FilePath -> Eff (World : es) a -> Eff es a
```

#### 2. Ephemeral Git Worktree Interpreter (`runWorldWorktree`)
Creates an isolated Git worktree on an ephemeral branch (`agent-worktree-<timestamp>`), executes the computation within the workspace sandbox, computes unified diffs, and automatically deletes the worktree on completion or failure.
```haskell
runWorldWorktree :: (IOE :> es, World :> es) => FilePath -> Text -> Eff (World : es) a -> Eff es (Either Text a)

runWorldWorktreeWith :: (IOE :> es, World :> es) => WorktreeConfig -> Eff (World : es) a -> Eff es (Either WorldError (a, WorktreeSummary))
```

#### 3. In-Memory Virtual Filesystem Interpreter (`runWorldMemory`)
Runs entirely in pure memory using a virtual tree structure. Ideal for deterministic unit testing without disk I/O.
```haskell
runWorldMemory :: Eff (World : es) a -> Eff es a
runWorldMemoryState :: MemoryFSState -> Eff (World : es) a -> Eff es (a, MemoryFSState)
```

---

## R2: Session Journal & Persistence Effect (`LLMonad.Journal`)

The `Journal` effect provides event sourcing for agent conversations, enabling persistence, live status tracking, crash recovery, and offline audit verification.

### Event Model
```haskell
data JournalEvent
  = TurnStarted !Text
  | UserMsg !Text
  | ModelTurn !Text
  | ToolInvoked !Text !Value
  | ToolCompleted !Text !ToolResult
  | MetricsReported !ModelMetrics
  | TurnFinished !Text
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
```

### Interpreters & Capabilities

#### 1. JSONL File Persistence (`runJournalFile`)
Appends events to a line-delimited JSONL file as they occur.
```haskell
runJournalFile :: (IOE :> es, World :> es) => FilePath -> Eff (Journal : es) a -> Eff es a
```

#### 2. In-Memory Journal (`runJournalMemory`)
Captures events in memory for testing and subagent introspection.
```haskell
runJournalMemory :: Eff (Journal : es) a -> Eff es (a, [JournalEvent])
```

#### 3. Session Resume (`resumeSession`)
Loads persisted JSONL events and reconstructs the full `[ChatMessage]` conversation history.
```haskell
resumeSession :: (IOE :> es, World :> es) => FilePath -> Eff es [JournalEvent]
reconstructChatHistory :: [JournalEvent] -> [ChatMessage]
```

#### 4. Audit Replay (`replayAudit`)
Validates that journal events form a valid, non-corrupted sequence (verifies turn start/finish matching, tool call associations, and aggregate token counts).
```haskell
replayAudit :: [JournalEvent] -> Either Text ReplaySummary
```

---

## R3: Standard Coding Tools & Subagent Orchestration

### Standard Coding Tools (`LLMonad.Tools.Coding`)

LLMonad provides six standard coding tools backed by the `World` effect:

| Tool | Purpose | Key Parameters | Boundary Protection |
|---|---|---|---|
| `viewFile` | Read entire file or 1-indexed line window | `filePath`, `startLine`, `endLine`, `contentOffset` | Byte truncation (46,080B), accurate line range reporting (`vfrEndLine`), automatic window clamping |
| `editFile` | Replace code snippets in files | `targetFile`, `targetContent`, `replacementContent`, `startLine`, `endLine`, `allowMultiple` | Ambiguity rejection if multiple matches occur without `allowMultiple = true`, automatic unified diff calculation |
| `grepSearch` | Search file contents by pattern | `query`, `searchPath`, `caseInsensitive`, `isRegex`, `includes` | Path filtering, regex safety, line numbering |
| `findByName` | Search filesystem hierarchy | `pattern`, `searchDirectory`, `type`, `maxDepth`, `excludes` | Depth limiting, glob matching, symlink loop avoidance |
| `listDir` | Inspect directory contents | `directoryPath`, `recursive`, `maxDepth` | Recursive tree discovery, depth bounding, metadata reporting (is directory, file size, child count) |
| `runCommand` | Run shell commands and subprocesses | `commandLine`, `cwd`, `timeoutMs` | Default bounded timeout (30,000ms), pipe deadlock prevention, process group termination and child reaping |

### Subagent Delegation (`LLMonad.Subagent`)

Child agents can be spawned on lightweight Haskell green threads using `async`:
- **Worktree Sandboxing**: Runs child work inside an ephemeral Git worktree sandbox to prevent corrupting the parent workspace.
- **Role-Based Whitelisting**: Limits child capabilities (e.g. `subagentRole = "explorer"` restricts the child to read-only tools).
- **Step Budget Enforcement**: Restricts maximum agent rounds (`maxRounds`) to prevent runaway recursive execution.
- **Anti-Fork-Bomb Guard**: Automatically removes `subagentTool` from child toolsets to prohibit unbounded nested subagent spawning.

```haskell
subagentTool :: (World :> es, Journal :> es, LLM :> es, IOE :> es) => [Tool (Eff es)] -> Tool (Eff es)
```

---

## R4: Interactive Terminal User Interface (TUI)

The `llmonad-tui` terminal interface is built with `brick` and `vty`.

```
+-----------------------------------------------------------------------------------------+
| [LLMonad Coding Agent]                   Workspace: /path/to/project | Model: deepseek  |
+-----------------------------------------------------------------------------------------+
| Chat History [FOCUSED]                 | Visual Diff                                    |
|                                        | + import LLMonad.World                         |
| User: Add World effect to Main.hs      | - import System.IO                             |
|                                        | @@ -1,4 +1,4 @@                                |
| Assistant (streaming):                 |------------------------------------------------|
| I will now update Main.hs to use the   | Tool Execution Logs                            |
| World effect interpreter...            | • editFile [SUCCESS]                           |
|                                        |   Args: {"targetFile": "src/Main.hs"}          |
+-----------------------------------------------------------------------------------------+
| Status: [STREAMING] Streaming response...        Turns: 3 | Tokens: 1,420 | Latency: 42ms |
+-----------------------------------------------------------------------------------------+
| Prompt Input                                                                            |
| > refactor error handling in LLMonad.World_                                             |
+-----------------------------------------------------------------------------------------+
|  [Enter] Submit | [Tab] Focus | [Ctrl+P/H/D/L] Direct Focus | [Ctrl+X] Diff | [Ctrl+C] Quit|
+-----------------------------------------------------------------------------------------+
```

### Keyboard Shortcuts Reference

| Shortcut | Action | Scope |
|---|---|---|
| `Enter` | Submit prompt from input editor | Active when `EditorPrompt` is focused |
| `Tab` | Shift focus to next viewport | Global |
| `Shift+Tab` / `BackTab` | Shift focus to previous viewport | Global |
| `Ctrl + P` | Focus **Prompt Input** editor | Global direct jump |
| `Ctrl + H` | Focus **Chat History** viewport | Global direct jump |
| `Ctrl + D` | Focus **Visual Diff** viewport | Global direct jump |
| `Ctrl + L` | Focus **Tool Execution Logs** viewport | Global direct jump |
| `Ctrl + X` | Clear the active Visual Diff | Global |
| `Up` / `Down` | Scroll focused viewport by 1 line | Active when any non-editor viewport is focused |
| `PageUp` / `PageDown` | Scroll focused viewport by 10 lines | Active when any non-editor viewport is focused |
| `Ctrl + C` / `Esc` | Quit the application | Global |

---

## Testing & Verification Architecture

LLMonad includes a 5-Tier test suite implemented with `hspec` and `QuickCheck`. All 461 test cases run 100% offline without external network dependencies.

```
+-----------------------------------------------------------------------+
|                       5-Tier Test Architecture                        |
+-----------------------------------------------------------------------+
|  Tier 1: Feature Coverage (M1-M4)                                     |
|    Happy-path verification of World, Journal, Tools, Subagent, & TUI  |
+-----------------------------------------------------------------------+
|  Tier 2: Boundary & Corner Cases                                      |
|    0-byte files, 20-level dirs, timeouts, JSONL corruption, clamping  |
+-----------------------------------------------------------------------+
|  Tier 3: Cross-Feature Interactions                                   |
|    Subagent + Worktree + Journal, TUI + editFile + Visual Diff        |
+-----------------------------------------------------------------------+
|  Tier 4: Real-World Application Scenarios                             |
|    1. Codebase Inspection & Search                                    |
|    2. Multi-File Refactoring with Diffs                               |
|    3. Isolated Subagent Feature Implementation                        |
|    4. Session Crash & Resume with Audit Replay                        |
|    5. Interactive TUI Prompt & Streaming Workflow                     |
+-----------------------------------------------------------------------+
|  Tier 5: Adversarial Hardening                                        |
|    10,000 micro-token stream, 500-event journal, concurrent worktrees |
+-----------------------------------------------------------------------+
```

### Running Tests
```bash
# Run all 461 examples
export PATH="$HOME/.ghcup/bin:$HOME/.cabal/bin:$PATH"
cabal test -j1
```

---

## Repository Structure

```
.
├── app/
│   ├── Main.hs                   # Standard CLI demo entrypoint
│   └── TUI.hs                    # llmonad-tui interactive Brick executable
├── src/
│   ├── LLMonad.hs                # Curated top-level prelude
│   └── LLMonad/
│       ├── Core.hs               # LLM monad, conversation state, env
│       ├── Types.hs              # Provider-neutral messages and tool specs
│       ├── Schema.hs             # Generic-driven JSON Schema generation
│       ├── Agent.hs              # Multi-turn ReAct agent loop
│       ├── World.hs              # World effect and smart constructors
│       ├── World/
│       │   ├── Types.hs          # ProcessResult, DirEntry, WorldError
│       │   ├── Local.hs          # Real workspace I/O interpreter
│       │   ├── Worktree.hs       # Ephemeral Git worktree interpreter
│       │   └── Memory.hs         # Pure in-memory virtual filesystem
│       ├── Journal.hs            # Journal effect and session constructors
│       ├── Journal/
│       │   ├── Types.hs          # JournalEvent, ReplaySummary, ModelMetrics
│       │   ├── File.hs           # JSONL append-only persistence
│       │   ├── Memory.hs         # In-memory ephemeral event recorder
│       │   └── Replay.hs         # Session resume and audit validator
│       ├── Tools/
│       │   └── Coding.hs         # 6 standard coding tools (view, edit, grep, find, list, run)
│       ├── Subagent.hs           # Concurrent child agent delegation and sandboxing
│       ├── TUI.hs                # Brick + Vty TUI state machine and widgets
│       ├── Streaming.hs          # Real-time token streaming combinators
│       ├── Structured.hs         # Typed structured output extraction
│       ├── Providers/
│       │   ├── OpenAICompatible.hs # OpenAI-compatible Chat Completions
│       │   └── Anthropic.hs      # Native Anthropic Messages API
│       └── Internal/
│           ├── Http.hs           # HTTP transport and connection manager
│           ├── SSE.hs            # Server-Sent Events parser
│           └── Extract.hs         # Lenient JSON extractor
├── test/
│   ├── Spec.hs                   # Master test suite runner
│   └── LLMonad/
│       ├── E2ESpec.hs            # 5-Tier comprehensive E2E suite (60 examples)
│       ├── WorldSpec.hs          # Local, Worktree, Memory tests
│       ├── WorldAdversarialSpec.hs # Pathological World edge cases
│       ├── JournalSpec.hs        # JSONL, resume, and audit replay tests
│       ├── CodingToolsSpec.hs    # Coding tools functionality tests
│       ├── CodingToolsAdversarialSpec.hs # Tools boundary and clamping tests
│       ├── SubagentSpec.hs       # Subagent isolation and budget tests
│       ├── TUISpec.hs            # Brick event loop and navigation tests
│       └── TUIAdversarialSpec.hs # 10k token streaming and layout stress tests
├── llmonad.cabal                 # Cabal package definition
├── PROJECT.md                    # Architecture and feature specification
├── TEST_INFRA.md                 # Test architecture documentation
├── TEST_READY.md                 # Test verification matrix
└── LICENSE                       # MIT License
```

---

## License

LLMonad is open source software licensed under the [MIT License](LICENSE).
