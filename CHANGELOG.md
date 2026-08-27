# Revision history for llmonad

## Unreleased

**Added**

- Session recording that actually records: new `journaling` middleware
  (`LLMonad.Middleware.Journaling`) turns every observed model round --
  prompts, requested tool calls, tool results, model turns -- into `Journal`
  events, and `withRecordedTurn` brackets one audit-valid turn, even when the
  run aborts mid-flight. Until now, installing a journal interpreter around a
  workflow wrote an empty file unless every event was hand-recorded.

**Breaking**

- `ask'` is removed as promised in 0.3.0.0: it had byte-identical behavior to
  `ask`, so multi-parameter templates use `ask` directly.
- `defaultAgentOpts` raises `agentMaxRounds` from 8 to 16: live multi-tool
  research legitimately spends 10-20 rounds, and eight exhausted real runs
  mid-investigation. Override with `withAgentOpts` where shorter caps matter.

## 0.3.0.0 -- 2026-08-24

**Breaking**

- `bind` is renamed to `mount`: "attach a model and toolset to a
  definition" no longer shares its name with monadic bind, which the
  `Monad (Agent)` instance also uses with different meaning. Mechanical
  rename at all call sites; no behavior change.
- `mount` is restructured internally into named stages (`checkedToolset`,
  `rejected`, `wired`, `seedSession`, and a private `runLoopFor` that maps a
  definition's output kind onto its conversation loop). Same eagerness as
  before: duplicate tool names are detected at mount time; only the error
  itself waits for the first agent call.

## 0.2.0.0 -- 2026-08-21

A ground-up rebuild of the toy 0.1 prototype into a proper DSL.

**Added**

- Typed structured output: `ask @MyType "..."` derives a real JSON Schema
  from your type (`ToSchema`, generic derivation), has it enforced
  server-side where possible, and returns a decoded Haskell value.
- Provider abstraction (`Provider` as a record of functions) with two
  transports:
  - `LLMonad.Providers.OpenAICompatible`: OpenAI, Groq, DeepSeek, Mistral,
    Together, OpenRouter, Ollama, LM Studio — with an automatic
    structured-output downgrade ladder (strict `json_schema` → non-strict →
    `json_object` → prompt-only).
  - `LLMonad.Providers.Anthropic`: native Messages API, structured output
    via forced tool-use.
- Conversation memory: calls inside an `LLM` block share history;
  `runLLMConversation` persists/resumes conversations.
- Tool calling with typed tools (`mkTool`) and a ready-made agent loop
  (`useTools`).
- Token streaming over SSE for both protocols.
- Composable error handling: a typed `LLMError` hierarchy (no more `error`
  calls), `attempt`, and `retry` with exponential backoff.
- Tracing hooks (`withTrace`) and token usage reporting.
- A real test suite (schema derivation, SSE parsing, JSON extraction,
  request/response shapes, agent loop, retries) running against scripted
  mock providers — no network needed.

**Added**

- Glob support in SearchFiles include/exclude patterns: entries carrying
  `*` or `?` anchor-match whole relative paths (so `*.hs` finally behaves),
  while metachar-free entries keep substring semantics. Hidden-directory
  skipping (`.git`, `dist-newstyle`, `.agents`) is now uniform across the
  local and memory World backends via shared predicates in
  `LLMonad.World.Match`.
- `runJournalFileTruncate`: per-run capture on append-only journal files --
  truncates at startup so the returned events contain exactly this run.

**Changed**

- `ask` now takes just an instruction; interpolate inputs with `embed`.
  The old two-input `ask'` is gone — use `<>`.
- `Schema` renamed to `ToSchema` and now produces actual JSON Schema
  documents instead of fake example values.
- Project layout moved from `lib/` to `src/`.

**Removed**

- The hardcoded Groq-only client.

## 0.1.0.0 -- 2025

* First version. Released on an unsuspecting world.
