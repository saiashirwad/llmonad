# Project: LLMonad

## Architecture
LLMonad is a type-safe, composable Haskell library and DSL for Large Language Models built on top of `effectful`.

### Core Architectural Layers
1. **Effectful Core Layer**: Dynamic effect `data LLM :: Effect`, operations GADT, interpreters (`runLLMHTTP`, `runLLMMock`), and higher-order middleware (`withCache`, `withTrace`, `withRateLimit`).
2. **Schema & Structured Layer**: Type-driven JSON Schema generation (`HasSchema`) using GHC Generics, self-correcting error recovery extraction loops (`askStructured`).
3. **Curried Functional API Layer**: Concise, curried functions (`ask`, `ask'`) driven by the `AskFunction` typeclass family with full Monadic/Applicative composition.
4. **Autonomous Agent Layer**: Type-safe tool definitions (`tool`, `toolSync`) and autonomous multi-step ReAct agent execution (`runAgent`, `runAgentStructured`) with cycle detection and step bounds.
5. **Prompt & Streaming Layer**: `Prompt` monoid with `IsString`, message constructors, few-shot templating, and SSE token streaming.
6. **Template Haskell Layer**: QuasiQuoter `[prompt| ... |]` with `#{var}` variable interpolation and `makeTool` / `makeToolNamed` splices.
7. **Application & Documentation Layer**: Top-level `LLMonad` module, executable CLI demo `app/Main.hs`, standalone `examples/`, and comprehensive `README.md`.

---

## Feature Inventory
| # | Feature | Description | Milestone | Source |
|---|---------|-------------|-----------|--------|
| F1.1 | `LLM` Dynamic Effect | `data LLM :: Effect` with `Effectful.Dispatch.Dynamic` and smart constructors | M1 | R1 |
| F1.2 | Provider Protocols | OpenAI-compatible (DeepSeek, OpenRouter, Together, Ollama) and Anthropic Messages protocols | M1 | R1 |
| F1.3 | HTTP Interpreter | `runLLMHTTP` executing live requests in `IOE :> es` | M1 | R1 |
| F1.4 | Mock Interpreter | `runLLMMock` pure in-memory test interpreter without API keys | M1 | R1 |
| F1.5 | Higher-Order Middleware | Response caching (`withCache`), tracing (`withTrace`), and rate limiting (`withRateLimit`) | M1 | R1 |
| F1.6 | Cabal Configuration | Updated `llmonad.cabal` with `effectful`, `template-haskell`, `hspec`, `-Wall` flags | M1 | R1 / AC |
| F2.1 | `HasSchema` Typeclass | GHC Generics JSON schema derivation for records, sum types, enums, primitives, containers | M2 | R3 |
| F2.2 | Schema Compliance | OpenAI Structured Outputs and Anthropic tool schema compliance (`additionalProperties: false`, `required`, nullable) | M2 | R3 |
| F2.3 | Structured Extraction | `askStructured` and self-correcting error recovery loop feeding validation errors back to model | M2 | R3 |
| F3.1 | Curried `ask` API | `ask` combinator supporting 0, 1, or multi-argument curried functions (e.g. `summarize = ask "..."`) | M3 | R2 |
| F3.2 | Curried `ask'` API | `ask'` multi-argument prompt templating (e.g. `compareDates = ask' "..."`) | M3 | R2 |
| F3.3 | Monadic & Applicative Composition | Seamless `do` notation and `<*>` parallel composition on `Eff es` | M3 | R2 |
| F4.1 | Type-Safe Tools | `tool` and `toolSync` deriving JSON parameter schemas and marshaling Haskell values | M4 | R4 |
| F4.2 | ReAct Agent Loop | `runAgent` autonomous multi-step execution loop | M4 | R4 |
| F4.3 | Cycle Detection & Bounding | Loop cycle detection for repeated identical tool calls and configurable step bounds | M4 | R4 |
| F5.1 | Prompt Monoid | `Prompt` type as `Monoid` and `IsString` | M5 | R5 |
| F5.2 | Message Algebra | Conversational state tracking and few-shot templating combinators (`fewShot`) | M5 | R5 |
| F5.3 | SSE Streaming | Real-time Server-Sent Events token streaming and parser state machines | M5 | R5 |
| F6.1 | Prompt QuasiQuoter | `[prompt| ... |]` QuasiQuoter supporting `#{var}` variable interpolation at compile-time | M6 | R6 |
| F6.2 | TH Tool Generator | `makeTool` / `makeToolNamed` splices to register Haskell functions as LLM tools | M6 | R6 |
| F7.1 | Top-level LLMonad Module | Re-export clean, unified public API in `LLMonad` | M7 | R1-R6 |
| F7.2 | Main Executable | `app/Main.hs` demonstration application with fallback mock mode | M7 | AC |
| F7.3 | Standalone Examples | Executable examples in `examples/` for currying, structured output, agents, QuasiQuoters | M7 | AC |
| F7.4 | Documentation | Comprehensive `README.md` with exact user API code snippets and zero-warning verification | M7 | AC |
| T1.1 | E2E Test Suite | Comprehensive Hspec test suite in `test/Spec.hs` across Tiers 1-4 (189 tests) | E2E | AC |

---

## Milestones
| # | Name | Scope | Dependencies | Status |
|---|------|-------|-------------|--------|
| E2E | E2E Testing Track | `TEST_INFRA.md`, Opaque-box Hspec test suite (Tiers 1-4), `TEST_READY.md` | none | DONE |
| M1 | Core Effect & Types | `llmonad.cabal`, `LLMonad.Core`, `LLMonad.Types`, `LLMonad.Provider`, `LLMonad.Providers.*`, `LLMonad.Interpreter.*`, `LLMonad.Middleware.*` | none | DONE |
| M2 | Schema & Structured Output | `LLMonad.Schema` (`HasSchema`), `LLMonad.Structured` (`askStructured`, self-correcting retry loop) | M1 | DONE |
| M3 | Curried Functional API | `LLMonad.API` (`AskFunction`, `ToPromptArg`, `ask`, `ask'`), Monadic/Applicative composition | M2 | DONE |
| M4 | Autonomous Tool Agent | `LLMonad.Tools` (`tool`, `toolSync`), `LLMonad.Agent` (`runAgent`, cycle detection, step bounds) | M2 | DONE |
| M5 | Prompt & Streaming | `LLMonad.Prompt` (`Prompt`, `fewShot`), `LLMonad.Streaming`, `LLMonad.Internal.*` | M1 | DONE |
| M6 | Template Haskell | `LLMonad.TH.QuasiQuoter` (`[prompt| ... |]`), `LLMonad.TH` (`makeTool`) | M4, M5 | DONE |
| M7 | Integration & Documentation | `LLMonad` umbrella module, `app/Main.hs`, `examples/`, `README.md`, zero-warning verification | M1, M2, M3, M4, M5, M6 | DONE |
| P2 | Final Verification & Hardening | 189 passing tests, 5 passing examples, zero compiler warnings under `-Wall -Werror`, clean forensic audit | all | DONE |

---

## Code Layout
```
/Users/texoport/code/llmonad/
├── llmonad.cabal
├── cabal.project
├── src/
│   ├── LLMonad.hs
│   ├── LLMonad/
│   │   ├── Core.hs
│   │   ├── Types.hs
│   │   ├── Provider.hs
│   │   ├── Providers/
│   │   │   ├── OpenAICompatible.hs
│   │   │   └── Anthropic.hs
│   │   ├── Interpreter/
│   │   │   ├── HTTP.hs
│   │   │   └── Mock.hs
│   │   ├── Middleware/
│   │   │   ├── Cache.hs
│   │   │   ├── Trace.hs
│   │   │   └── RateLimit.hs
│   │   ├── Schema.hs
│   │   ├── Structured.hs
│   │   ├── API.hs
│   │   ├── Prompt.hs
│   │   ├── Streaming.hs
│   │   ├── Tools.hs
│   │   ├── Agent.hs
│   │   ├── TH.hs
│   │   ├── TH/
│   │   │   └── QuasiQuoter.hs
│   │   └── Internal/
│   │       ├── Http.hs
│   │       ├── SSE.hs
│   │       └── Extract.hs
├── app/
│   └── Main.hs
├── examples/
│   ├── 01_CurriedAPI.hs
│   ├── 02_StructuredOutput.hs
│   ├── 03_AgentWithTools.hs
│   ├── 04_QuasiQuotes.hs
│   └── 05_EffectfulHandlers.hs
├── test/
│   ├── Spec.hs
│   └── LLMonad/
│       ├── CoreSpec.hs
│       ├── MockSpec.hs
│       ├── SchemaSpec.hs
│       ├── StructuredSpec.hs
│       ├── CurriedAPISpec.hs
│       ├── AgentSpec.hs
│       ├── PromptSpec.hs
│       ├── StreamingSpec.hs
│       ├── THSpec.hs
│       ├── SSESpec.hs
│       ├── OpenAICompatSpec.hs
│       ├── AnthropicSpec.hs
│       ├── ExtractSpec.hs
│       ├── ChallengerSpec.hs
│       ├── E2ETier3Spec.hs
│       └── E2ETier4Spec.hs
└── README.md
```
