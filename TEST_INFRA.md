# LLMonad Test Infrastructure & Coverage Strategy

## 1. Architectural Overview & Test Philosophy

LLMonad is a type-safe, composable Haskell DSL for Large Language Models built on `effectful`.
This test infrastructure provides an opaque-box, deterministic, offline verification suite.

### 1.1 Core Principles
1. **Zero External Network Dependencies**: All unit and end-to-end tests execute offline against pure mock interpreters (`runLLMMock`). No API keys or network sockets are required during test execution.
2. **Deterministic Oracle Derivations**: Expected test outputs derive from rigorous mathematical specifications, JSON Schema standards (Draft 2020-12 / OpenAI Structured Outputs / Anthropic Tools), and algebraic laws (Monoid, Applicative, Monad).
3. **Multi-Tiered Verification**: Tests are structured in four hierarchical tiers ensuring baseline correctness, edge-case resilience, multi-component composition, and end-to-end application workflow stability.
4. **Zero-Warning Compliance**: All test modules compile under GHC 9.6+ with `-Wall`, `-Wcompat`, `-Wincomplete-record-updates`, and `-Wincomplete-uni-patterns`.

---

## 2. Four-Tier Coverage Strategy

### Tier 1: Feature Coverage (>= 5 Tests Per Feature)
Verifies the happy-path behavior of every feature declared in `PROJECT.md` Feature Inventory:
- **F1.1 Dynamic Effect Core**: Effect creation, dispatch, history state operations (`getHistory`, `setHistory`, `pushMessage`, `clearHistory`), system prompt management (`getSystem`, `setSystem`, `clearSystem`).
- **F1.2 Provider Protocols**: OpenAI-compatible serialization, Anthropic Messages serialization, role mapping, tool call payload encoding.
- **F1.3 HTTP Interpreter**: Request translation, header injection, TLS client configuration, response parsing.
- **F1.4 Mock Interpreter**: Scripted response queue head-popping, request capturing, multi-turn history accumulation, out-of-order execution prevention.
- **F1.5 Middleware**: Response caching (`withCache`), observability tracing (`withTrace`), token rate-limiting (`withRateLimit`).
- **F2.1 `HasSchema` Derivation**: Primitive types (`Int`, `Double`, `Text`, `Bool`), records with field labels, sum types with discriminators, nullary enums, standard containers (`[]`, `Maybe`, `Map`).
- **F2.2 Schema Compliance**: Strict mode enforcement (`additionalProperties: false`), required fields list, nullable union representations (`["type", "null"]`).
- **F2.3 Structured Extraction**: `askStructured`, lenient JSON parsing, markdown fence stripping, self-correcting retry loops with error feedback.
- **F3.1 Curried `ask` API**: 0-arg, 1-arg, and multi-arg curried invocation, return-type driven decoding.
- **F3.2 Curried `ask'` API**: Multi-parameter prompt interpolation with custom argument formatters.
- **F3.3 Composition**: Monadic sequencing (`>>=`, `do`), Applicative parallel composition (`<*>`), error propagation in pipelines.
- **F4.1 Type-Safe Tools**: `tool` (IO callback) and `toolSync` (pure callback), parameter schema generation, Haskell value unmarshaling.
- **F4.2 ReAct Agent Loop**: Autonomous multi-step loop, tool invocation, tool result feeding, final answer termination.
- **F4.3 Cycle Detection & Bounds**: Repeated identical tool call interception, configurable maximum step limit enforcement (`MaxStepsExceeded`).
- **F5.1 Prompt Monoid**: `Semigroup` associativity, `Monoid` identity (`mempty`), `IsString` literal conversion, string concatenation.
- **F5.2 Message Algebra**: Few-shot message builder (`fewShot`), multi-turn role alternating history constructors.
- **F5.3 SSE Streaming**: SSE byte parsing, chunk boundary splitting, carriage-return line feed (`\r\n`) handling, delta aggregation.
- **F6.1 Prompt QuasiQuoter**: `[prompt| ... |]` compile-time expression quasiquoting, `#{var}` variable interpolation.
- **F6.2 TH Tool Generator**: `makeTool` and `makeToolNamed` automatic splice generation from Haskell functions.

### Tier 2: Boundary & Corner Cases (>= 5 Tests Per Feature)
Stresses edge conditions, extremal parameters, and failure modes:
- **Empty & Null Inputs**: Empty prompts, empty system messages, zero-length message histories, empty string tool parameters, empty lists.
- **Extreme & Large Payloads**: Deeply nested JSON records, large token buffers, multi-kilobyte text fragments, special Unicode characters (emoji, zero-width joiners, non-Latin scripts).
- **Malformed & Corrupted Data**: Syntax errors in JSON payloads, missing required fields, type mismatches (string given when integer expected), truncated SSE fragments.
- **Error Recovery & Retries**: Transient HTTP errors (429, 500, 503) triggering retry backoff, permanent HTTP errors (400, 401, 403) failing immediately without retry.
- **Agent Pathologies**: Model generating non-existent tool names, malformed tool arguments, infinite tool loops, immediate empty tool calls.

### Tier 3: Cross-Feature Interactions & Pairwise Combinations
Verifies orthogonal features working in harmony:
- **Schema + Agent (`runAgentStructured`)**: Agent executes intermediate tools to gather data and returns a strongly-typed record adhering to `HasSchema`.
- **Curried API + Middleware (`withCache` / `withTrace`)**: Curried `ask` functions properly hit cache and emit structured trace events.
- **Prompt Monoid + QuasiQuoter (`[prompt| ... |]`)**: QuasiQuoted strings composed seamlessly with `Prompt` monoidal operators (`<>`).
- **Streaming + Rate Limiting**: SSE token streams throttled accurately through rate-limiting middleware.
- **Few-Shot Prompting + Structured Extraction**: `fewShot` message histories prepended to `askStructured` queries.
- **TH Tool Generation + ReAct Agent**: Tools generated via `makeTool` registered into `runAgent` execution contexts.

### Tier 4: Real-World Application Workloads
Simulates complete real-world applications using the pure mock interpreter:
1. **Autonomous Financial Research Agent**: Multi-step tool workflow querying stock tickers, calculating risk metrics via local math tool, and generating a validated financial report.
2. **Structured Invoice & Entity Extraction Pipeline**: Complex document OCR text converted into structured `Invoice` records with nested line items, validation checksums, and automatic repair on malformed numbers.
3. **Multi-Turn Customer Support Conversation**: Stateful session maintaining user identity, handling inquiries, mutating session state, and escalating to human support via tool invocation.
4. **Pairwise Code Review & Synthesis Engine**: Monadic pipeline performing AST static check via tool, querying model for performance review, and combining reviews via Applicative composition.

---

## 3. Test Suite Organization

```
test/
├── Spec.hs                    -- Main Hspec runner executing all modules
└── LLMonad/
    ├── CoreSpec.hs            -- F1.1, F1.5 Dynamic effect, state, middleware
    ├── MockSpec.hs            -- F1.4 Pure mock interpreter & request inspection
    ├── SchemaSpec.hs          -- F2.1, F2.2 Generic JSON Schema derivations
    ├── StructuredSpec.hs      -- F2.3 Structured extraction & retry repair
    ├── CurriedAPISpec.hs      -- F3.1, F3.2, F3.3 Curried ask/ask' & composition
    ├── AgentSpec.hs           -- F4.1, F4.2, F4.3 Tools, ReAct loop, cycle bounds
    ├── PromptSpec.hs          -- F5.1, F5.2 Prompt monoid & message algebra
    ├── StreamingSpec.hs       -- F5.3 SSE parsing & token streaming
    ├── THSpec.hs              -- F6.1, F6.2 QuasiQuoters & TH splices
    ├── OpenAICompatSpec.hs    -- F1.2 OpenAI protocol & tiered downgrade
    ├── AnthropicSpec.hs       -- F1.2 Anthropic Messages protocol & tool formatting
    ├── ExtractSpec.hs         -- F2.3 Lenient JSON extraction & fence stripping
    ├── SSESpec.hs             -- F5.3 SSE state machine low-level tests
    ├── E2ETier3Spec.hs        -- Tier 3 Cross-feature pairwise interactions
    └── E2ETier4Spec.hs        -- Tier 4 End-to-end real-world workload scenarios
```

---

## 4. Test Execution & Verification

To execute the test suite:
```bash
export PATH="$HOME/.ghcup/bin:$HOME/.cabal/bin:$PATH"
cabal test --test-show-details=direct
```

All test cases are verified against 100% pass criteria with pure in-memory mock providers.
