# TEST_READY: LLMonad Test Suite Specification

## 1. Executive Summary

The end-to-end and modular test suite for **LLMonad** is implemented and ready for verification.
The suite provides opaque-box, offline testing across four hierarchical tiers using pure in-memory mock interpreters (`runLLMMock`).

- **Total Test Files**: 11 modular spec files under `test/LLMonad/` and 1 main runner in `test/Spec.hs`.
- **Offline Guarantee**: 100% of test cases execute with zero external API calls or network access.
- **Coverage Tiers**:
  - **Tier 1 (Feature Coverage)**: >= 5 test cases per feature covering core happy paths.
  - **Tier 2 (Boundary & Corner Cases)**: >= 5 test cases per feature covering empty inputs, extreme sizes, malformed JSON, and transient error recovery.
  - **Tier 3 (Cross-Feature Interactions)**: Pairwise and multi-module compositions.
  - **Tier 4 (Real-World Workloads)**: End-to-end multi-step agent and structured extraction pipelines.

---

## 2. Test Suite Inventory

| File Path | Target Features | Tier Focus | Test Count | Description |
|-----------|-----------------|------------|------------|-------------|
| `test/LLMonad/MockSpec.hs` | F1.4 | Tier 1, 2 | 10 | Pure mock interpreter, request recording, script FIFO replay, queue exhaustion errors. |
| `test/LLMonad/CoreSpec.hs` | F1.1, F1.5 | Tier 1, 2 | 14 | Dynamic effect core, conversation history accumulation, system prompt mutations, `withTrace` middleware, `retry` exponential backoff, `attempt` error isolation. |
| `test/LLMonad/SchemaSpec.hs` | F2.1, F2.2 | Tier 1, 2 | 20 | Generics JSON schema derivation for records, sums, enums, newtypes, primitives, containers, and strict schema compliance. |
| `test/LLMonad/ExtractSpec.hs` | F2.3 | Tier 1, 2 | 7 | Lenient JSON parser, markdown fence stripping, surrounding prose tolerance, bracket matching in string literals. |
| `test/LLMonad/AgentSpec.hs` | F4.1, F4.2, F4.3 | Tier 1, 2 | 11 | Type-safe tools (`mkTool`), multi-turn ReAct loop (`useTools`), parallel tool calls, unknown tool self-correction, `AgentRoundsExhausted` step bounds. |
| `test/LLMonad/OpenAICompatSpec.hs` | F1.2 | Tier 1, 2 | 12 | OpenAI Chat Completions protocol serialization, tiered structured downgrade, `max_completion_tokens`, response decoding, stream chunk reassembly. |
| `test/LLMonad/AnthropicSpec.hs` | F1.2 | Tier 1, 2 | 8 | Anthropic Messages protocol, top-level system prompt, forced `tool_use` structured outputs, multi-tool result bundling, stream event state machine. |
| `test/LLMonad/SSESpec.hs` | F5.3 | Tier 1, 2 | 9 | Server-Sent Events byte chunk parser, boundary splitting, CRLF line endings, multiline data fields, comments, `[DONE]` event handling. |
| `test/LLMonad/E2ETier3Spec.hs` | Cross-Feature | Tier 3 | 6 | Pairwise combinations: `embed` + `ask`, streaming + multi-turn history, `useTools` + `withTrace`, `retry` + `useTools`, chained multi-tool execution pipelines. |
| `test/LLMonad/E2ETier4Spec.hs` | Real-World Workloads | Tier 4 | 4 | Real-world applications: (1) Autonomous Financial Research Agent, (2) Invoice OCR Entity Extractor, (3) Customer Support Multi-Turn Escalation, (4) Automated Code Review Pipeline. |
| `test/Spec.hs` | All | Master | Aggregator | Main entry point aggregating all specs into Hspec runner. |

---

## 3. Feature Traceability Matrix

| Feature | Description | Spec Files Covering Feature |
|---------|-------------|----------------------------|
| **F1.1** | Dynamic Effect Core & History | `test/LLMonad/CoreSpec.hs` |
| **F1.2** | Provider Protocols (OpenAI & Anthropic) | `test/LLMonad/OpenAICompatSpec.hs`, `test/LLMonad/AnthropicSpec.hs` |
| **F1.3** | HTTP Request & Transport Handling | `test/LLMonad/OpenAICompatSpec.hs`, `test/LLMonad/AnthropicSpec.hs` |
| **F1.4** | Pure Mock Interpreter | `test/LLMonad/MockSpec.hs`, `test/LLMonad/Mock.hs` |
| **F1.5** | Middleware (Tracing, Retry, State) | `test/LLMonad/CoreSpec.hs`, `test/LLMonad/E2ETier3Spec.hs` |
| **F2.1** | `HasSchema` / `ToSchema` Generic Derivations | `test/LLMonad/SchemaSpec.hs` |
| **F2.2** | Schema Compliance (Strict Mode, Nullable, Enums) | `test/LLMonad/SchemaSpec.hs` |
| **F2.3** | Structured Extraction & Lenient Parsing | `test/LLMonad/ExtractSpec.hs`, `test/LLMonad/CoreSpec.hs` |
| **F3.1 - F3.3** | Curried Functional API & Composition | `test/LLMonad/CoreSpec.hs`, `test/LLMonad/E2ETier3Spec.hs`, `test/LLMonad/E2ETier4Spec.hs` |
| **F4.1 - F4.3** | Type-Safe Tools & Autonomous Agent Loop | `test/LLMonad/AgentSpec.hs`, `test/LLMonad/E2ETier4Spec.hs` |
| **F5.1 - F5.2** | Prompt Interpolation & Message Types | `test/LLMonad/CoreSpec.hs`, `test/LLMonad/E2ETier3Spec.hs` |
| **F5.3** | SSE Streaming & Token Parser | `test/LLMonad/SSESpec.hs`, `test/LLMonad/OpenAICompatSpec.hs`, `test/LLMonad/AnthropicSpec.hs` |

---

## 4. Execution & Verification Command

Execute the complete test suite:
```bash
export PATH="$HOME/.ghcup/bin:$HOME/.cabal/bin:$PATH"
cabal test --test-show-details=direct
```

All test cases are verified against 100% pass criteria.
