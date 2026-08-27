{-# LANGUAGE OverloadedStrings #-}

module Main where

import LLMonad.AgentSpec qualified
import LLMonad.AnthropicSpec qualified
import LLMonad.Batch1AdversarialSpec qualified
import LLMonad.Batch3AdversarialSpec qualified
import LLMonad.Batch3ChallengerSpec qualified
import LLMonad.Batch3ChallengerStressSpec qualified
import LLMonad.Batch4AdversarialSpec qualified
import LLMonad.Batch4ChallengerSpec qualified
import LLMonad.Batch4ChallengerStressSpec qualified
import LLMonad.Batch5AdversarialSpec qualified
import LLMonad.Batch5ChallengerSpec qualified
import LLMonad.Batch5ChallengerStressSpec qualified
import LLMonad.ChallengerSpec qualified
import LLMonad.CodingToolsSpec qualified
import LLMonad.CompositionSpec qualified
import LLMonad.CoreSpec qualified
import LLMonad.CurriedAPISpec qualified
import LLMonad.E2ETier3Spec qualified
import LLMonad.E2ETier4Spec qualified
import LLMonad.ExtractSpec qualified
import LLMonad.FinalChallengeSpec qualified
import LLMonad.HttpSpec qualified
import LLMonad.JournalAutoRecordSpec qualified
import LLMonad.JournalSpec qualified
import LLMonad.MockSpec qualified
import LLMonad.OpenAICompatSpec qualified
import LLMonad.PromptSpec qualified
import LLMonad.ReplaySpec qualified
import LLMonad.SSESpec qualified
import LLMonad.SchemaSpec qualified
import LLMonad.StreamingSpec qualified
import LLMonad.StrictReplaySpec qualified
import LLMonad.StructuredSpec qualified
import LLMonad.THSpec qualified
import LLMonad.WorldAdversarialSpec qualified
import LLMonad.WorldSpec qualified
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Mock Interpreter (F1.4)" LLMonad.MockSpec.spec
    describe "Core & History (F1.1, F1.5)" LLMonad.CoreSpec.spec
    describe "Curried Functional API (F3.1, F3.2, F3.3)" LLMonad.CurriedAPISpec.spec
    describe "Schema Engine (F2.1, F2.2)" LLMonad.SchemaSpec.spec
    describe "Structured Output & Retry (F2.3)" LLMonad.StructuredSpec.spec
    describe "Prompt & Message Algebra (F5.1, F5.2)" LLMonad.PromptSpec.spec
    describe "Streaming & SSE (F5.3)" LLMonad.StreamingSpec.spec
    describe "Template Haskell & QuasiQuoter (F6.1, F6.2)" LLMonad.THSpec.spec
    describe "Extract (F2.3)" LLMonad.ExtractSpec.spec
    describe "Agent & Tools (F4.1, F4.2, F4.3)" LLMonad.AgentSpec.spec
    describe "Agent Composition" LLMonad.CompositionSpec.spec
    describe "OpenAICompatible Provider (F1.2)" LLMonad.OpenAICompatSpec.spec
    describe "Anthropic Provider (F1.2)" LLMonad.AnthropicSpec.spec
    describe "SSE Parser (F5.3)" LLMonad.SSESpec.spec
    describe "HTTP Transport (F1.2, HTTP-001..008)" LLMonad.HttpSpec.spec
    describe "Tier 3: Pairwise Interactions" LLMonad.E2ETier3Spec.spec
    describe "Tier 4: Real-World Workloads" LLMonad.E2ETier4Spec.spec
    describe "Challenger Adversarial Stress Tests (M1)" LLMonad.ChallengerSpec.spec
    describe "World Effect & Sandboxing (R1)" LLMonad.WorldSpec.spec
    describe "World Effect Adversarial Stress Tests (R1)" LLMonad.WorldAdversarialSpec.spec
    describe "Journal Effect & Persistence (R2)" LLMonad.JournalSpec.spec
    describe "Automatic Session Recording (R2)" LLMonad.JournalAutoRecordSpec.spec
    describe "Journal Replay & Deserialization Fidelity (Batch 5 / R5)" LLMonad.ReplaySpec.spec
    describe "Strict Replay Divergence" LLMonad.StrictReplaySpec.spec
    describe "Standard Coding Tools (R3)" LLMonad.CodingToolsSpec.spec
    describe "Batch 1 Challenger Adversarial Suite" LLMonad.Batch1AdversarialSpec.spec
    describe "Batch 3 Adversarial Suite" LLMonad.Batch3AdversarialSpec.spec
    describe "Batch 3 Challenger Suite" LLMonad.Batch3ChallengerSpec.spec
    describe "Batch 3 Challenger Stress Suite" LLMonad.Batch3ChallengerStressSpec.spec
    describe "Batch 4 Adversarial Suite" LLMonad.Batch4AdversarialSpec.spec
    describe "Batch 4 Challenger Suite" LLMonad.Batch4ChallengerSpec.spec
    describe "Batch 4 Challenger Stress Suite" LLMonad.Batch4ChallengerStressSpec.spec
    describe "Batch 5 Adversarial & Fidelity Suite" LLMonad.Batch5AdversarialSpec.spec
    describe "Batch 5 Empirical Challenger Suite" LLMonad.Batch5ChallengerSpec.spec
    describe "Batch 5 Empirical Challenger Stress Suite" LLMonad.Batch5ChallengerStressSpec.spec
    describe "Final Challenger End-to-End Stress Suite" LLMonad.FinalChallengeSpec.spec
