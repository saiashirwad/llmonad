{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified LLMonad.AgentSpec
import qualified LLMonad.AnthropicSpec
import qualified LLMonad.ChallengerSpec
import qualified LLMonad.CoreSpec
import qualified LLMonad.CurriedAPISpec
import qualified LLMonad.E2ETier3Spec
import qualified LLMonad.E2ETier4Spec
import qualified LLMonad.ExtractSpec
import qualified LLMonad.HttpSpec
import qualified LLMonad.MockSpec
import qualified LLMonad.OpenAICompatSpec
import qualified LLMonad.PromptSpec
import qualified LLMonad.SchemaSpec
import qualified LLMonad.SSESpec
import qualified LLMonad.StreamingSpec
import qualified LLMonad.StructuredSpec
import qualified LLMonad.THSpec
import qualified LLMonad.CodingToolsSpec
import qualified LLMonad.CodingToolsAdversarialSpec
import qualified LLMonad.JournalSpec
import qualified LLMonad.SubagentSpec
import qualified LLMonad.TUISpec
import qualified LLMonad.TUIAdversarialSpec
import qualified LLMonad.WorldSpec
import qualified LLMonad.WorldAdversarialSpec
import qualified LLMonad.E2ESpec
import qualified LLMonad.Batch1AdversarialSpec
import qualified LLMonad.Batch2AdversarialSpec
import qualified LLMonad.Batch2ChallengerStressSpec
import qualified LLMonad.Batch2ChallengerSpec
import qualified LLMonad.Batch3AdversarialSpec
import qualified LLMonad.Batch3ChallengerSpec
import qualified LLMonad.Batch3ChallengerStressSpec
import qualified LLMonad.Batch4AdversarialSpec
import qualified LLMonad.Batch4ChallengerSpec
import qualified LLMonad.Batch4ChallengerStressSpec
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
  describe "Standard Coding Tools (R3)" LLMonad.CodingToolsSpec.spec
  describe "Subagent Delegation (R3)" LLMonad.SubagentSpec.spec
  describe "Coding Tools & Subagents Adversarial Suite (M3)" LLMonad.CodingToolsAdversarialSpec.spec
  describe "Brick + Vty Interactive TUI (R4 / Milestone 4)" LLMonad.TUISpec.spec
  describe "Brick + Vty Interactive TUI Adversarial Suite (M4)" LLMonad.TUIAdversarialSpec.spec
  describe "E2E Master Suite (Milestone 5 / Tiers 1-5)" LLMonad.E2ESpec.spec
  describe "Batch 1 Challenger Adversarial Suite" LLMonad.Batch1AdversarialSpec.spec
  describe "Batch 2 Adversarial Suite" LLMonad.Batch2AdversarialSpec.spec
  describe "Batch 2 Challenger Stress Suite" LLMonad.Batch2ChallengerStressSpec.spec
  describe "Batch 2 Challenger Adversarial Suite" LLMonad.Batch2ChallengerSpec.spec
  describe "Batch 3 Adversarial Suite" LLMonad.Batch3AdversarialSpec.spec
  describe "Batch 3 Challenger Suite" LLMonad.Batch3ChallengerSpec.spec
  describe "Batch 3 Challenger Stress Suite" LLMonad.Batch3ChallengerStressSpec.spec
  describe "Batch 4 Adversarial Suite" LLMonad.Batch4AdversarialSpec.spec
  describe "Batch 4 Challenger Suite" LLMonad.Batch4ChallengerSpec.spec
  describe "Batch 4 Challenger Stress Suite" LLMonad.Batch4ChallengerStressSpec.spec
