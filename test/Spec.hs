{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified LLMonad.AgentSpec
import qualified LLMonad.AnthropicSpec
import qualified LLMonad.ChallengerSpec
import qualified LLMonad.CoreSpec
import qualified LLMonad.E2ETier3Spec
import qualified LLMonad.E2ETier4Spec
import qualified LLMonad.ExtractSpec
import qualified LLMonad.MockSpec
import qualified LLMonad.OpenAICompatSpec
import qualified LLMonad.SchemaSpec
import qualified LLMonad.SSESpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Mock Interpreter (F1.4)" LLMonad.MockSpec.spec
  describe "Core & History (F1.1, F1.5)" LLMonad.CoreSpec.spec
  describe "Schema Engine (F2.1, F2.2)" LLMonad.SchemaSpec.spec
  describe "Extract (F2.3)" LLMonad.ExtractSpec.spec
  describe "Agent & Tools (F4.1, F4.2, F4.3)" LLMonad.AgentSpec.spec
  describe "OpenAICompatible Provider (F1.2)" LLMonad.OpenAICompatSpec.spec
  describe "Anthropic Provider (F1.2)" LLMonad.AnthropicSpec.spec
  describe "SSE Parser (F5.3)" LLMonad.SSESpec.spec
  describe "Tier 3: Pairwise Interactions" LLMonad.E2ETier3Spec.spec
  describe "Tier 4: Real-World Workloads" LLMonad.E2ETier4Spec.spec
  describe "Challenger Adversarial Stress Tests (M1)" LLMonad.ChallengerSpec.spec
