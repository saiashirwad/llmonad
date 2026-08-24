{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module LLMonad.E2ETier4Spec (spec) where

import Data.Aeson (FromJSON, object, (.=))
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import GHC.Generics (Generic)
import LLMonad
import Test.Hspec

-- Workload 1: Financial Types
data StockQuery = StockQuery {ticker :: Text}
    deriving (Show, Eq, Generic, FromJSON, ToSchema)

data RatioArgs = RatioArgs {valA :: Double, valB :: Double}
    deriving (Show, Eq, Generic, FromJSON, ToSchema)

-- Workload 2: Invoice Types
data LineItem = LineItem
    { description :: Text
    , quantity :: Int
    , unitPrice :: Double
    }
    deriving (Show, Eq, Generic, FromJSON, ToSchema)

data Invoice = Invoice
    { vendor :: Text
    , invoiceId :: Text
    , items :: [LineItem]
    , tax :: Double
    , totalAmount :: Double
    }
    deriving (Show, Eq, Generic, FromJSON, ToSchema)

-- Workload 3: Support Ticket Types
data TicketArgs = TicketArgs
    { userId :: Text
    , issueSummary :: Text
    , priority :: Text
    }
    deriving (Show, Eq, Generic, FromJSON, ToSchema)

-- Workload 4: Code Review Metrics
data CodeMetrics = CodeMetrics
    { cyclomaticComplexity :: Int
    , securityVulnerabilities :: Int
    , reviewRecommendation :: Text
    }
    deriving (Show, Eq, Generic, FromJSON, ToSchema)

runTier4Script ::
    [Either LLMError CompletionResponse] ->
    Eff '[LLM, IOE] x ->
    IO (x, [ChatMessage], [CompletionRequest])
runTier4Script script act = do
    (x, reqs, hist, _) <- runEff (runLLMMockFull script act)
    pure (x, hist, reqs)

spec :: Spec
spec = do
    describe "Tier 4: Real-World Application Workloads" $ do
        it "Scenario 1: Autonomous Financial Research Agent with multi-tool chaining" $ do
            let quoteTool = mkTool "get_quote" "Retrieve stock quote" $ \(q :: StockQuery) ->
                    if ticker q == "AAPL"
                        then pure ("Price: 200.0, EPS: 5.0" :: Text)
                        else pure ("Price: 150.0, EPS: 3.0" :: Text)
                ratioTool = mkTool "compute_pe" "Compute P/E ratio" $ \(r :: RatioArgs) ->
                    pure (valA r / valB r)
                script =
                    [ Right (toolResp [ToolCall "c1" "get_quote" (object ["ticker" .= ("AAPL" :: Text)])])
                    , Right (toolResp [ToolCall "c2" "compute_pe" (object ["valA" .= (200.0 :: Double), "valB" .= (5.0 :: Double)])])
                    , Right (textResp "AAPL P/E ratio is 40.0, showing premium valuation.")
                    ]
            (report, hist, _) <-
                runTier4Script script (useTools [quoteTool, ratioTool] "Analyze AAPL valuation metrics")
            report `shouldBe` "AAPL P/E ratio is 40.0, showing premium valuation."
            hist `shouldSatisfy` any (\case ToolMsg _ "\"Price: 200.0, EPS: 5.0\"" -> True; _ -> False)
            hist `shouldSatisfy` any (\case ToolMsg _ content -> "40" `T.isInfixOf` content; _ -> False)

        it "Scenario 2: Structured Invoice & OCR Entity Extraction" $ do
            let rawOCR = "INVOICE #INV-9021\nVendor: Acme Corp\nItem: Haskell Compiler Pro x2 @ 150.0\nItem: Cloud Hosting x1 @ 50.0\nTax: 35.0\nTotal: 385.0"
                mockResponseJSON =
                    "{\n"
                        <> "  \"vendor\": \"Acme Corp\",\n"
                        <> "  \"invoiceId\": \"INV-9021\",\n"
                        <> "  \"items\": [\n"
                        <> "    {\"description\": \"Haskell Compiler Pro\", \"quantity\": 2, \"unitPrice\": 150.0},\n"
                        <> "    {\"description\": \"Cloud Hosting\", \"quantity\": 1, \"unitPrice\": 50.0}\n"
                        <> "  ],\n"
                        <> "  \"tax\": 35.0,\n"
                        <> "  \"totalAmount\": 385.0\n"
                        <> "}"
                script = [Right (textResp mockResponseJSON)]
            (invoice, _, _) <- runTier4Script script (ask @Invoice ("Extract invoice from:\n" <> rawOCR))
            vendor invoice `shouldBe` "Acme Corp"
            invoiceId invoice `shouldBe` "INV-9021"
            length (items invoice) `shouldBe` 2
            totalAmount invoice `shouldBe` 385.0
            tax invoice `shouldBe` 35.0

        it "Scenario 3: Multi-Turn Customer Support Conversation with Escalation" $ do
            ticketRef <- newIORef []
            let escalateTool = mkTool "escalate_ticket" "Escalate to human support" $ \(t :: TicketArgs) -> do
                    liftIO (modifyIORef' ticketRef (t :))
                    pure ("Ticket ESC-1001 created" :: Text)
                script =
                    [ Right (textResp "Hello! I understand you are having login trouble. Have you tried resetting your password?")
                    , Right (textResp "I apologize. Let me verify if your account is locked.")
                    , Right (toolResp [ToolCall "c3" "escalate_ticket" (object ["userId" .= ("U-8821" :: Text), "issueSummary" .= ("Password reset token loop" :: Text), "priority" .= ("HIGH" :: Text)])])
                    , Right (textResp "I have escalated your ticket to priority support (Ticket ESC-1001). An agent will email you shortly.")
                    ]
                supportWorkflow = do
                    r1 <- generateText "I cannot log in to my account."
                    r2 <- generateText "The password reset link says token expired."
                    r3 <- useTools [escalateTool] "User account U-8821 needs escalation for token loop."
                    pure (r1, r2, r3)
            ((r1, r2, r3), hist, _) <- runTier4Script script supportWorkflow
            r1 `shouldBe` "Hello! I understand you are having login trouble. Have you tried resetting your password?"
            r2 `shouldBe` "I apologize. Let me verify if your account is locked."
            r3 `shouldBe` "I have escalated your ticket to priority support (Ticket ESC-1001). An agent will email you shortly."
            tickets <- readIORef ticketRef
            case tickets of
                [t] -> priority t `shouldBe` "HIGH"
                other -> expectationFailure ("Expected 1 ticket, got: " <> show (length other))
            length hist `shouldBe` 8

        it "Scenario 4: Code Review & AST Metric Analysis Pipeline" $ do
            let codeSnippet = "def factorial(n):\n  if n <= 1: return 1\n  return n * factorial(n - 1)"
                script =
                    [ Right (textResp "Code appears clean with simple recursion.")
                    , Right (textResp "{\"cyclomaticComplexity\": 2, \"securityVulnerabilities\": 0, \"reviewRecommendation\": \"APPROVE\"}")
                    ]
                pipeline = do
                    setSystem "You are an automated code review bot."
                    textSummary <- generateText ("Review this function:\n" <> codeSnippet)
                    metrics <- ask @CodeMetrics "Score the cyclomatic complexity and security risks."
                    pure (textSummary, metrics)
            ((summary, metrics), _, reqs) <- runTier4Script script pipeline
            summary `shouldBe` "Code appears clean with simple recursion."
            cyclomaticComplexity metrics `shouldBe` 2
            securityVulnerabilities metrics `shouldBe` 0
            reviewRecommendation metrics `shouldBe` "APPROVE"
            length reqs `shouldBe` 2
            map crSystem reqs `shouldSatisfy` all (== Just "You are an automated code review bot.")
