{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module LLMonad.E2ETier3Spec (spec) where

import Data.Aeson (FromJSON, object, (.=))
import Data.IORef
import Data.Text (Text)
import Effectful
import GHC.Generics (Generic)
import LLMonad
import Test.Hspec

data SentimentResult = SentimentResult
    { sentiment :: Text
    , confidence :: Double
    , keywords :: [Text]
    }
    deriving (Show, Eq, Generic, FromJSON, ToSchema)

data FetchQuery = FetchQuery
    { entityId :: Text
    }
    deriving (Show, Eq, Generic, FromJSON, ToSchema)

data TransformQuery = TransformQuery
    { rawData :: Text
    }
    deriving (Show, Eq, Generic, FromJSON, ToSchema)

runTier3Script ::
    [Either LLMError CompletionResponse] ->
    Eff '[LLM, IOE] x ->
    IO (x, [ChatMessage], [CompletionRequest])
runTier3Script script act = do
    (x, reqs, hist, _) <- runEff (runLLMMockFull script act)
    pure (x, hist, reqs)

spec :: Spec
spec = do
    describe "Tier 3: Cross-Feature Interactions & Combinations" $ do
        it "combines embed interpolation with structured ask decoding" $ do
            let inputData = object ["user" .= ("Alice" :: Text), "score" .= (95 :: Int)]
                promptText = "Evaluate the following user data: " <> embed inputData
                script = [Right (textResp "{\"sentiment\": \"Positive\", \"confidence\": 0.95, \"keywords\": [\"score\", \"high\"]}")]
            (res, hist, reqs) <- runTier3Script script (ask @SentimentResult promptText)
            res `shouldBe` SentimentResult "Positive" 0.95 ["score", "high"]
            case reqs of
                (r : _) -> crMessages r `shouldBe` [UserMsg ("Evaluate the following user data: {\"score\":95,\"user\":\"Alice\"}")]
                [] -> expectationFailure "expected at least 1 request"
            hist
                `shouldBe` [ UserMsg ("Evaluate the following user data: {\"score\":95,\"user\":\"Alice\"}")
                           , AssistantMsg "{\"sentiment\": \"Positive\", \"confidence\": 0.95, \"keywords\": [\"score\", \"high\"]}" []
                           ]

        it "combines streaming with conversational memory across multi-turn exchanges" $ do
            chunksRef <- newIORef []
            let script = [Right (textResp "Part 1. Part 2."), Right (textResp "Second answer")]
                workflow = do
                    streamed <- streamText (\t -> modifyIORef' chunksRef (t :)) "First question"
                    nextReply <- generateText ("Following up on: " <> streamed)
                    pure (streamed, nextReply)
            ((s, r), hist, reqs) <- runTier3Script script workflow
            s `shouldBe` "Part 1. Part 2."
            r `shouldBe` "Second answer"
            hist
                `shouldBe` [ UserMsg "First question"
                           , AssistantMsg "Part 1. Part 2." []
                           , UserMsg "Following up on: Part 1. Part 2."
                           , AssistantMsg "Second answer" []
                           ]
            length reqs `shouldBe` 2

        it "combines useTools with tracing hooks capturing tool execution events" $ do
            tracesRef <- newIORef []
            let fetchTool = mkTool "fetch" "Fetch entity info" $ \(q :: FetchQuery) -> pure ("Data for " <> entityId q)
                script =
                    [ Right (toolResp [ToolCall "c1" "fetch" (object ["entityId" .= ("E101" :: Text)])])
                    , Right (textResp "Entity data resolved")
                    ]
                act = withTrace (\t -> modifyIORef' tracesRef (t :)) (useTools [fetchTool] "Get E101")
            (ans, _, _) <- runTier3Script script act
            ans `shouldBe` "Entity data resolved"
            traces <- readIORef tracesRef
            traces `shouldSatisfy` any (\case TraceToolExecuted{trToolName = "fetch", trToolOk = True} -> True; _ -> False)

        it "combines retry error recovery with multi-step agent loop" $ do
            let fetchTool = mkTool "fetch" "Fetch entity info" $ \(q :: FetchQuery) -> pure ("Data for " <> entityId q)
                script =
                    [ Left (ApiError 500 "Transient LLM outage")
                    , Right (toolResp [ToolCall "c1" "fetch" (object ["entityId" .= ("E202" :: Text)])])
                    , Right (textResp "Final answer after transient recovery")
                    ]
            (ans, _, reqs) <- runTier3Script script (retry 3 (useTools [fetchTool] "Get E202"))
            ans `shouldBe` "Final answer after transient recovery"
            length reqs `shouldBe` 3

        it "combines multiple chained tools passing outputs between pipeline stages" $ do
            let fetchTool = mkTool "fetch" "Fetch raw entity" $ \(q :: FetchQuery) -> pure ("RAW-" <> entityId q)
                transformTool = mkTool "transform" "Transform raw entity" $ \(t :: TransformQuery) -> pure ("PROCESSED-" <> rawData t)
                script =
                    [ Right (toolResp [ToolCall "c1" "fetch" (object ["entityId" .= ("123" :: Text)])])
                    , Right (toolResp [ToolCall "c2" "transform" (object ["rawData" .= ("RAW-123" :: Text)])])
                    , Right (textResp "Pipeline finished: PROCESSED-RAW-123")
                    ]
            (ans, hist, _) <- runTier3Script script (useTools [fetchTool, transformTool] "Process 123")
            ans `shouldBe` "Pipeline finished: PROCESSED-RAW-123"
            hist `shouldSatisfy` any (\case ToolMsg _ "\"RAW-123\"" -> True; _ -> False)
            hist `shouldSatisfy` any (\case ToolMsg _ "\"PROCESSED-RAW-123\"" -> True; _ -> False)

        it "tolerates markdown fences and prose surrounding structured JSON in ask" $ do
            let messyResponse = "Certainly! Here is the JSON output you requested:\n```json\n{\"sentiment\":\"Neutral\",\"confidence\":0.5,\"keywords\":[\"average\"]}\n```\nLet me know if you need more analysis."
                script = [Right (textResp messyResponse)]
            (res, _, _) <- runTier3Script script (ask @SentimentResult "Analyze score")
            res `shouldBe` SentimentResult "Neutral" 0.5 ["average"]
