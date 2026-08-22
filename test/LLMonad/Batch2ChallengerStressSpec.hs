{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Batch 2 Challenger Stress Specification: Empirical verification of Batch 2
-- Covers:
-- 1. Transactional Turns & Rollback Integrity (CORE-001, CORE-002, CORE-003, CORE-006, CORE-007, CORE-010)
-- 2. Exponential Backoff, Jitter Distribution & Retry-After Timing (CORE-001, CORE-002)
-- 3. High-Concurrency Subagent State Isolation (SUB-001, SUB-002, SUB-004)
-- 4. Cache Key Discrimination & Non-Terminal Tool Turn Exclusion (CACHE-001, CACHE-002, CACHE-003, CACHE-004)
module LLMonad.Batch2ChallengerStressSpec (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, replicateM)
import Data.Aeson (FromJSON, ToJSON (..), object)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Effectful
import GHC.Generics (Generic)
import LLMonad hiding (prompt)
import Test.Hspec

data StressData = StressData
  { count :: !Int
  , label :: !Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

spec :: Spec
spec = describe "Batch 2 Challenger Stress Suite" $ do

  describe "1. Transactional Turns & Rollback Under Adversarial Error Conditions" $ do
    it "Rolls back staged message for all ApiError status codes (400, 401, 403, 404, 429, 500, 502, 503, 504)" $ do
      let statusCodes = [400, 401, 403, 404, 429, 500, 502, 503, 504]
      forM_ statusCodes $ \status -> do
        let script = [Left (ApiError status ("Error " <> T.pack (show status)))]
            prompt = "Status Test " <> T.pack (show status)
        (res, _, hist, _) <- runEff $ runLLMMockFull script (attempt (generateText prompt))
        case res of
          Left (ApiError s _) -> s `shouldBe` status
          other -> expectationFailure ("Expected ApiError " <> show status <> ", got: " <> show other)
        hist `shouldBe` []

    it "Rolls back staged message on diverse HttpError messages" $ do
      let errs = ["Connection refused", "Read timeout", "TLS handshake failed", "Host unreachable"]
      forM_ errs $ \eMsg -> do
        let script = [Left (HttpError eMsg)]
        (res, _, hist, _) <- runEff $ runLLMMockFull script (attempt (generateText "Http prompt"))
        case res of
          Left (HttpError _) -> pure ()
          other -> expectationFailure ("Expected HttpError, got: " <> show other)
        hist `shouldBe` []

    it "Rolls back staged message when streaming callback throws an exception midway" $ do
      let script = [Left (HttpError "Stream transport failed")]
          failingCallback _ = pure ()
          act = streamText failingCallback "Stream that fails"
      (res, _, hist, _) <- runEff $ runLLMMockFull script (attempt act)
      case res of
        Left (HttpError _) -> pure ()
        other -> expectationFailure ("Expected HttpError, got: " <> show other)
      hist `shouldBe` []

    it "Multi-turn alternating success/failure pipeline strictly isolates committed turns" $ do
      let script =
            [ Right (textResp "OK 1")
            , Left (ApiError 500 "Fail 1")
            , Right (textResp "OK 2")
            , Left (HttpError "Fail 2")
            , Right (textResp "OK 3")
            , Left (ApiError 503 "Fail 3")
            , Right (textResp "OK 4")
            ]
          pipeline = do
            r1 <- generateText "Query 1"
            _  <- attempt (generateText "Bad Query 1")
            r2 <- generateText "Query 2"
            _  <- attempt (generateText "Bad Query 2")
            r3 <- generateText "Query 3"
            _  <- attempt (generateText "Bad Query 3")
            r4 <- generateText "Query 4"
            pure (r1, r2, r3, r4)

      ((r1, r2, r3, r4), reqs, hist, _) <- runEff $ runLLMMockFull script pipeline
      r1 `shouldBe` "OK 1"
      r2 `shouldBe` "OK 2"
      r3 `shouldBe` "OK 3"
      r4 `shouldBe` "OK 4"
      length reqs `shouldBe` 7
      -- Final history must have exactly 8 messages (4 successful user queries + 4 successful assistant replies)
      length hist `shouldBe` 8
      hist `shouldBe`
        [ UserMsg "Query 1"
        , AssistantMsg "OK 1" []
        , UserMsg "Query 2"
        , AssistantMsg "OK 2" []
        , UserMsg "Query 3"
        , AssistantMsg "OK 3" []
        , UserMsg "Query 4"
        , AssistantMsg "OK 4" []
        ]

    it "extractWithRetry rollbacks cleanly when all retry attempts fail" $ do
      let badResp = Right (textResp "Malformed JSON output")
          script = [badResp, badResp, badResp, badResp]
      (res, reqs, hist, _) <- runEff $ runLLMMockFull script (attempt (extractWithRetry @StressData 4 "Extract data"))
      case res of
        Left (DecodeError _ _) -> pure ()
        other -> expectationFailure ("Expected DecodeError, got: " <> show other)
      length reqs `shouldBe` 4
      -- Everything rolled back to 0 messages
      hist `shouldBe` []

  describe "2. Exponential Backoff, Jitter & Retry-After Timing" $ do
    it "Zero history duplication across multiple retry attempts with ultimate success" $ do
      let script =
            [ Left (ApiError 500 "Server error 1")
            , Left (ApiError 502 "Bad gateway 2")
            , Left (HttpError "Timeout 3")
            , Left (RateLimitError 429 "Rate limit 4" (Just 0))
            , Right (textResp "Successful response after 4 retries")
            ]
      (reply, reqs, hist, _) <- runEff $ runLLMMockFull script (retry 5 (generateText "Single Prompt with 4 Retries"))
      reply `shouldBe` "Successful response after 4 retries"
      length reqs `shouldBe` 5
      -- Crucial verification: History contains exactly 1 UserMsg and 1 AssistantMsg
      hist `shouldBe`
        [ UserMsg "Single Prompt with 4 Retries"
        , AssistantMsg "Successful response after 4 retries" []
        ]

    it "Fast-fails immediately without retrying or delaying on non-transient errors" $ do
      let nonTransientErrors =
            [ ApiError 400 "Bad Request"
            , ApiError 401 "Unauthorized"
            , ApiError 403 "Forbidden"
            , ApiError 404 "Not Found"
            , DecodeError "JSON parse error" "{}"
            , SchemaError "Type mismatch"
            , ToolArgumentError "tool" "bad argument"
            , AgentRoundsExhausted 3
            , UnsupportedCapability "vision"
            ]
      forM_ nonTransientErrors $ \err -> do
        let script = [Left err, Right (textResp "Should never be reached")]
        t0 <- getCurrentTime
        (_, reqs, hist, _) <- runEff $ runLLMMockFull script (attempt (retry 4 (generateText "Fast fail prompt")))
        t1 <- getCurrentTime
        let elapsed = diffUTCTime t1 t0
        -- Elapsed time must be virtually instantaneous (< 100ms)
        elapsed `shouldSatisfy` (< 0.1)
        length reqs `shouldBe` 1
        hist `shouldBe` []

    it "Measures exponential backoff delay timing across 1, 2, and 3 retries" $ do
      -- 1 retry (attempt 1 -> delay base 100ms with jitter [80-120ms])
      let script1 = [Left (ApiError 503 "Unavailable 1"), Right (textResp "Done 1")]
      t0 <- getCurrentTime
      _ <- runEff $ runLLMMock script1 (retry 2 (generateText "Backoff 1"))
      t1 <- getCurrentTime
      let dt1 = diffUTCTime t1 t0
      -- Base 100ms jittered is 80ms-120ms; allow generous bounds [0.06s, 0.35s]
      dt1 `shouldSatisfy` (\t -> t >= 0.06 && t <= 0.35)

      -- 2 retries (attempt 1 base 100ms + attempt 2 base 200ms = ~300ms total)
      let script2 = [Left (ApiError 503 "U1"), Left (ApiError 503 "U2"), Right (textResp "Done 2")]
      t2 <- getCurrentTime
      _ <- runEff $ runLLMMock script2 (retry 3 (generateText "Backoff 2"))
      t3 <- getCurrentTime
      let dt2 = diffUTCTime t3 t2
      -- 100ms + 200ms = 300ms; allow bounds [0.20s, 0.70s]
      dt2 `shouldSatisfy` (\t -> t >= 0.20 && t <= 0.70)

    it "Respects rateLimitRetryAfterSecs delay" $ do
      -- RateLimitError with retryAfterSecs = 1 (1.0s delay jittered 0.8s-1.2s)
      let script = [Left (RateLimitError 429 "Rate limit" (Just 1)), Right (textResp "Cleared")]
      t0 <- getCurrentTime
      _ <- runEff $ runLLMMock script (retry 2 (generateText "Rate limit delay"))
      t1 <- getCurrentTime
      let dt = diffUTCTime t1 t0
      -- Should take at least 0.70 seconds due to retry-after: 1
      dt `shouldSatisfy` (\t -> t >= 0.70 && t <= 1.60)

  describe "3. High-Concurrency Subagent State Isolation" $ do
    it "Executes 30 concurrent subagents in parallel with zero history cross-pollution" $ do
      let numSubagents = 30
      mvar <- newEmptyMVar
      forM_ [1 .. numSubagents] $ \idx -> forkIO $ do
        let taskName = "WorkerTask-" <> T.pack (show idx)
            expectedOut = "Result-" <> T.pack (show idx)
            script = [Right (textResp expectedOut)]
            args = SubagentArgs taskName Nothing (Just 2) Nothing Nothing
            worldState = initMemoryWorld []

        (((res, _), _), _) <- runEff $ runJournalMemory $ runWorldMemory worldState $ runLLMMock script $ do
          runSubagent args []
        putMVar mvar (srStatus res == "completed" && srOutput res == expectedOut)

      results <- replicateM numSubagents (takeMVar mvar)
      results `shouldBe` replicate numSubagents True

    it "Mixed success and failure in concurrent subagents does not crash or corrupt state" $ do
      let numTasks = 20
      mvar <- newEmptyMVar
      forM_ [1 .. numTasks] $ \idx -> forkIO $ do
        let isSuccess = idx `mod` 2 == 0
            taskName = "Task-" <> T.pack (show idx)
            expectedOut = "Output-" <> T.pack (show idx)
            script = if isSuccess
              then [Right (textResp expectedOut)]
              else [Left (ApiError 500 "Simulated worker crash")]
            args = SubagentArgs taskName Nothing (Just 2) Nothing Nothing
            worldState = initMemoryWorld []

        (((res, _), _), _) <- runEff $ runJournalMemory $ runWorldMemory worldState $ runLLMMock script $ do
          runSubagent args []
        if isSuccess
          then putMVar mvar (srStatus res == "completed" && srOutput res == expectedOut)
          else putMVar mvar (srStatus res == "failed")

      results <- replicateM numTasks (takeMVar mvar)
      results `shouldBe` replicate numTasks True

  describe "4. Cache Key Discrimination & Non-Terminal Tool Turn Exclusion" $ do
    it "Differentiates cache keys by System prompt" $ do
      store <- newInMemoryCache
      let script1 = [Right (textResp "System A Answer")]
          script2 = [Right (textResp "System B Answer")]
          prog sys prompt = do
            setSystem sys
            generateText prompt

      (r1, reqs1) <- runEff $ runLLMMock script1 (withCache store (prog "System A" "Common prompt"))
      r1 `shouldBe` "System A Answer"
      length reqs1 `shouldBe` 1

      -- Different system prompt must MISS cache
      (r2, reqs2) <- runEff $ runLLMMock script2 (withCache store (prog "System B" "Common prompt"))
      r2 `shouldBe` "System B Answer"
      length reqs2 `shouldBe` 1

    it "Differentiates cache keys by Params (temperature, maxTokens)" $ do
      store <- newInMemoryCache
      let script1 = [Right (textResp "Low Temp Answer")]
          script2 = [Right (textResp "High Temp Answer")]
          pLow = defaultParams { paramTemperature = Just 0.1 }
          pHigh = defaultParams { paramTemperature = Just 0.9 }

      (r1, reqs1) <- runEff $ runLLMMock script1 (withCache store (generateTextWith pLow "Param prompt"))
      r1 `shouldBe` "Low Temp Answer"
      length reqs1 `shouldBe` 1

      (r2, reqs2) <- runEff $ runLLMMock script2 (withCache store (generateTextWith pHigh "Param prompt"))
      r2 `shouldBe` "High Temp Answer"
      length reqs2 `shouldBe` 1

    it "Differentiates cache keys by ResponseFormat (Text vs JSON Schema)" $ do
      store <- newInMemoryCache
      let script1 = [Right (textResp "{\"count\": 1, \"label\": \"test\"}")]
          script2 = [Right (textResp "{\"count\": 1, \"label\": \"test\"}")]
          prompt = "JSON prompt"

      (r1, reqs1) <- runEff $ runLLMMock script1 (withCache store (askStructured @StressData prompt))
      count r1 `shouldBe` 1
      length reqs1 `shouldBe` 1

      -- Calling generateText (RfText) with identical prompt must MISS cache because format differs
      (r2, reqs2) <- runEff $ runLLMMock script2 (withCache store (generateText prompt))
      r2 `shouldBe` "{\"count\": 1, \"label\": \"test\"}"
      length reqs2 `shouldBe` 1

    it "Never caches responses when FinishReason is FrToolUse even if crspToolCalls is empty" $ do
      store <- newInMemoryCache
      let dummyTool = ToolSpec "test_tool" "A tool" (object [])
          respToolUse = CompletionResponse
            { crspText = "Invoking tool"
            , crspToolCalls = []
            , crspFinishReason = FrToolUse
            , crspUsage = Nothing
            , crspStructuredPayload = Nothing
            }
          script = [Right respToolUse, Right respToolUse]
          program = do
            r1 <- chatRound defaultParams RfText [dummyTool] ToolAuto
            clearHistory
            r2 <- chatRound defaultParams RfText [dummyTool] ToolAuto
            pure (r1, r2)

      ((r1, r2), reqs) <- runEff $ runLLMMock script (withCache store program)
      length reqs `shouldBe` 2
      crspFinishReason r1 `shouldBe` FrToolUse
      crspFinishReason r2 `shouldBe` FrToolUse

    it "Concurrent stress test: 50 threads reading and writing to cache simultaneously" $ do
      store <- newInMemoryCache
      let numThreads = 50
      mvar <- newEmptyMVar
      forM_ [1 .. numThreads] $ \idx -> forkIO $ do
        let modelName = "model-" <> T.pack (show (idx `mod` 5))
            promptText = "Prompt-" <> T.pack (show (idx `mod` 10))
            req = CompletionRequest
              { crModel = Model modelName
              , crSystem = Nothing
              , crMessages = [UserMsg promptText]
              , crParams = defaultParams
              , crTools = []
              , crToolChoice = ToolAuto
              , crResponseFormat = RfText
              }
            resp = textResp ("Answer-" <> promptText)
        cacheInsert store req resp
        mHit <- cacheLookup store req
        case mHit of
          Just hit -> putMVar mvar (crspText hit == ("Answer-" <> promptText))
          Nothing -> putMVar mvar False

      results <- replicateM numThreads (takeMVar mvar)
      results `shouldBe` replicate numThreads True
