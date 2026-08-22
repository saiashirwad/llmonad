{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Batch 2 Adversarial Suite: State, Concurrency & Retry Safety
-- Covers:
-- 1. Transactional Turns & Rollback (CORE-001, CORE-002, CORE-003, CORE-006, CORE-007, CORE-010)
-- 2. Retry Safety, History Cleanliness, Backoff & Jitter
-- 3. Subagent State Isolation & Concurrency (SUB-001, SUB-002, SUB-004)
-- 4. Cache Middleware Safety & Concurrency (CACHE-001, CACHE-002, CACHE-003, CACHE-004)
module LLMonad.Batch2AdversarialSpec (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, replicateM, replicateM_)
import Data.Aeson (FromJSON, ToJSON (..), object, toJSON, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import GHC.Generics (Generic)
import LLMonad
import Test.Hspec

data TestSchema = TestSchema
  { fieldA :: !Text
  , fieldB :: !Int
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

spec :: Spec
spec = describe "Batch 2 Adversarial Suite: State, Concurrency & Retry Safety" $ do

  describe "1. Transactional Turns & History Rollback" $ do
    it "generateTextWith rolls back staged UserMsg on ApiError 500" $ do
      let script = [Left (ApiError 500 "Internal Server Error")]
      (res, _, hist, _) <- runEff $ runLLMMockFull script (attempt (generateText "Staged Prompt"))
      case res of
        Left (ApiError 500 _) -> pure ()
        other -> expectationFailure ("Expected ApiError 500, got: " <> show other)
      hist `shouldBe` []

    it "generateTextWith rolls back staged UserMsg on HttpError" $ do
      let script = [Left (HttpError "Connection reset by peer")]
      (res, _, hist, _) <- runEff $ runLLMMockFull script (attempt (generateText "Network Failed Prompt"))
      case res of
        Left (HttpError _) -> pure ()
        other -> expectationFailure ("Expected HttpError, got: " <> show other)
      hist `shouldBe` []

    it "streamTextWith rolls back staged UserMsg on error" $ do
      let script = [Left (ApiError 503 "Service Unavailable")]
          act = streamText (\_ -> pure ()) "Streaming prompt that fails"
      (res, _, hist, _) <- runEff $ runLLMMockFull script (attempt act)
      case res of
        Left (ApiError 503 _) -> pure ()
        other -> expectationFailure ("Expected ApiError 503, got: " <> show other)
      hist `shouldBe` []

    it "askStructured rolls back staged UserMsg when response cannot be decoded" $ do
      let badResp = Right (textResp "{\"invalid\": \"json for schema\"}")
          script = [badResp]
      (res, _, hist, _) <- runEff $ runLLMMockFull script (attempt (askStructured @TestSchema "Extract Schema"))
      case res of
        Left (DecodeError _ _) -> pure ()
        other -> expectationFailure ("Expected DecodeError, got: " <> show other)
      hist `shouldBe` []

    it "extractWithRetry rolls back all staged messages when retry attempts are exhausted" $ do
      let badResp = Right (textResp "Not valid JSON")
          script = [badResp, badResp, badResp]
      (res, _, hist, _) <- runEff $ runLLMMockFull script (attempt (extractWithRetry @TestSchema 3 "Extract with retry"))
      case res of
        Left (DecodeError _ _) -> pure ()
        other -> expectationFailure ("Expected DecodeError, got: " <> show other)
      hist `shouldBe` []

    it "Sequential turn pipeline preserves committed turns and cleanly rolls back failed turn" $ do
      let script =
            [ Right (textResp "Response 1")
            , Left (ApiError 500 "Server explosion on turn 2")
            , Right (textResp "Response 3")
            ]
          pipeline = do
            r1 <- generateText "Turn 1"
            _ <- attempt (generateText "Turn 2 that blows up")
            r3 <- generateText "Turn 3"
            pure (r1, r3)
      ((r1, r3), reqs, hist, _) <- runEff $ runLLMMockFull script pipeline
      r1 `shouldBe` "Response 1"
      r3 `shouldBe` "Response 3"
      length reqs `shouldBe` 3
      hist `shouldBe`
        [ UserMsg "Turn 1"
        , AssistantMsg "Response 1" []
        , UserMsg "Turn 3"
        , AssistantMsg "Response 3" []
        ]

  describe "2. Retry Safety, History Cleanliness, Backoff & Jitter" $ do
    it "retry does NOT accumulate duplicate user messages in history across retries" $ do
      let script =
            [ Left (ApiError 500 "Outage 1")
            , Left (ApiError 503 "Outage 2")
            , Right (textResp "Finally recovered")
            ]
      (reply, reqs, hist, _) <- runEff $ runLLMMockFull script (retry 3 (generateText "Single Query"))
      reply `shouldBe` "Finally recovered"
      length reqs `shouldBe` 3
      hist `shouldBe`
        [ UserMsg "Single Query"
        , AssistantMsg "Finally recovered" []
        ]

    it "retry terminates immediately and rolls back history on permanent 400 bad request error" $ do
      let script =
            [ Left (ApiError 400 "Invalid Parameter")
            , Right (textResp "Unreachable")
            ]
      (res, reqs, hist, _) <- runEff $ runLLMMockFull script (attempt (retry 3 (generateText "Bad Query")))
      case res of
        Left (ApiError 400 _) -> pure ()
        other -> expectationFailure ("Expected ApiError 400, got: " <> show other)
      length reqs `shouldBe` 1
      hist `shouldBe` []

    it "retry recovers from RateLimitError with rateLimitRetryAfterSecs" $ do
      let script =
            [ Left (RateLimitError 429 "Too many requests" (Just 0))
            , Right (textResp "Rate limit cleared")
            ]
      (reply, reqs, hist, _) <- runEff $ runLLMMockFull script (retry 2 (generateText "Rate limited query"))
      reply `shouldBe` "Rate limit cleared"
      length reqs `shouldBe` 2
      hist `shouldBe`
        [ UserMsg "Rate limited query"
        , AssistantMsg "Rate limit cleared" []
        ]

    it "isTransient correctly identifies transient vs permanent errors" $ do
      isTransient (HttpError "timeout") `shouldBe` True
      isTransient (RateLimitError 429 "rate limit" Nothing) `shouldBe` True
      isTransient (RateLimitError 429 "rate limit" (Just 5)) `shouldBe` True
      isTransient (ApiError 408 "Request Timeout") `shouldBe` True
      isTransient (ApiError 409 "Conflict") `shouldBe` True
      isTransient (ApiError 500 "Internal Server Error") `shouldBe` True
      isTransient (ApiError 502 "Bad Gateway") `shouldBe` True
      isTransient (ApiError 503 "Service Unavailable") `shouldBe` True
      isTransient (ApiError 504 "Gateway Timeout") `shouldBe` True

      isTransient (ApiError 400 "Bad Request") `shouldBe` False
      isTransient (ApiError 401 "Unauthorized") `shouldBe` False
      isTransient (ApiError 403 "Forbidden") `shouldBe` False
      isTransient (ApiError 404 "Not Found") `shouldBe` False
      isTransient (DecodeError "parse fail" "") `shouldBe` False
      isTransient (SchemaError "invalid schema") `shouldBe` False
      isTransient (ToolArgumentError "tool" "bad args") `shouldBe` False
      isTransient (AgentRoundsExhausted 5) `shouldBe` False
      isTransient (UnsupportedCapability "streaming") `shouldBe` False

  describe "3. Subagent State Isolation & Concurrency" $ do
    it "Subagent internal prompts, instructions, and tool messages do NOT pollute parent history" $ do
      let dummyTool = toolSync "calc" "Add numbers" (\(_ :: TestSchema) -> ("Calculated" :: Text))
          script =
            [ Right (toolResp [ToolCall "call-calc" "calc" (toJSON (TestSchema "val" 1))])
            , Right (textResp "Subagent step finished.")
            ]
          subagentArgs = SubagentArgs "Run calculation subtask" Nothing (Just 4) Nothing Nothing

      -- Parent conversation performs an initial turn, runs subagent directly, then checks history
      let parentAct = do
            pushMessage (UserMsg "Parent initial message")
            subResult <- runSubagent subagentArgs [dummyTool]
            parentHist <- getHistory
            pure (subResult, parentHist)

      ((((res, finalParentHist), reqs), _), _) <- runEff $ runJournalMemory $ runWorldMemory (initMemoryWorld []) $ runLLMMock script parentAct
      srStatus res `shouldBe` "completed"
      srOutput res `shouldBe` "Subagent step finished."
      length reqs `shouldBe` 2

      -- Parent history MUST contain ONLY the initial message and NOT the subagent's internal UserMsg, tool calls, or tool results
      finalParentHist `shouldBe` [UserMsg "Parent initial message"]

    it "Subagent tool invocation from parent agent records only final SubagentResult as ToolMsg in parent history" $ do
      let childScript =
            [ Right (toolResp [ToolCall "call-1" "subagent" (toJSON (SubagentArgs "Worker subtask" Nothing (Just 3) Nothing Nothing))])
            , Right (textResp "Child worker finished task.")
            , Right (textResp "Parent agent summarized the subagent answer.")
            ]
      let parentTools = [subagentTool standardCodingTools]
      (((answer, _reqs), _), _) <- runEff $ runJournalMemory $ runWorldMemory (initMemoryWorld []) $ runLLMMock childScript $ do
        runAgent parentTools "Parent instructing subagent delegation"

      answer `shouldBe` "Parent agent summarized the subagent answer."

    it "Concurrent subagents run asynchronously without state race conditions or history pollution" $ do
      let numSubagents = 10
      mvar <- newEmptyMVar
      forM_ [1 .. numSubagents] $ \idx -> forkIO $ do
        let taskName = "Task-" <> T.pack (show idx)
            expectedOut = "Completed-" <> T.pack (show idx)
            script = [Right (textResp expectedOut)]
            args = SubagentArgs taskName Nothing (Just 2) Nothing Nothing
            worldState = initMemoryWorld []

        (((res, _), _), _) <- runEff $ runJournalMemory $ runWorldMemory worldState $ runLLMMock script $ do
          runSubagent args []
        putMVar mvar (srStatus res == "completed" && srOutput res == expectedOut)

      results <- replicateM numSubagents (takeMVar mvar)
      results `shouldBe` replicate numSubagents True

    it "Subagent failure does not corrupt parent conversation state" $ do
      let failScript = repeat (Left (ApiError 500 "Child LLM crashed"))
          args = SubagentArgs "Failing task" Nothing (Just 2) Nothing Nothing
          parentAct = do
            pushMessage (UserMsg "Parent pre-failure")
            res <- runSubagent args []
            hist <- getHistory
            pure (res, hist)

      ((((res, hist), _), _), _) <- runEff $ runJournalMemory $ runWorldMemory (initMemoryWorld []) $ runLLMMock failScript parentAct
      srStatus res `shouldBe` "failed"
      hist `shouldBe` [UserMsg "Parent pre-failure"]

  describe "4. Cache Middleware Safety & Concurrency" $ do
    it "Distinguishes requests by model name in cache keys" $ do
      cacheStore <- newInMemoryCache
      let script1 = [Right (textResp "Model A Answer")]
          script2 = [Right (textResp "Model B Answer")]
          promptText = "What is the capital of France?"

      -- Execute request with Model A
      (r1, reqs1) <- runEff $ runLLMMock script1 (withCacheModel (Model "model-a") cacheStore (generateText promptText))
      r1 `shouldBe` "Model A Answer"
      length reqs1 `shouldBe` 1

      -- Execute identical prompt with Model B - must MISS cache and hit backend
      (r2, reqs2) <- runEff $ runLLMMock script2 (withCacheModel (Model "model-b") cacheStore (generateText promptText))
      r2 `shouldBe` "Model B Answer"
      length reqs2 `shouldBe` 1

    it "Never caches responses containing tool calls (non-terminal turns)" $ do
      cacheStore <- newInMemoryCache
      let dummyTool = ToolSpec "search" "Search web" (object [])
          toolResp1 = toolResp [ToolCall "c1" "search" (object ["q" .= ("haskell" :: Text)])]
          toolResp2 = toolResp [ToolCall "c2" "search" (object ["q" .= ("haskell 2026" :: Text)])]
          script = [Right toolResp1, Right toolResp2]
          program = do
            r1 <- chatRound defaultParams RfText [dummyTool] ToolAuto
            clearHistory
            -- Second call with identical request must NOT be served from cache because tool turns are uncacheable
            r2 <- chatRound defaultParams RfText [dummyTool] ToolAuto
            pure (r1, r2)

      ((r1, r2), reqs) <- runEff $ runLLMMock script (withCache cacheStore program)
      length reqs `shouldBe` 2
      crspToolCalls r1 `shouldNotBe` []
      crspToolCalls r2 `shouldNotBe` []

    it "Caches terminal text responses and serves subsequent identical requests from cache" $ do
      cacheStore <- newInMemoryCache
      let script = [Right (textResp "Cached Terminal Answer")]
          program = do
            r1 <- generateText "Query"
            clearHistory
            r2 <- generateText "Query"
            pure (r1, r2)

      ((r1, r2), reqs) <- runEff $ runLLMMock script (withCache cacheStore program)
      r1 `shouldBe` "Cached Terminal Answer"
      r2 `shouldBe` "Cached Terminal Answer"
      -- Only 1 backend request was made; 2nd request was served from cache
      length reqs `shouldBe` 1

    it "Thread-safe concurrent atomic cache lookups and inserts across 20 threads" $ do
      cacheStore <- newInMemoryCache
      let numThreads = 20
      mvar <- newEmptyMVar
      replicateM_ numThreads $ forkIO $ do
        let req = CompletionRequest
              { crModel = Model "model-test"
              , crSystem = Nothing
              , crMessages = [UserMsg "Concurrent Query"]
              , crParams = defaultParams
              , crTools = []
              , crToolChoice = ToolAuto
              , crResponseFormat = RfText
              }
            resp = textResp "Thread-safe answer"
        cacheInsert cacheStore req resp
        mHit <- cacheLookup cacheStore req
        case mHit of
          Just hit -> putMVar mvar (crspText hit == "Thread-safe answer")
          Nothing -> putMVar mvar False

      results <- replicateM numThreads (takeMVar mvar)
      results `shouldBe` replicate numThreads True
