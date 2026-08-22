{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Batch 2 Challenger Adversarial Stress Test Suite:
-- Rigorously stress-tests:
-- 1. Concurrent Subagent Execution & State Isolation with runSubagentAsync
-- 2. Cache Middleware Safety, Model Discrimination, Non-caching of Tool turns & CAS Concurrency
-- 3. Transactional Staging & Error Rollbacks across edge cases
module LLMonad.Batch2ChallengerSpec (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (wait)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM, forM_, replicateM)
import Data.Aeson (FromJSON, ToJSON (..), object, toJSON)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import GHC.Generics (Generic)
import LLMonad
import Test.Hspec

data CalcArgs = CalcArgs
  { operand :: !Int
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data RecordSchema = RecordSchema
  { recKey :: !Text
  , recVal :: !Int
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

-- | Dynamic provider that deterministically matches requests from concurrent subagents
dynamicProvider :: IORef [CompletionRequest] -> Provider
dynamicProvider reqsRef = Provider
  { providerName = "dynamic-mock"
  , providerStructured = StructuredNative
  , providerComplete = \req -> do
      atomicModifyIORef' reqsRef (\rs -> (req : rs, ()))
      let msgs = crMessages req
          isFailTask = any (\case UserMsg t -> "FailTask" `T.isInfixOf` t; _ -> False) msgs
      if isFailTask
        then pure (Left (ApiError 500 "Simulated subagent crash"))
        else case [t | UserMsg t <- msgs, "Subtask" `T.isInfixOf` t] of
          (taskText : _) -> do
            let subtaskId = case T.words taskText of
                  (_:wid:_) -> wid
                  _ -> "0"
                hasToolResult = any (\case ToolMsg _ _ -> True; _ -> False) msgs
            if hasToolResult
              then pure (Right (textResp ("Result worker " <> subtaskId)))
              else pure (Right (toolResp [ToolCall ("c-" <> subtaskId) "calc" (toJSON (CalcArgs 1))]))
          [] -> case [t | UserMsg t <- msgs, "SuccessTask" `T.isInfixOf` t] of
            (succText : _) -> do
              let succId = case T.words succText of
                    (_:sid:_) -> sid
                    _ -> "0"
              pure (Right (textResp ("Success " <> succId)))
            [] -> pure (Right (textResp "Generic mock response"))
  , providerStream = nonStreamingFallback $ \req -> do
      atomicModifyIORef' reqsRef (\rs -> (req : rs, ()))
      pure (Right (textResp ("Streamed response")))
  }

spec :: Spec
spec = describe "Batch 2 Challenger Adversarial Suite" $ do

  describe "1. Subagent Concurrency, State Isolation & Sandboxing" $ do

    it "Executes 30 concurrent subagents via runSubagentAsync with zero history pollution" $ do
      let numWorkers = 30
      reqsRef <- newIORef []
      let prov = dynamicProvider reqsRef
      let calcTool = toolSync "calc" "Square a number" (\(args :: CalcArgs) -> ("Squared: " <> T.pack (show (operand args * operand args)) :: Text))
      let parentTools = [calcTool]

      -- Run parent agent computation with initial history
      let parentAct = do
            pushMessage (UserMsg "Parent Initial Mission")

            -- Spawn 30 async subagents concurrently
            asyncHandles <- forM [1 .. numWorkers] $ \i -> do
              let args = SubagentArgs ("Subtask " <> T.pack (show i)) Nothing (Just 4) Nothing Nothing
              runSubagentAsync args parentTools

            -- Await all subagent results
            results <- liftIO (mapM wait asyncHandles)

            -- Verify parent history after all subagents finish
            parentHist <- getHistory
            pure (results, parentHist)

      ((((results, finalParentHist), _), _)) <- runEff $ runJournalMemory $ runWorldMemory (initMemoryWorld []) $ runLLMHTTP (defaultConfig prov (Model "mock")) parentAct

      -- Verify all 30 completed successfully
      length results `shouldBe` numWorkers
      forM_ (zip [1 .. numWorkers] results) $ \(i, res) -> do
        srStatus res `shouldBe` "completed"
        srOutput res `shouldBe` ("Result worker " <> T.pack (show i))

      -- Crucial assertion: Parent history has ONLY the parent's initial message
      -- Absolutely ZERO subagent user messages, tool calls, or tool results leaked
      finalParentHist `shouldBe` [UserMsg "Parent Initial Mission"]

    it "Maintains strict transcript isolation across concurrent subagents (no message cross-talk)" $ do
      -- Subagent 1 task: "UniqueTaskAlpha"
      -- Subagent 2 task: "UniqueTaskBeta"
      -- Both run concurrently. We verify through captured requests that Subagent 1 never sees "UniqueTaskBeta"
      let script1 =
            [ Right (toolResp [ToolCall "c1" "calc" (toJSON (CalcArgs 10))])
            , Right (textResp "Alpha Finished")
            ]
          script2 =
            [ Right (toolResp [ToolCall "c2" "calc" (toJSON (CalcArgs 20))])
            , Right (textResp "Beta Finished")
            ]
          calcTool = toolSync "calc" "Square" (\(_args :: CalcArgs) -> ("Squared" :: Text))

      let runConcurrent = do
            a1 <- runSubagentAsync (SubagentArgs "UniqueTaskAlpha" Nothing (Just 3) Nothing Nothing) [calcTool]
            a2 <- runSubagentAsync (SubagentArgs "UniqueTaskBeta" Nothing (Just 3) Nothing Nothing) [calcTool]
            r1 <- liftIO (wait a1)
            r2 <- liftIO (wait a2)
            pure (r1, r2)

      let combinedScript = script1 ++ script2
      ((((r1, r2), reqs), _), _) <- runEff $ runJournalMemory $ runWorldMemory (initMemoryWorld []) $ runLLMMock combinedScript runConcurrent

      srStatus r1 `shouldBe` "completed"
      srStatus r2 `shouldBe` "completed"

      -- Inspect all captured CompletionRequests.
      -- Every request containing "UniqueTaskAlpha" must NOT contain "UniqueTaskBeta" in its crMessages, and vice versa.
      forM_ reqs $ \req -> do
        let msgs = crMessages req
            hasAlpha = any (\case UserMsg t -> "UniqueTaskAlpha" `T.isInfixOf` t; _ -> False) msgs
            hasBeta  = any (\case UserMsg t -> "UniqueTaskBeta" `T.isInfixOf` t; _ -> False) msgs
        (hasAlpha && hasBeta) `shouldBe` False

    it "Handles mixed success and failure across concurrent subagents without state corruption" $ do
      let numSuccess = 10
          numFailure = 10
      reqsRef <- newIORef []
      let prov = dynamicProvider reqsRef

      let runMixed = do
            pushMessage (UserMsg "Parent Stays Intact")
            succHandles <- forM [1 .. numSuccess] $ \i ->
              runSubagentAsync (SubagentArgs ("SuccessTask " <> T.pack (show i)) Nothing (Just 2) Nothing Nothing) []
            failHandles <- forM [1 .. numFailure] $ \i ->
              runSubagentAsync (SubagentArgs ("FailTask " <> T.pack (show i)) Nothing (Just 2) Nothing Nothing) []
            succRes <- liftIO (mapM wait succHandles)
            failRes <- liftIO (mapM wait failHandles)
            hist <- getHistory
            pure (succRes, failRes, hist)

      ((((succResults, failResults, finalHist), _), _)) <- runEff $ runJournalMemory $ runWorldMemory (initMemoryWorld []) $ runLLMHTTP (defaultConfig prov (Model "mock")) runMixed

      length succResults `shouldBe` numSuccess
      forM_ succResults $ \res -> do
        srStatus res `shouldBe` "completed"
        "Success" `T.isPrefixOf` srOutput res `shouldBe` True

      length failResults `shouldBe` numFailure
      forM_ failResults $ \res -> do
        srStatus res `shouldBe` "failed"

      -- Parent history remains solely the initial message
      finalHist `shouldBe` [UserMsg "Parent Stays Intact"]

    it "Tool filtering enforces sandboxing constraints for subagents" $ do
      let allTools :: [Tool (Eff '[World, Journal, LLM, IOE])]
          allTools = standardCodingTools ++ [subagentTool standardCodingTools]
      -- 1. Recursive subagent tool is stripped
      let filtered1 = filterSubagentTools (SubagentArgs "task" Nothing Nothing Nothing Nothing) allTools
      any (\t -> toolSpecName (toolSpec t) == "subagent") filtered1 `shouldBe` False

      -- 2. Role 'explorer' restricts to read-only tools
      let filteredExplorer = filterSubagentTools (SubagentArgs "task" (Just "explorer") Nothing Nothing Nothing) allTools
      let explorerNames = map (toolSpecName . toolSpec) filteredExplorer
      explorerNames `shouldBe` ["view_file", "grep_search", "find_by_name", "list_dir"]

      -- 3. Whitelist allowedTools
      let filteredWhitelist = filterSubagentTools (SubagentArgs "task" Nothing Nothing (Just ["view_file", "edit_file"]) Nothing) allTools
      let whitelistNames = map (toolSpecName . toolSpec) filteredWhitelist
      whitelistNames `shouldBe` ["view_file", "edit_file"]

  describe "2. Cache Middleware Safety, Discrimination & Concurrency" $ do

    it "Discriminates cache entries across diverse models for identical prompts" $ do
      cacheStore <- newInMemoryCache
      let models = [Model "gpt-4o", Model "claude-3-5-sonnet", Model "gemini-1.5-pro", Model "mistral-large"]
      let promptText = "Explain monads in 3 words."

      -- First pass: Every model executes and caches its distinct response
      forM_ models $ \m -> do
        let modelText = "Response from " <> unModel m
            script = [Right (textResp modelText)]
        (ans, reqs) <- runEff $ runLLMMock script (withCacheModel m cacheStore (generateText promptText))
        ans `shouldBe` modelText
        length reqs `shouldBe` 1

      -- Second pass: Every model is queried again - must ALL hit cache with 0 new backend calls
      forM_ models $ \m -> do
        let expectedText = "Response from " <> unModel m
            emptyScript = []
        (ans, reqs) <- runEff $ runLLMMock emptyScript (withCacheModel m cacheStore (generateText promptText))
        ans `shouldBe` expectedText
        length reqs `shouldBe` 0

    it "Discriminates cache entries across different request parameters and system prompts" $ do
      cacheStore <- newInMemoryCache
      let m = Model "model-params-test"
      let p1 = defaultParams { paramTemperature = Just 0.0 }
          p2 = defaultParams { paramTemperature = Just 1.0 }

      -- Same prompt, different temperature -> distinct cache entries
      (r1, reqs1) <- runEff $ runLLMMock [Right (textResp "Temp 0.0")] $
        withCacheModel m cacheStore (generateTextWith p1 "Hello")
      r1 `shouldBe` "Temp 0.0"
      length reqs1 `shouldBe` 1

      (r2, reqs2) <- runEff $ runLLMMock [Right (textResp "Temp 1.0")] $
        withCacheModel m cacheStore (generateTextWith p2 "Hello")
      r2 `shouldBe` "Temp 1.0"
      length reqs2 `shouldBe` 1

      -- Same prompt & params, different system prompt -> distinct cache entries
      (rSys1, reqsSys1) <- runEff $ runLLMMock [Right (textResp "System Sys1")] $
        withCacheModel m cacheStore (do setSystem "Sys1"; generateText "Query")
      rSys1 `shouldBe` "System Sys1"
      length reqsSys1 `shouldBe` 1

      (rSys2, reqsSys2) <- runEff $ runLLMMock [Right (textResp "System Sys2")] $
        withCacheModel m cacheStore (do setSystem "Sys2"; generateText "Query")
      rSys2 `shouldBe` "System Sys2"
      length reqsSys2 `shouldBe` 1

    it "Never caches responses containing tool calls or FrToolUse finish reason" $ do
      -- 1. Response with non-empty tool calls and FrToolUse
      let resp1 = toolResp [ToolCall "call-1" "search" (object [])]
      isCacheableResponse resp1 `shouldBe` False

      -- 2. Response with non-empty tool calls and FrStop
      let resp2 = (toolResp [ToolCall "call-1" "search" (object [])]) { crspFinishReason = FrStop }
      isCacheableResponse resp2 `shouldBe` False

      -- 3. Response with empty tool calls and FrToolUse
      let resp3 = (textResp "Looking up...") { crspFinishReason = FrToolUse }
      isCacheableResponse resp3 `shouldBe` False

      -- 4. Response with empty tool calls and FrStop (terminal text)
      let resp4 = textResp "Final answer"
      isCacheableResponse resp4 `shouldBe` True

      -- Verify with middleware: repeated tool turns always hit backend
      cacheStore <- newInMemoryCache
      let dummyTool = ToolSpec "search" "Search" (object [])
          script = [Right resp1, Right resp1, Right resp1]
          act = do
            _ <- chatRound defaultParams RfText [dummyTool] ToolAuto
            clearHistory
            _ <- chatRound defaultParams RfText [dummyTool] ToolAuto
            clearHistory
            _ <- chatRound defaultParams RfText [dummyTool] ToolAuto
            pure ()

      (_, reqs) <- runEff $ runLLMMock script (withCache cacheStore act)
      length reqs `shouldBe` 3

    it "Handles high-concurrency atomic cache operations (50 threads / 500 ops) without race or deadlock" $ do
      cacheStore <- newInMemoryCache
      let numThreads = 50
          opsPerThread = 10
      mvar <- newEmptyMVar

      forM_ [1 .. numThreads] $ \threadId -> forkIO $ do
        results <- forM [1 .. opsPerThread] $ \opId -> do
          let keyId = (threadId + opId) `mod` 5
              m = Model ("model-" <> T.pack (show keyId))
              req = CompletionRequest
                { crModel = m
                , crSystem = Nothing
                , crMessages = [UserMsg ("Prompt " <> T.pack (show keyId))]
                , crParams = defaultParams
                , crTools = []
                , crToolChoice = ToolAuto
                , crResponseFormat = RfText
                }
              val = textResp ("Answer " <> T.pack (show keyId))
          -- Perform atomic insert
          cacheInsert cacheStore req val
          -- Perform atomic lookup
          mHit <- cacheLookup cacheStore req
          case mHit of
            Just hit -> pure (crspText hit == ("Answer " <> T.pack (show keyId)))
            Nothing -> pure False
        putMVar mvar (and results)

      allResults <- replicateM numThreads (takeMVar mvar)
      allResults `shouldBe` replicate numThreads True

  describe "3. Transactional Staging & Error Rollbacks" $ do

    it "askStructured rolls back staged prompt on decode failure and leaves history clean" $ do
      let badPayload = Right (textResp "{\"recKey\": 123, \"recVal\": \"not an int\"}")
          script = [badPayload]
          act = askStructured @RecordSchema "Get Record"

      (res, _, hist, _) <- runEff $ runLLMMockFull script (attempt act)
      case res of
        Left (DecodeError _ _) -> pure ()
        other -> expectationFailure ("Expected DecodeError, got: " <> show other)
      hist `shouldBe` []

    it "extractWithRetry cleanly rolls back all staged messages when all retries are exhausted" $ do
      let badPayload = Right (textResp "Invalid JSON response")
          script = [badPayload, badPayload, badPayload]
          act = extractWithRetry @RecordSchema 3 "Extract Record with Retry"

      (res, _, hist, _) <- runEff $ runLLMMockFull script (attempt act)
      case res of
        Left (DecodeError _ _) -> pure ()
        other -> expectationFailure ("Expected DecodeError, got: " <> show other)
      -- All 3 attempts generated prompts/feedback, but all must be rolled back
      hist `shouldBe` []

    it "Multi-turn conversation preserves committed turns while rolling back only failed turn" $ do
      let script =
            [ Right (textResp "Answer 1")
            , Left (HttpError "Connection reset on turn 2")
            , Right (textResp "Answer 3")
            ]
          pipeline = do
            pushMessage (UserMsg "Turn 1 Prompt")
            r1 <- chatRound defaultParams RfText [] ToolAuto
            _ <- attempt (generateText "Failing Turn 2")
            pushMessage (UserMsg "Turn 3 Prompt")
            r3 <- chatRound defaultParams RfText [] ToolAuto
            pure (crspText r1, crspText r3)

      ((r1, r3), _, hist, _) <- runEff $ runLLMMockFull script pipeline
      r1 `shouldBe` "Answer 1"
      r3 `shouldBe` "Answer 3"
      hist `shouldBe`
        [ UserMsg "Turn 1 Prompt"
        , AssistantMsg "Answer 1" []
        , UserMsg "Turn 3 Prompt"
        , AssistantMsg "Answer 3" []
        ]
