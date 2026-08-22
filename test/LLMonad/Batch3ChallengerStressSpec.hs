{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Batch 3 Challenger Stress Specification: Empirical verification of Batch 3
-- Covers:
-- 1. Anthropic Structured Output & Synthetic Schema Tool Isolation (ANT-011, ANT-012, ANT-017, ANT-018)
-- 2. runAgentStructuredWith Multi-Turn Tool Workflows & Phase Transitions (CORE-029)
-- 3. Transactional History Rollback on Structured Loop Failures (CORE-001, CORE-002, CORE-029)
module LLMonad.Batch3ChallengerStressSpec (spec) where

import Control.Exception (SomeException, try)
import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , Value (..)
  , encode
  , object
  , (.=)
  )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Effectful
import qualified Effectful.Exception as EE
import GHC.Generics (Generic)
import LLMonad
import Test.Hspec

-- | Target structured output record
data StressReport = StressReport
  { reportId :: !Text
  , score    :: !Int
  , tags     :: ![Text]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

-- | Additional nested structured record
data NestedPayload = NestedPayload
  { innerList :: ![Int]
  , metadata  :: !Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

-- | Tool parameter types
data AddArgs = AddArgs
  { valA :: !Int
  , valB :: !Int
  } deriving (Show, Eq, Generic, FromJSON, ToSchema)

data MulArgs = MulArgs
  { factorA :: !Int
  , factorB :: !Int
  } deriving (Show, Eq, Generic, FromJSON, ToSchema)

data QueryArgs = QueryArgs
  { key :: !Text
  } deriving (Show, Eq, Generic, FromJSON, ToSchema)

lbs :: Text -> LBS.ByteString
lbs = LBS.fromStrict . encodeUtf8

addTool :: Monad m => Tool m
addTool = mkTool "add" "Add two integers" $ \(args :: AddArgs) ->
  pure (valA args + valB args)

mulTool :: Monad m => Tool m
mulTool = mkTool "multiply" "Multiply two integers" $ \(args :: MulArgs) ->
  pure (factorA args * factorB args)

queryTool :: (IOE :> es) => IORef [Text] -> Tool (Eff es)
queryTool auditRef = mkTool "query_db" "Query database key" $ \(args :: QueryArgs) -> do
  liftIO (modifyIORef' auditRef (key args :))
  pure ("value_for_" <> key args)

failingTool :: Monad m => Tool m
failingTool = mkTool "exploding_tool" "Throws error" $ \(_ :: QueryArgs) ->
  error "Uncaught tool failure inside IO handler" :: m Text

cfg :: AnthropicConfig
cfg = defaultAnthropicConfig "sk-ant-test"

spec :: Spec
spec = describe "Batch 3 Challenger Stress Suite" $ do

  describe "1. Anthropic Structured Output & Synthetic Schema Tool Disentanglement" $ do
    it "excludes synthetic schema tool from crspToolCalls in single tool_use response" $ do
      let raw = "{\"role\":\"assistant\",\"content\":[{\"type\":\"tool_use\",\"id\":\"tu_synth_1\",\"name\":\"__llmonad_structured_output\",\"input\":{\"reportId\":\"REP-100\",\"score\":98,\"tags\":[\"alpha\",\"beta\"]}}],\"stop_reason\":\"tool_use\"}"
      case parseMessagesResponse (lbs raw) of
        Right resp -> do
          crspToolCalls resp `shouldBe` []
          crspStructuredPayload resp `shouldBe` Just (object [Key.fromText "reportId" .= ("REP-100" :: Text), Key.fromText "score" .= (98 :: Int), Key.fromText "tags" .= (["alpha", "beta"] :: [Text])])
          crspFinishReason resp `shouldBe` FrStop
        Left err -> expectationFailure ("Expected parse success, got: " <> show err)

    it "handles mixed content blocks with text and synthetic tool_use without leaking to tool calls" $ do
      let raw = "{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"Here is your structured report:\"},{\"type\":\"tool_use\",\"id\":\"tu_synth_2\",\"name\":\"__llmonad_structured_output\",\"input\":{\"innerList\":[10,20,30],\"metadata\":\"test-meta\"}}],\"stop_reason\":\"tool_use\"}"
      case parseMessagesResponse (lbs raw) of
        Right resp -> do
          crspText resp `shouldBe` "Here is your structured report:"
          crspToolCalls resp `shouldBe` []
          crspStructuredPayload resp `shouldBe` Just (object [Key.fromText "innerList" .= ([10, 20, 30] :: [Int]), Key.fromText "metadata" .= ("test-meta" :: Text)])
          crspFinishReason resp `shouldBe` FrStop
        Left err -> expectationFailure ("Expected parse success, got: " <> show err)

    it "isolates synthetic tool from real user tools when both are present in response" $ do
      let raw = "{\"role\":\"assistant\",\"content\":[{\"type\":\"tool_use\",\"id\":\"tu_real_1\",\"name\":\"add\",\"input\":{\"valA\":5,\"valB\":10}},{\"type\":\"tool_use\",\"id\":\"tu_synth_3\",\"name\":\"__llmonad_structured_output\",\"input\":{\"reportId\":\"R1\",\"score\":100,\"tags\":[]}}],\"stop_reason\":\"tool_use\"}"
      case parseMessagesResponse (lbs raw) of
        Right resp -> do
          crspToolCalls resp `shouldBe` [ToolCall "tu_real_1" "add" (object [Key.fromText "valA" .= (5 :: Int), Key.fromText "valB" .= (10 :: Int)])]
          crspStructuredPayload resp `shouldBe` Just (object [Key.fromText "reportId" .= ("R1" :: Text), Key.fromText "score" .= (100 :: Int), Key.fromText "tags" .= ([] :: [Text])])
          crspFinishReason resp `shouldBe` FrToolUse
        Left err -> expectationFailure ("Expected parse success, got: " <> show err)

    it "reassembles fine-grained 1-character streaming chunks of synthetic schema tools" $ do
      let jsonPayload = "{\"reportId\":\"STREAM-1\",\"score\":42,\"tags\":[\"fast\",\"safe\"]}" :: Text
          headerEvent = "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"tu_stream_fine\",\"name\":\"__llmonad_structured_output\"}}"
          deltaEvents = [ decodeUtf8 (LBS.toStrict (encode (object ["type" .= ("content_block_delta" :: Text), "index" .= (0 :: Int), "delta" .= object ["type" .= ("input_json_delta" :: Text), "partial_json" .= T.singleton c]]))) | c <- T.unpack jsonPayload ]
          stopEvents =
            [ "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\"}}"
            , "{\"type\":\"message_stop\"}"
            ]
          allEvents = headerEvent : deltaEvents ++ stopEvents
          finalState = foldl (\s e -> fst (handleAnthropicEvent s e)) initialAntStreamState allEvents
      case finalizeAnthropicStream finalState of
        Right resp -> do
          crspToolCalls resp `shouldBe` []
          crspStructuredPayload resp `shouldBe` Just (object [Key.fromText "reportId" .= ("STREAM-1" :: Text), Key.fromText "score" .= (42 :: Int), Key.fromText "tags" .= (["fast", "safe"] :: [Text])])
          crspFinishReason resp `shouldBe` FrStop
        Left err -> expectationFailure ("Expected successful stream finalize, got: " <> show err)

    it "forces synthetic schema tool choice and payload in buildMessagesBody" $ do
      let req = CompletionRequest
            { crModel = "claude-3-5-sonnet-20241022"
            , crSystem = Just "You are an analyzer."
            , crMessages = [UserMsg "Generate report"]
            , crParams = defaultParams
            , crTools = [toolSpec (addTool @IO)]
            , crToolChoice = ToolAuto
            , crResponseFormat = RfJsonSchema "StressReport" (toSchema @StressReport) True
            }
          body = buildMessagesBody cfg req
      case body of
        Object o -> do
          case KM.lookup (Key.fromText "tool_choice") o of
            Just (Object tc) -> do
              KM.lookup (Key.fromText "type") tc `shouldBe` Just (String "tool")
              KM.lookup (Key.fromText "name") tc `shouldBe` Just (String "__llmonad_structured_output")
            other -> expectationFailure ("Expected tool_choice object, got: " <> show other)
          case KM.lookup (Key.fromText "tools") o of
            Just (Array ts) -> do
              -- tools list must contain both user tool and forced synthetic tool
              length ts `shouldBe` 2
            other -> expectationFailure ("Expected tools array of length 2, got: " <> show other)
        other -> expectationFailure ("Expected Object body, got: " <> show other)

  describe "2. runAgentStructuredWith Multi-Turn Tool Workflows & Phase Transitions" $ do
    it "executes multi-turn tool chain before returning structured output" $ do
      auditRef <- newIORef []
      let qTool = queryTool auditRef
          -- Turn 1: model calls add(10, 20)
          round1 = Right (toolResp [ToolCall "c1" "add" (object ["valA" .= (10 :: Int), "valB" .= (20 :: Int)])])
          -- Turn 2: model calls multiply(30, 2)
          round2 = Right (toolResp [ToolCall "c2" "multiply" (object ["factorA" .= (30 :: Int), "factorB" .= (2 :: Int)])])
          -- Turn 3: model calls query_db("key_60")
          round3 = Right (toolResp [ToolCall "c3" "query_db" (object ["key" .= ("key_60" :: Text)])])
          -- Turn 4: model returns final structured response
          finalObj = object ["reportId" .= ("REP-FINAL" :: Text), "score" .= (60 :: Int), "tags" .= (["step1", "step2", "step3"] :: [Text])]
          round4 = Right (structuredResp finalObj)
          script = [round1, round2, round3, round4]

      (res, reqs, hist, _) <- runEff $ runLLMMockFull script $
        runAgentStructuredWith @StressReport defaultAgentOpts [addTool, mulTool, qTool] "Execute multi-step calculation"

      res `shouldBe` StressReport "REP-FINAL" 60 ["step1", "step2", "step3"]
      length reqs `shouldBe` 4
      -- Verify audit tool executed
      queries <- readIORef auditRef
      queries `shouldBe` ["key_60"]

      -- Verify conversation history transcript ordering:
      -- UserMsg -> AssistantMsg (c1) -> ToolMsg (30) -> AssistantMsg (c2) -> ToolMsg (60) -> AssistantMsg (c3) -> ToolMsg ("value_for_key_60") -> AssistantMsg (finalObj)
      let msgTypes = map (\case UserMsg _ -> "user" :: Text; AssistantMsg _ _ -> "assistant"; ToolMsg _ _ -> "tool"; SystemMsg _ -> "system") hist
      msgTypes `shouldBe` ["user", "assistant", "tool", "assistant", "tool", "assistant", "tool", "assistant"]

    it "handles parallel tool execution in multi-turn structured workflow" $ do
      let parallelRound1 = Right (toolResp
            [ ToolCall "p1" "add" (object ["valA" .= (1 :: Int), "valB" .= (2 :: Int)])
            , ToolCall "p2" "add" (object ["valA" .= (10 :: Int), "valB" .= (20 :: Int)])
            ])
          parallelRound2 = Right (toolResp
            [ ToolCall "p3" "multiply" (object ["factorA" .= (3 :: Int), "factorB" .= (4 :: Int)])
            , ToolCall "p4" "multiply" (object ["factorA" .= (5 :: Int), "factorB" .= (6 :: Int)])
            ])
          finalObj = object ["reportId" .= ("PARALLEL" :: Text), "score" .= (72 :: Int), "tags" .= (["p1", "p2", "p3", "p4"] :: [Text])]
          finalRound = Right (structuredResp finalObj)
          script = [parallelRound1, parallelRound2, finalRound]

      (res, reqs, hist, _) <- runEff $ runLLMMockFull script $
        runAgentStructuredWith @StressReport defaultAgentOpts [addTool, mulTool] "Execute parallel batch"

      res `shouldBe` StressReport "PARALLEL" 72 ["p1", "p2", "p3", "p4"]
      length reqs `shouldBe` 3
      let toolResults = [c | ToolMsg _ c <- hist]
      toolResults `shouldBe` ["3", "30", "12", "30"]

    it "succeeds immediately when model returns valid structured JSON on first round without tools" $ do
      let finalObj = object ["reportId" .= ("IMMEDIATE" :: Text), "score" .= (100 :: Int), "tags" .= (["direct"] :: [Text])]
          script = [Right (structuredResp finalObj)]
      (res, reqs, _, _) <- runEff $ runLLMMockFull script $
        runAgentStructuredWith @StressReport defaultAgentOpts [addTool] "Answer directly"
      res `shouldBe` StressReport "IMMEDIATE" 100 ["direct"]
      length reqs `shouldBe` 1

    it "transitions from tool phase to structured phase with schema constraint after non-JSON text" $ do
      let toolRound = Right (toolResp [ToolCall "c1" "add" (object ["valA" .= (5 :: Int), "valB" .= (5 :: Int)])])
          -- Model returns plain conversational text instead of structured output
          plainTextRound = Right (textResp "The sum is 10, now let me format the output.")
          -- Model responds to structured phase feedback with schema-compliant JSON
          finalObj = object ["reportId" .= ("RECOVERED" :: Text), "score" .= (10 :: Int), "tags" .= (["recovered"] :: [Text])]
          structuredRound = Right (structuredResp finalObj)
          script = [toolRound, plainTextRound, structuredRound]

      (res, reqs, hist, _) <- runEff $ runLLMMockFull script $
        runAgentStructuredWith @StressReport defaultAgentOpts [addTool] "Calculate and give structured report"

      res `shouldBe` StressReport "RECOVERED" 10 ["recovered"]
      length reqs `shouldBe` 3
      -- Check that request 3 enforced RfJsonSchema
      case crResponseFormat (reqs !! 2) of
        RfJsonSchema name _ _ -> name `shouldBe` "StressReport"
        other -> expectationFailure ("Expected RfJsonSchema in round 3, got: " <> show other)
      -- Verify feedback message was pushed before round 3
      let userMsgs = [t | UserMsg t <- hist]
      userMsgs `shouldSatisfy` any (T.isInfixOf "Your final response could not be decoded")

    it "recovers from decode failure during structured phase" $ do
      let opts = defaultAgentOpts { agentMaxRounds = 4 }
          badStructuredRound = Right (textResp "Not valid JSON at all")
          goodFinalObj = object ["reportId" .= ("RETRY-OK" :: Text), "score" .= (50 :: Int), "tags" .= ([] :: [Text])]
          goodStructuredRound = Right (structuredResp goodFinalObj)
          -- No tools: goes straight to structured phase
          script = [badStructuredRound, goodStructuredRound]

      (res, reqs, hist, _) <- runEff $ runLLMMockFull script $
        runAgentStructuredWith @StressReport opts [] "No tools needed"

      res `shouldBe` StressReport "RETRY-OK" 50 []
      length reqs `shouldBe` 2
      let feedbackMsgs = [t | UserMsg t <- hist, "Your final response could not be decoded" `T.isInfixOf` t]
      length feedbackMsgs `shouldBe` 1

    it "throws AgentRoundsExhausted when agentMaxRounds is exceeded in tool phase" $ do
      let infiniteTools = repeat (Right (toolResp [ToolCall "loop" "add" (object ["valA" .= (1 :: Int), "valB" .= (1 :: Int)])]))
          opts = defaultAgentOpts { agentMaxRounds = 3 }
      res <- try (runEff $ runLLMMockFull (take 10 infiniteTools) (runAgentStructuredWith @StressReport opts [addTool] "Infinite loop"))
      case res of
        Left (AgentRoundsExhausted 3) -> pure ()
        Left other -> expectationFailure ("Expected AgentRoundsExhausted 3, got: " <> show other)
        Right _ -> expectationFailure "Expected AgentRoundsExhausted exception"

  describe "3. Transactional History Rollback on Structured Loop Failures" $ do
    it "rolls back staged user prompt and partial assistant turns on round-1 ApiError" $ do
      let script = [Left (ApiError 500 "Internal Server Error")]
          opts = defaultAgentOpts { agentMaxRounds = 3 }
          testProg = do
            pushMessage (UserMsg "Prior message 1")
            pushMessage (AssistantMsg "Prior response 1" [])
            _ <- attempt (runAgentStructuredWith @StressReport opts [addTool] "Failing structured agent")
            getHistory

      (hist, reqs, _, _) <- runEff $ runLLMMockFull script testProg
      length reqs `shouldBe` 1
      -- History must only contain prior committed messages
      hist `shouldBe`
        [ UserMsg "Prior message 1"
        , AssistantMsg "Prior response 1" []
        ]

    it "rolls back complete multi-turn tool execution history when structured loop fails at final decode" $ do
      let toolRound1 = Right (toolResp [ToolCall "c1" "add" (object ["valA" .= (1 :: Int), "valB" .= (2 :: Int)])])
          toolRound2 = Right (toolResp [ToolCall "c2" "add" (object ["valA" .= (3 :: Int), "valB" .= (4 :: Int)])])
          badFinal1 = Right (textResp "Bad final JSON 1")
          badFinal2 = Right (textResp "Bad final JSON 2")
          script = [toolRound1, toolRound2, badFinal1, badFinal2]
          opts = defaultAgentOpts { agentMaxRounds = 3 }
          testProg = do
            pushMessage (UserMsg "Initial committed task")
            pushMessage (AssistantMsg "Initial committed reply" [])
            _ <- attempt (runAgentStructuredWith @StressReport opts [addTool] "Run agent that exhausts rounds")
            getHistory

      (hist, reqs, _, _) <- runEff $ runLLMMockFull script testProg
      length reqs `shouldBe` 3
      -- Must restore exactly the initial history before the structured loop
      hist `shouldBe`
        [ UserMsg "Initial committed task"
        , AssistantMsg "Initial committed reply" []
        ]

    it "rolls back history when tool execution throws an unhandled exception" $ do
      let explodingCall = Right (toolResp [ToolCall "c_boom" "exploding_tool" (object ["key" .= ("boom" :: Text)])])
          script = [explodingCall]
          opts = defaultAgentOpts { agentMaxRounds = 3 }
          testProg = do
            pushMessage (UserMsg "Safe prompt")
            pushMessage (AssistantMsg "Safe answer" [])
            _ <- EE.try @SomeException (runAgentStructuredWith @StressReport opts [failingTool] "Run exploding tool")
            getHistory

      (hist, _, _, _) <- runEff $ runLLMMockFull script testProg
      hist `shouldBe`
        [ UserMsg "Safe prompt"
        , AssistantMsg "Safe answer" []
        ]

    it "isolates committed turns across sequential successful and failed structured agent turns" $ do
      let successObj1 = object ["reportId" .= ("REP-1" :: Text), "score" .= (10 :: Int), "tags" .= ([] :: [Text])]
          successObj2 = object ["reportId" .= ("REP-2" :: Text), "score" .= (20 :: Int), "tags" .= ([] :: [Text])]
          script =
            [ -- Turn 1: structured agent success
              Right (structuredResp successObj1)
              -- Turn 2: structured agent failure (ApiError)
            , Left (ApiError 503 "Service Unavailable")
              -- Turn 3: structured agent success with tool call
            , Right (toolResp [ToolCall "c1" "add" (object ["valA" .= (10 :: Int), "valB" .= (10 :: Int)])])
            , Right (structuredResp successObj2)
            ]
          opts = defaultAgentOpts { agentMaxRounds = 3 }
          pipeline = do
            r1 <- runAgentStructuredWith @StressReport opts [addTool] "First task"
            _  <- attempt (runAgentStructuredWith @StressReport opts [addTool] "Second failing task")
            r2 <- runAgentStructuredWith @StressReport opts [addTool] "Third task"
            pure (r1, r2)

      ((r1, r2), reqs, hist, _) <- runEff $ runLLMMockFull script pipeline
      r1 `shouldBe` StressReport "REP-1" 10 []
      r2 `shouldBe` StressReport "REP-2" 20 []
      length reqs `shouldBe` 4
      -- Final history must contain Turn 1 messages and Turn 3 messages, with ZERO artifacts of Turn 2
      let userMsgs = [t | UserMsg t <- hist]
      userMsgs `shouldBe` ["First task", "Third task"]

    it "preserves pre-existing system prompt and conversation history on structured loop failure" $ do
      let script = [Left (ApiError 500 "Server error")]
          opts = defaultAgentOpts { agentMaxRounds = 3 }
          testProg = do
            setSystem "You are a critical system tester."
            pushMessage (UserMsg "Prior user query")
            pushMessage (AssistantMsg "Prior assistant reply" [])
            _ <- attempt (runAgentStructuredWith @StressReport opts [addTool] "Should fail and rollback")
            (,,) <$> getSystem <*> getHistory <*> pure ()

      ((sys, hist, ()), _, _, _) <- runEff $ runLLMMockFull script testProg
      sys `shouldBe` Just "You are a critical system tester."
      hist `shouldBe`
        [ UserMsg "Prior user query"
        , AssistantMsg "Prior assistant reply" []
        ]

  describe "4. Multi-Turn Tool Failure Recovery & Deep Workflows in Structured Agents" $ do
    it "recovers from fail-closed tool argument validation error and successfully completes structured output" $ do
      -- Round 1: Model calls add with invalid argument (string instead of int)
      let badToolCall = Right (toolResp [ToolCall "c_bad" "add" (object ["valA" .= ("not_an_int" :: Text), "valB" .= (10 :: Int)])])
          -- Round 2: Model receives error message in ToolMsg, corrects arguments to add(20, 30)
          goodToolCall = Right (toolResp [ToolCall "c_good" "add" (object ["valA" .= (20 :: Int), "valB" .= (30 :: Int)])])
          -- Round 3: Model returns structured result
          finalObj = object ["reportId" .= ("RECOVERED-TOOL" :: Text), "score" .= (50 :: Int), "tags" .= (["healed"] :: [Text])]
          finalRound = Right (structuredResp finalObj)
          script = [badToolCall, goodToolCall, finalRound]
          opts = defaultAgentOpts { agentMaxRounds = 5 }

      (res, reqs, hist, _) <- runEff $ runLLMMockFull script $
        runAgentStructuredWith @StressReport opts [addTool] "Calculate with error recovery"

      res `shouldBe` StressReport "RECOVERED-TOOL" 50 ["healed"]
      length reqs `shouldBe` 3
      -- Check that Round 1 tool result contains error payload
      let toolMsgs = [c | ToolMsg _ c <- hist]
      length toolMsgs `shouldBe` 2
      (toolMsgs !! 0) `shouldSatisfy` T.isInfixOf "invalid arguments"
      (toolMsgs !! 1) `shouldBe` "50"

    it "executes 5-step deep tool workflow before delivering structured output" $ do
      let r1 = Right (toolResp [ToolCall "c1" "add" (object ["valA" .= (1 :: Int), "valB" .= (2 :: Int)])])
          r2 = Right (toolResp [ToolCall "c2" "add" (object ["valA" .= (3 :: Int), "valB" .= (4 :: Int)])])
          r3 = Right (toolResp [ToolCall "c3" "multiply" (object ["factorA" .= (3 :: Int), "factorB" .= (7 :: Int)])])
          r4 = Right (toolResp [ToolCall "c4" "add" (object ["valA" .= (21 :: Int), "valB" .= (9 :: Int)])])
          r5 = Right (toolResp [ToolCall "c5" "multiply" (object ["factorA" .= (30 :: Int), "factorB" .= (2 :: Int)])])
          finalObj = object ["reportId" .= ("5-STEP" :: Text), "score" .= (60 :: Int), "tags" .= (["deep","chain"] :: [Text])]
          rFinal = Right (structuredResp finalObj)
          script = [r1, r2, r3, r4, r5, rFinal]
          opts = defaultAgentOpts { agentMaxRounds = 10 }

      (res, reqs, hist, _) <- runEff $ runLLMMockFull script $
        runAgentStructuredWith @StressReport opts [addTool, mulTool] "Run 5 steps"

      res `shouldBe` StressReport "5-STEP" 60 ["deep", "chain"]
      length reqs `shouldBe` 6
      let toolContents = [c | ToolMsg _ c <- hist]
      toolContents `shouldBe` ["3", "7", "21", "30", "60"]

