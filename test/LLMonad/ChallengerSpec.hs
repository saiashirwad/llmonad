{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module LLMonad.ChallengerSpec (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forM_, replicateM, replicateM_)
import Data.Aeson
  ( Value (..)
  , object
  , (.=)
  )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector as V
import Effectful
import LLMonad
import LLMonad.Internal.Extract (extractJSON)
import LLMonad.Internal.SSE (finishSSE, newSSEParser, stepSSE)
import Test.Hspec

spec :: Spec
spec = do
  describe "Challenger Adversarial Suite: Middleware Stacking & Interposition Semantics" $ do
    it "User-facing (inner) withTrace intercepts all calls, while backend (outer) withTrace only sees cache misses" $ do
      backendTracesRef <- newIORef []
      userTracesRef <- newIORef []
      cacheStore <- newInMemoryCache

      let script = [Right (textResp "Cached Reply")]
          program = do
            -- Call 1: Miss (populates cache)
            r1 <- generateText "Query 1"
            clearHistory
            -- Call 2: Hit (served from cache because history matches)
            r2 <- generateText "Query 1"
            pure (r1, r2)

      -- In effectful, (f . g . h) means h intercepts before g, and g intercepts before f.
      -- Here: backendTracer wraps cacheStore wraps userTracer.
      let stackedEff =
            withTrace (\t -> modifyIORef' backendTracesRef (t :)) $
              withCache cacheStore $
                withTrace (\t -> modifyIORef' userTracesRef (t :)) program

      ((res1, res2), reqs, hist, _) <- runEff (runLLMMockFull script stackedEff)
      res1 `shouldBe` "Cached Reply"
      res2 `shouldBe` "Cached Reply"

      -- Mock interpreter only received 1 request
      length reqs `shouldBe` 1

      -- User tracer (inner) saw 2 requests and 2 responses
      userTraces <- readIORef userTracesRef
      length userTraces `shouldBe` 4

      -- Backend tracer (outer) only saw 1 request and 1 response (bypassed on cache hit)
      backendTraces <- readIORef backendTracesRef
      length backendTraces `shouldBe` 2

      -- Final history contains turn 2
      hist `shouldBe` [UserMsg "Query 1", AssistantMsg "Cached Reply" []]

    it "Shares cache across independent Eff executions" $ do
      cacheStore <- newInMemoryCache
      let script1 = [Right (textResp "Shared Answer")]
          script2 = [] -- No responses scripted!

      -- Run 1: Miss and fill cache
      (r1, reqs1) <- runEff (runLLMMock script1 (withCache cacheStore (generateText "What is 2+2?")))
      r1 `shouldBe` "Shared Answer"
      length reqs1 `shouldBe` 1

      -- Run 2: Exact hit from cache without exhausting empty mock script
      (r2, reqs2) <- runEff (runLLMMock script2 (withCache cacheStore (generateText "What is 2+2?")))
      r2 `shouldBe` "Shared Answer"
      length reqs2 `shouldBe` 0

    it "Short-circuits backend withRateLimit when served from withCache" $ do
      cacheStore <- newInMemoryCache
      -- 1 token/sec, 1 capacity (will block 1.0s if 2nd request hits limiter)
      limiter <- newRateLimiter 1.0 1.0

      let script = [Right (textResp "Fast Reply")]
          program = do
            r1 <- generateText "Query"
            clearHistory
            r2 <- generateText "Query"
            pure (r1, r2)

      -- withRateLimit on backend (outer), withCache closer to user (inner)
      let stackedEff = withRateLimit limiter (withCache cacheStore program)

      t0 <- getPOSIXTime
      ((res1, res2), reqs) <- runEff (runLLMMock script stackedEff)
      t1 <- getPOSIXTime

      res1 `shouldBe` "Fast Reply"
      res2 `shouldBe` "Fast Reply"
      length reqs `shouldBe` 1
      -- Elapsed time should be near instantaneous because 2nd call was served from cache
      (t1 - t0) `shouldSatisfy` (< 0.5)

  describe "Challenger Adversarial Suite: Mock Script Exhaustion & Error Bounds" $ do
    it "Throws ApiError 500 when mock script is empty on ChatRound" $ do
      let act = generateText "Test"
      res <- runEff (runLLMMock [] (attempt act))
      case fst res of
        Left (ApiError 500 msg) -> msg `shouldBe` "runLLMMock: response script exhausted"
        other -> expectationFailure ("Expected ApiError 500 script exhausted, got: " <> show other)

    it "Throws ApiError 500 when mock script is empty on StreamRound" $ do
      let act = streamText (\_ -> pure ()) "Test"
      res <- runEff (runLLMMock [] (attempt act))
      case fst res of
        Left (ApiError 500 msg) -> msg `shouldBe` "runLLMMock: response script exhausted"
        other -> expectationFailure ("Expected ApiError 500 on stream exhaust, got: " <> show other)

    it "Preserves partial conversation history when exhausted mid-pipeline" $ do
      let script = [Right (textResp "First OK")]
          pipeline = do
            r1 <- generateText "Step 1"
            r2 <- generateText "Step 2"
            pure (r1, r2)
      ((res, _, hist, _)) <- runEff (runLLMMockFull script (attempt pipeline))
      case res of
        Left (ApiError 500 _) -> pure ()
        other -> expectationFailure ("Expected ApiError 500, got: " <> show other)
      -- History should contain Step 1, First OK, and Step 2
      hist `shouldBe`
        [ UserMsg "Step 1"
        , AssistantMsg "First OK" []
        , UserMsg "Step 2"
        ]

  describe "Challenger Adversarial Suite: Cache Key Discrimination & Tool Spec Collisions" $ do
    it "Distinguishes requests by system prompt in cache" $ do
      cacheStore <- newInMemoryCache
      let script = [Right (textResp "Sys1 Reply"), Right (textResp "Sys2 Reply")]
          act = withCache cacheStore $ do
            setSystem "System A"
            r1 <- generateText "Hello"
            clearHistory
            setSystem "System B"
            r2 <- generateText "Hello"
            pure (r1, r2)
      ((r1, r2), reqs) <- runEff (runLLMMock script act)
      r1 `shouldBe` "Sys1 Reply"
      r2 `shouldBe` "Sys2 Reply"
      length reqs `shouldBe` 2

    it "Distinguishes requests by sampling parameters in cache" $ do
      cacheStore <- newInMemoryCache
      let script = [Right (textResp "Temp 0.1"), Right (textResp "Temp 0.9")]
          act = withCache cacheStore $ do
            r1 <- generateTextWith (defaultParams {paramTemperature = Just 0.1}) "Hello"
            clearHistory
            r2 <- generateTextWith (defaultParams {paramTemperature = Just 0.9}) "Hello"
            pure (r1, r2)
      ((r1, r2), reqs) <- runEff (runLLMMock script act)
      r1 `shouldBe` "Temp 0.1"
      r2 `shouldBe` "Temp 0.9"
      length reqs `shouldBe` 2

    it "Distinguishes requests by tools and tool choice in cache" $ do
      cacheStore <- newInMemoryCache
      let tool1 = ToolSpec "tool1" "desc1" (object ["a" .= ("int" :: Text)])
          tool2 = ToolSpec "tool2" "desc2" (object ["b" .= ("str" :: Text)])
          script =
            [ Right (toolResp [ToolCall "c1" "tool1" (object [])])
            , Right (toolResp [ToolCall "c2" "tool2" (object [])])
            ]
          act = withCache cacheStore $ do
            r1 <- chatRound defaultParams RfText [tool1] ToolAuto
            clearHistory
            r2 <- chatRound defaultParams RfText [tool2] ToolAuto
            pure (r1, r2)
      ((r1, r2), reqs) <- runEff (runLLMMock script act)
      -- Request 2 does not hit cache of Request 1 because keyOf includes crTools
      r1 `shouldNotBe` r2
      length reqs `shouldBe` 2

  describe "Challenger Adversarial Suite: Trace Behavior on Exceptions" $ do
    it "Emits TraceRequest and TraceError on failure, and does not emit TraceResponse" $ do
      tracesRef <- newIORef []
      let script = [Left (ApiError 401 "Unauthorized")]
          act = withTrace (\t -> modifyIORef' tracesRef (t :)) (generateText "Fail me")
      res <- runEff (runLLMMock script (attempt act))
      case fst res of
        Left (ApiError 401 _) -> pure ()
        other -> expectationFailure ("Expected 401, got: " <> show other)
      traces <- readIORef tracesRef
      -- Both TraceRequest and TraceError were emitted before failure
      length traces `shouldBe` 2
      case reverse traces of
        [TraceRequest {}, TraceError (ApiError 401 "Unauthorized")] -> pure ()
        other -> expectationFailure ("Expected [TraceRequest, TraceError], got: " <> show other)

  describe "Challenger Adversarial Suite: RateLimiter Token Bucket Semantics" $ do
    it "Enforces waiting delay when token capacity is exhausted" $ do
      -- 10 tokens/sec, capacity 2
      limiter <- newRateLimiter 10.0 2.0
      t0 <- getPOSIXTime
      -- Consume 2 tokens instantly
      rlAcquire limiter 1
      rlAcquire limiter 1
      t1 <- getPOSIXTime
      (t1 - t0) `shouldSatisfy` (< 0.1)

      -- 3rd token must wait ~0.1s
      rlAcquire limiter 1
      t2 <- getPOSIXTime
      (t2 - t1) `shouldSatisfy` (>= 0.08)

  describe "Challenger Adversarial Suite: Multi-Protocol Serialization & Edge Cases" $ do
    it "OpenAI: parses responses with null message content and extracts tool calls" $ do
      let raw = "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":null,\"tool_calls\":[{\"id\":\"call_42\",\"type\":\"function\",\"function\":{\"name\":\"calc\",\"arguments\":\"{\\\"x\\\": 10}\"}}]},\"finish_reason\":\"tool_calls\"}],\"usage\":{\"prompt_tokens\":5,\"completion_tokens\":15}}"
      case parseChatCompletionsResponse (LBS.fromStrict (encodeUtf8 raw)) of
        Right resp -> do
          crspText resp `shouldBe` ""
          crspFinishReason resp `shouldBe` FrToolUse
          crspUsage resp `shouldBe` Just (Usage 5 15)
          case crspToolCalls resp of
            [c] -> do
              toolCallId c `shouldBe` "call_42"
              toolCallName c `shouldBe` "calc"
              toolCallArguments c `shouldBe` object [Key.fromText "x" .= (10 :: Int)]
            other -> expectationFailure ("Expected 1 tool call, got: " <> show (length other))
        Left err -> expectationFailure ("Failed to parse response: " <> show err)

    it "OpenAI: handles interleaved tool call deltas across streaming chunks" $ do
      let chunks =
            [ "{\"choices\":[{\"delta\":{\"role\":\"assistant\",\"content\":\"Working...\"}}]}"
            , "{\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"c0\",\"function\":{\"name\":\"tool_a\",\"arguments\":\"{\\\"a\\\": \"}}]}}]}"
            , "{\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":1,\"id\":\"c1\",\"function\":{\"name\":\"tool_b\",\"arguments\":\"{\\\"b\\\": \"}}]}}]}"
            , "{\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"1}\"}}]}}]}"
            , "{\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":1,\"function\":{\"arguments\":\"2}\"}}]}}]}"
            , "{\"choices\":[{\"delta\":{},\"finish_reason\":\"tool_calls\"}]}"
            , "[DONE]"
            ]
          (st, _) = foldl (\(s, _) c -> handleOpenAIChunk s c) (initialOAIStreamState, []) chunks
      case finalizeOAIStream st of
        Right resp -> do
          crspText resp `shouldBe` "Working..."
          crspFinishReason resp `shouldBe` FrToolUse
          let calls = crspToolCalls resp
          length calls `shouldBe` 2
          toolCallName (calls !! 0) `shouldBe` "tool_a"
          toolCallArguments (calls !! 0) `shouldBe` object [Key.fromText "a" .= (1 :: Int)]
          toolCallName (calls !! 1) `shouldBe` "tool_b"
          toolCallArguments (calls !! 1) `shouldBe` object [Key.fromText "b" .= (2 :: Int)]
        Left err -> expectationFailure ("Finalize stream failed: " <> show err)

    it "OpenAI: captures streaming error chunk as ApiError 500" $ do
      let errChunk = "{\"error\":{\"message\":\"Rate limit exceeded\",\"type\":\"rate_limit_error\",\"code\":429}}"
          (st, _) = handleOpenAIChunk initialOAIStreamState errChunk
      case finalizeOAIStream st of
        Left (ApiError 500 msg) -> msg `shouldSatisfy` T.isInfixOf "Rate limit exceeded"
        other -> expectationFailure ("Expected ApiError 500, got: " <> show other)

    it "Anthropic: groups multiple consecutive tool results into a single user message" $ do
      let cfg = defaultAnthropicConfig "sk-ant"
          req = CompletionRequest
            { crModel = Model "claude-3-5-sonnet-20241022"
            , crSystem = Just "System instructions"
            , crMessages =
                [ UserMsg "Execute tasks"
                , AssistantMsg "" [ToolCall "t1" "fn1" (object []), ToolCall "t2" "fn2" (object [])]
                , ToolMsg "t1" "output1"
                , ToolMsg "t2" "output2"
                ]
            , crParams = defaultParams
            , crTools = []
            , crToolChoice = ToolAuto
            , crResponseFormat = RfText
            }
          body = buildMessagesBody cfg req
      case body of
        Object o -> case KM.lookup "messages" o of
          Just (Array msgs) -> do
            length msgs `shouldBe` 3
            let toolUserMsg = msgs V.! 2
            case toolUserMsg of
              Object to -> do
                KM.lookup "role" to `shouldBe` Just (String "user")
                case KM.lookup "content" to of
                  Just (Array blocks) -> length blocks `shouldBe` 2
                  other -> expectationFailure ("Expected content array, got: " <> show other)
              other -> expectationFailure ("Expected object, got: " <> show other)
          other -> expectationFailure ("Expected messages array, got: " <> show other)
        other -> expectationFailure ("Expected object body, got: " <> show other)

    it "Anthropic: streaming state machine combines text, tool_use, and token counts" $ do
      let antEvents =
            [ "{\"type\":\"message_start\",\"message\":{\"usage\":{\"input_tokens\":10}}}"
            , "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\"}}"
            , "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"Hello \"}}"
            , "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"world!\"}}"
            , "{\"type\":\"content_block_start\",\"index\":1,\"content_block\":{\"type\":\"tool_use\",\"id\":\"tu_99\",\"name\":\"search\"}}"
            , "{\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"term\\\": \\\"haskell\\\"}\"}}"
            , "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\"},\"usage\":{\"output_tokens\":20}}"
            , "{\"type\":\"message_stop\"}"
            ]
          (st, _) = foldl (\(s, _) e -> handleAnthropicEvent s e) (initialAntStreamState, []) antEvents
      case finalizeAnthropicStream st of
        Right resp -> do
          crspText resp `shouldBe` "Hello world!"
          crspFinishReason resp `shouldBe` FrToolUse
          crspUsage resp `shouldBe` Just (Usage 10 20)
          case crspToolCalls resp of
            [c] -> do
              toolCallId c `shouldBe` "tu_99"
              toolCallName c `shouldBe` "search"
              toolCallArguments c `shouldBe` object [Key.fromText "term" .= ("haskell" :: Text)]
            other -> expectationFailure ("Expected 1 tool call, got: " <> show (length other))
        Left err -> expectationFailure ("Finalize Anthropic stream failed: " <> show err)

  describe "Challenger Adversarial Suite: SSE Streaming & Multi-Byte UTF-8 Resilience" $ do
    it "Reassembles SSE stream fed 1 byte at a time" $ do
      let whole = "data: {\"token\": \"A\"}\n\ndata: {\"token\": \"B\"}\n\n"
          bytes = encodeUtf8 whole
          feedByte (p, evts) b =
            let (p', newEvts) = stepSSE p (BS.singleton b)
             in (p', evts ++ newEvts)
          (pFinal, collected) = BS.foldl feedByte (newSSEParser, []) bytes
          allEvents = collected ++ finishSSE pFinal
      allEvents `shouldBe` ["{\"token\": \"A\"}", "{\"token\": \"B\"}"]

    it "Handles 4-byte UTF-8 emoji and multi-byte characters split across chunk boundaries" $ do
      let msg = "data: {\"emoji\": \"🚀\", \"text\": \"漢字\"}\n\n"
          bs = encodeUtf8 msg
      forM_ [1 .. BS.length bs - 1] $ \splitAtIdx -> do
        let c1 = BS.take splitAtIdx bs
            c2 = BS.drop splitAtIdx bs
            (p1, e1) = stepSSE newSSEParser c1
            (p2, e2) = stepSSE p1 c2
            evts = e1 ++ e2 ++ finishSSE p2
        evts `shouldBe` ["{\"emoji\": \"🚀\", \"text\": \"漢字\"}"]

  describe "Challenger Adversarial Suite: JSON Extraction Resilience" $ do
    it "Extracts JSON from unclosed, uppercase, and untagged markdown code fences" $ do
      extractJSON "```\n{\"k\": 1}\n```" `shouldBe` Right (object [Key.fromText "k" .= (1 :: Int)])
      extractJSON "```json\n{\"k\": 2}" `shouldBe` Right (object [Key.fromText "k" .= (2 :: Int)])
      extractJSON "```json {\"k\": 3} ```" `shouldBe` Right (object [Key.fromText "k" .= (3 :: Int)])

    it "Handles JSON strings containing escaped quotes and curly braces" $ do
      extractJSON "{\"pattern\": \"a {b} c\", \"quote\": \"He said \\\"ok\\\"\"}"
        `shouldBe` Right (object [Key.fromText "pattern" .= ("a {b} c" :: Text), Key.fromText "quote" .= ("He said \"ok\"" :: Text)])

    it "Extracts primitive literals embedded within surrounding sentence punctuation" $ do
      extractJSON "The boolean flag is true." `shouldBe` Right (Bool True)
      extractJSON "Result code = 42;" `shouldBe` Right (Number 42)
      extractJSON "Calculated value is 3.14159." `shouldBe` Right (Number 3.14159)

    it "Fails cleanly with Left when given non-JSON or unbalanced strings" $ do
      extractJSON "no json here" `shouldSatisfy` isLeft
      extractJSON "{\"unbalanced\": [1, 2" `shouldSatisfy` isLeft
      extractJSON "" `shouldSatisfy` isLeft

  describe "Challenger Adversarial Suite: Mock Interpreter Multi-Threaded Concurrency" $ do
    it "Executes 20 concurrent mock interpreter threads without state leakage" $ do
      let numThreads = 20
      mvar <- newEmptyMVar
      replicateM_ numThreads $ forkIO $ do
        let script = [Right (textResp "isolated response")]
        (res, reqs, hist, _) <- runEff (runLLMMockFull script (generateText "isolated query"))
        putMVar mvar (res == "isolated response" && length reqs == 1 && length hist == 2)
      results <- replicateM numThreads (takeMVar mvar)
      results `shouldBe` replicate numThreads True
