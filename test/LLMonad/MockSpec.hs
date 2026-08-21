{-# LANGUAGE OverloadedStrings #-}

module LLMonad.MockSpec (spec) where

import Data.Aeson (object, (.=))
import Data.IORef
import Data.Text (Text)
import LLMonad.Error (LLMError (..))
import LLMonad.Mock
import LLMonad.Provider (Provider (..))
import LLMonad.Types
import Test.Hspec

dummyReq :: CompletionRequest
dummyReq =
  CompletionRequest
    { crModel = "mock-model"
    , crSystem = Nothing
    , crMessages = [UserMsg "test"]
    , crParams = defaultParams
    , crTools = []
    , crToolChoice = ToolAuto
    , crResponseFormat = RfText
    }

spec :: Spec
spec = do
  describe "Mock Interpreter (Tier 1: Feature Coverage)" $ do
    it "initializes with scripted responses" $ do
      m <- newMock [Right (textResp "first"), Right (textResp "second")]
      q <- readIORef (mockQueue m)
      length q `shouldBe` 2

    it "replays scripted text responses in FIFO order" $ do
      m <- newMock [Right (textResp "first"), Right (textResp "second")]
      let p = mockProvider m
      r1 <- providerComplete p dummyReq
      r2 <- providerComplete p dummyReq
      case (r1, r2) of
        (Right resp1, Right resp2) -> do
          crspText resp1 `shouldBe` "first"
          crspText resp2 `shouldBe` "second"
        _ -> expectationFailure "Expected successful completion responses"

    it "records all incoming CompletionRequests" $ do
      m <- newMock [Right (textResp "ok")]
      let p = mockProvider m
          req1 = dummyReq {crMessages = [UserMsg "msg 1"]}
      _ <- providerComplete p req1
      reqs <- readIORef (mockRequests m)
      case reqs of
        [r] -> crMessages r `shouldBe` [UserMsg "msg 1"]
        other -> expectationFailure ("expected 1 request, got: " <> show (length other))

    it "builds valid textResp with finish reason FrStop" $ do
      let resp = textResp "hello world"
      crspText resp `shouldBe` "hello world"
      crspFinishReason resp `shouldBe` FrStop
      crspToolCalls resp `shouldBe` []
      crspUsage resp `shouldBe` Just (Usage 1 1)

    it "builds valid toolResp with finish reason FrToolUse" $ do
      let calls = [ToolCall "id-1" "calculator" (object ["expr" .= ("2+2" :: Text)])]
          resp = toolResp calls
      crspText resp `shouldBe` ""
      crspFinishReason resp `shouldBe` FrToolUse
      crspToolCalls resp `shouldBe` calls

  describe "Mock Interpreter (Tier 2: Boundary & Corner Cases)" $ do
    it "returns ApiError 500 when mock queue is exhausted" $ do
      m <- newMock []
      let p = mockProvider m
      res <- providerComplete p dummyReq
      case res of
        Left (ApiError 500 msg) -> msg `shouldBe` "mock queue empty"
        other -> expectationFailure ("Expected ApiError 500, got: " <> show other)

    it "propagates custom scripted LLM errors" $ do
      m <- newMock [Left (ApiError 429 "Rate limit exceeded")]
      let p = mockProvider m
      res <- providerComplete p dummyReq
      case res of
        Left (ApiError 429 msg) -> msg `shouldBe` "Rate limit exceeded"
        other -> expectationFailure ("Expected ApiError 429, got: " <> show other)

    it "handles requests with empty messages safely" $ do
      m <- newMock [Right (textResp "response to empty")]
      let p = mockProvider m
          reqEmpty = dummyReq {crMessages = []}
      res <- providerComplete p reqEmpty
      case res of
        Right resp -> crspText resp `shouldBe` "response to empty"
        Left err -> expectationFailure ("Unexpected error on empty message: " <> show err)

    it "streams scripted responses via providerStream emitting SEFinished" $ do
      m <- newMock [Right (textResp "streamed result")]
      let p = mockProvider m
      evtsRef <- newIORef []
      res <- providerStream p dummyReq (\ev -> modifyIORef' evtsRef (ev :))
      case res of
        Right resp -> do
          crspText resp `shouldBe` "streamed result"
          evts <- readIORef evtsRef
          case evts of
            [SEFinished r] -> crspText r `shouldBe` "streamed result"
            other -> expectationFailure ("Expected [SEFinished event], got: " <> show other)
        Left err -> expectationFailure ("Stream failed: " <> show err)

    it "maintains isolation across independent mock instances" $ do
      m1 <- newMock [Right (textResp "instance 1")]
      m2 <- newMock [Right (textResp "instance 2")]
      r1 <- providerComplete (mockProvider m1) dummyReq
      r2 <- providerComplete (mockProvider m2) dummyReq
      case (r1, r2) of
        (Right resp1, Right resp2) -> do
          crspText resp1 `shouldBe` "instance 1"
          crspText resp2 `shouldBe` "instance 2"
        _ -> expectationFailure "Expected independent instance success"
