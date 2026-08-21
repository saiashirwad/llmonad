{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module LLMonad.CoreSpec (spec) where

import Data.Aeson (FromJSON, object, (.=))
import Data.IORef
import Data.Text (Text)
import Effectful
import GHC.Generics (Generic)
import LLMonad
import Test.Hspec

data Pet = Pet
  { species :: Text
  , legs :: Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToSchema)

data CalcArgs = CalcArgs
  { a :: Int
  , b :: Int
  }
  deriving (Show, Generic, FromJSON, ToSchema)

runScript ::
  [Either LLMError CompletionResponse] ->
  Eff '[LLM, IOE] x ->
  IO (x, [ChatMessage], [CompletionRequest], Maybe Text)
runScript script act = do
  (x, reqs, hist, mSys) <- runEff (runLLMMockFull script act)
  pure (x, hist, reqs, mSys)

spec :: Spec
spec = do
  describe "Core Monad & History (Tier 1: Feature Coverage)" $ do
    it "generateText returns the reply and records conversation history" $ do
      (reply, hist, reqs, _) <-
        runScript [Right (textResp "hello")] (generateText "hi")
      reply `shouldBe` "hello"
      hist `shouldBe` [UserMsg "hi", AssistantMsg "hello" []]
      length reqs `shouldBe` 1

    it "later calls see accumulated prior conversation history" $ do
      (_, _, reqs, _) <-
        runScript
          [Right (textResp "first"), Right (textResp "second")]
          (generateText "one" >> generateText "two")
      length reqs `shouldBe` 2
      crMessages (reqs !! 0) `shouldBe` [UserMsg "one"]
      crMessages (reqs !! 1) `shouldBe` [UserMsg "one", AssistantMsg "first" [], UserMsg "two"]

    it "setSystem configures the system prompt for all subsequent requests" $ do
      (_, _, reqs, _) <-
        runScript [Right (textResp "ok")] (setSystem "be terse" >> generateText "hi")
      map crSystem reqs `shouldSatisfy` all (== Just "be terse")

    it "getHistory, setHistory, getSystem, setSystem allow state snapshotting and restoring" $ do
      let priorHist = [UserMsg "prior 1", AssistantMsg "reply 1" []]
      ((hRead, sRead), histFinal, _, sysFinal) <-
        runScript [] $ do
          setSystem "custom system"
          setHistory priorHist
          h <- getHistory
          s <- getSystem
          pure (h, s)
      hRead `shouldBe` priorHist
      sRead `shouldBe` Just "custom system"
      histFinal `shouldBe` priorHist
      sysFinal `shouldBe` Just "custom system"

    it "withTrace captures request and response lifecycle events" $ do
      tracesRef <- newIORef []
      let act = withTrace (\t -> modifyIORef' tracesRef (t :)) (generateText "ping")
      (reply, _, _, _) <- runScript [Right (textResp "pong")] act
      reply `shouldBe` "pong"
      traces <- readIORef tracesRef
      length traces `shouldBe` 2
      case reverse traces of
        [TraceRequest {}, TraceResponse {}] -> pure ()
        other -> expectationFailure ("Unexpected trace events: " <> show other)

    it "embed and embedShow render data into prompt strings" $ do
      embed (object ["key" .= ("val" :: Text)]) `shouldBe` "{\"key\":\"val\"}"
      embedShow (42 :: Int) `shouldBe` "42"

  describe "Core Monad & History (Tier 2: Boundary & Corner Cases)" $ do
    it "handles empty string user prompt without failure" $ do
      (reply, hist, reqs, _) <- runScript [Right (textResp "ok")] (generateText "")
      reply `shouldBe` "ok"
      hist `shouldBe` [UserMsg "", AssistantMsg "ok" []]
      case reqs of
        [r] -> crMessages r `shouldBe` [UserMsg ""]
        other -> expectationFailure ("expected 1 request, got: " <> show (length other))

    it "preserves complex unicode and multiline content in system prompts" $ do
      let unicodeSystem = "System: \n🚀 λ-calculus \t\r\n 漢字"
      (_, _, reqs, mSys) <- runScript [Right (textResp "done")] (setSystem unicodeSystem >> generateText "query")
      mSys `shouldBe` Just unicodeSystem
      case reqs of
        [r] -> crSystem r `shouldBe` Just unicodeSystem
        other -> expectationFailure ("expected 1 request, got: " <> show (length other))

    it "clearSystem resets system prompt to Nothing" $ do
      (_, _, reqs, mSys) <-
        runScript
          [Right (textResp "ok")]
          (setSystem "initial system" >> clearSystem >> generateText "test")
      mSys `shouldBe` Nothing
      case reqs of
        [r] -> crSystem r `shouldBe` Nothing
        other -> expectationFailure ("expected 1 request, got: " <> show (length other))

    it "attempt catches LLMError and preserves conversation history before error" $ do
      (_, hist, _, _) <-
        runScript
          [ Left (ApiError 500 "server down")
          , Right (textResp "recovered")
          ]
          ( do
              _ <- attempt (generateText "first try")
              generateText "second try"
          )
      hist `shouldBe`
        [ UserMsg "first try"
        , UserMsg "second try"
        , AssistantMsg "recovered" []
        ]

    it "retry recovers from transient 500 / 503 errors within limit" $ do
      (reply, _, reqs, _) <-
        runScript
          [ Left (ApiError 500 "internal error")
          , Left (ApiError 503 "service unavailable")
          , Right (textResp "success")
          ]
          (retry 3 (generateText "request"))
      reply `shouldBe` "success"
      length reqs `shouldBe` 3

    it "retry does not retry on permanent 400 bad request error" $ do
      (res, _, reqs, _) <-
        runScript
          [ Left (ApiError 400 "malformed request")
          , Right (textResp "never reached")
          ]
          (attempt (retry 3 (generateText "request")))
      case res of
        Left (ApiError 400 msg) -> msg `shouldBe` "malformed request"
        other -> expectationFailure ("Expected permanent error, got: " <> show other)
      length reqs `shouldBe` 1

    it "pushMessage appends arbitrary ChatMessage types to conversation" $ do
      let customToolMsg = ToolMsg "tool-1" "42"
      (_, hist, _, _) <-
        runScript [] $ do
          pushMessage (UserMsg "custom user")
          pushMessage customToolMsg
      hist `shouldBe` [UserMsg "custom user", customToolMsg]

    it "overrideParams properly layers callsite parameters over default config" $ do
      let pBase = defaultParams {paramTemperature = Just 0.7, paramMaxTokens = Just 100}
          pOver = defaultParams {paramTemperature = Just 0.2}
          pRes = pBase `overrideParams` pOver
      paramTemperature pRes `shouldBe` Just 0.2
      paramMaxTokens pRes `shouldBe` Just 100
