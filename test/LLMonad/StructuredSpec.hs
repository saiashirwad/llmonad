{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LLMonad.StructuredSpec (spec) where

import Control.Exception (evaluate, try)
import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import GHC.Generics (Generic)
import LLMonad
import Test.Hspec

data UserProfile = UserProfile
  { username :: Text
  , email :: Text
  , karma :: Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

runMockFull ::
  [Either LLMError CompletionResponse] ->
  Eff '[LLM, IOE] a ->
  IO (a, [CompletionRequest], [ChatMessage], Maybe Text)
runMockFull script act = do
  (res, reqs, hist, sys) <- runEff (runLLMMockFull script act)
  _ <- evaluate res
  pure (res, reqs, hist, sys)

spec :: Spec
spec = do
  describe "Structured Output & Extraction (R3 / F2.3)" $ do
    describe "askStructured" $ do
      it "extracts typed record from crspStructuredPayload" $ do
        let expected = UserProfile "alice" "alice@example.com" 100
            script = [Right (structuredResp (toJSON expected))]
        (res, reqs, _, _) <- runMockFull script (askStructured @UserProfile "Get profile")
        res `shouldBe` expected
        length reqs `shouldBe` 1

      it "extracts typed record from crspText with markdown code fence" $ do
        let jsonStr = "```json\n{\"username\":\"bob\",\"email\":\"bob@test.com\",\"karma\":50}\n```"
            resp = textResp jsonStr
            script = [Right resp]
        (res, reqs, _, _) <- runMockFull script (askStructured @UserProfile "Get profile")
        res `shouldBe` UserProfile "bob" "bob@test.com" 50
        length reqs `shouldBe` 1

      it "fails with DecodeError on unparseable response" $ do
        let script = [Right (textResp "Not a JSON at all")]
        r <- try (runMockFull script (askStructured @UserProfile "Get profile"))
        case r of
          Left (DecodeError _ raw) -> raw `shouldBe` "Not a JSON at all"
          Left other -> expectationFailure ("Expected DecodeError, got: " <> show other)
          Right _ -> expectationFailure "Expected DecodeError exception"

    describe "extractWithRetry" $ do
      it "succeeds on first attempt without extra retries" $ do
        let expected = UserProfile "carol" "carol@example.com" 75
            script = [Right (structuredResp (toJSON expected))]
        (res, reqs, hist, _) <- runMockFull script (extractWithRetry @UserProfile 3 "Extract profile")
        res `shouldBe` expected
        length reqs `shouldBe` 1
        length hist `shouldBe` 2 -- UserMsg + AssistantMsg

      it "recovers on second attempt and sends feedback prompt" $ do
        let badResp = textResp "Invalid JSON {username: carol}"
            goodResp = structuredResp (toJSON (UserProfile "carol" "carol@example.com" 75))
            script = [Right badResp, Right goodResp]
        (res, reqs, hist, _) <- runMockFull script (extractWithRetry @UserProfile 3 "Extract profile")
        res `shouldBe` UserProfile "carol" "carol@example.com" 75
        length reqs `shouldBe` 2
        -- Verify feedback message was sent to the model
        let userMsgs = [msg | UserMsg msg <- hist]
        length userMsgs `shouldBe` 2
        last userMsgs `shouldSatisfy` T.isInfixOf "Your previous response could not be decoded"

      it "throws DecodeError when all retries are exhausted" $ do
        let badResp = textResp "Garbage text"
            script = [Right badResp, Right badResp, Right badResp]
        r <- try (runMockFull script (extractWithRetry @UserProfile 3 "Extract profile"))
        case r of
          Left (DecodeError _ raw) -> raw `shouldBe` "Garbage text"
          Left other -> expectationFailure ("Expected DecodeError, got: " <> show other)
          Right _ -> expectationFailure "Expected DecodeError exception"
