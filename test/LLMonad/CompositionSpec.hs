{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module LLMonad.CompositionSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad ((>=>))
import Data.Aeson (toJSON)
import Data.IORef
import Data.List (sort)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import LLMonad
import Test.Hspec

spec :: Spec
spec = do
  describe "configured agents" $ do
    it "keeps model selection outside the workflow" $ do
      requests <- newIORef []
      let provider = recordingProvider requests modelAnswer
          writer = bind (model provider "writer-model") noTools definition
          reviewer = bind (model provider "reviewer-model") noTools definition

      result <- runEff (pipeline writer reviewer "draft")

      result `shouldBe` "reviewer-model(writer-model(draft))"
      seenModels <- sort . map crModel <$> readIORef requests
      seenModels `shouldBe` ["reviewer-model", "writer-model"]

    it "starts a fresh conversation for each invocation" $ do
      requests <- newIORef []
      let provider = recordingProvider requests (const "ok")
          configuredAgent = bind (model provider "test-model") noTools definition

      _ <- runEff $ concurrently (invoke configuredAgent "one") (invoke configuredAgent "two")

      seen <- readIORef requests
      map crMessages seen `shouldMatchList` [[UserMsg "one"], [UserMsg "two"]]

    it "keeps history only in an explicit session" $ do
      requests <- newIORef []
      let provider = recordingProvider requests (T.pack . show . length . crMessages)
          configuredAgent = bind (model provider "test-model") noTools definition

      (first, second) <- runEff $ do
        session <- start configuredAgent
        first <- continue session "one"
        second <- continue session "two"
        pure (first, second)

      first `shouldBe` "1"
      second `shouldBe` "3"
      messageCounts <- sort . map (length . crMessages) <$> readIORef requests
      messageCounts `shouldBe` [1, 3]

    it "composes toolsets" $ do
      requests <- newIORef []
      let provider = recordingProvider requests (const "ok")
          configuredAgent = bind (model provider "test-model") (tools [firstTool] <> tools [secondTool]) definition

      _ <- runEff (invoke configuredAgent "use tools")

      seen <- readIORef requests
      case seen of
        [request] -> sort (map toolSpecName (crTools request)) `shouldBe` ["first", "second"]
        _ -> expectationFailure "expected one model request"

    it "rejects duplicate tool names" $ do
      requests <- newIORef []
      let provider = recordingProvider requests (const "ok")
          configuredAgent = bind (model provider "test-model") (tools [firstTool, firstTool]) definition

      result <- try @LLMError (runEff (invoke configuredAgent "use tools"))

      case result of
        Left (AgentConfigurationError message) ->
          message `shouldSatisfy` (T.isInfixOf "duplicate tool names: first")
        Left other -> expectationFailure ("unexpected error: " <> show other)
        Right _ -> expectationFailure "expected duplicate tool names to fail"

    it "decodes structured agent output" $ do
      let structuredDefinition :: AgentDef Text Int
          structuredDefinition = structuredAgent "Return the number." id
          configuredAgent =
            bind
              (mockModel [Right (structuredResp (toJSON (42 :: Int)))])
              noTools
              structuredDefinition

      result <- runEff (invoke configuredAgent "answer")

      result `shouldBe` 42

  describe "workflow concurrency" $ do
    it "limits concurrent work and preserves result order" $ do
      active <- newIORef (0 :: Int)
      peak <- newIORef (0 :: Int)

      results <- runEff $ mapConcurrentlyN 2 (measuredTask active peak) [1 .. 8]

      results `shouldBe` map (* 2) [1 .. 8]
      readIORef peak `shouldReturn` 2

    it "rejects a non-positive concurrency limit" $ do
      result <- try @WorkflowError (runEff (mapConcurrentlyN 0 pure [1 :: Int]))
      result `shouldBe` Left (InvalidConcurrencyLimit 0)

definition :: AgentDef Text Text
definition = textAgent "Give a short answer." id

pipeline :: Agent es Text Text -> Agent es Text Text -> Text -> Eff es Text
pipeline first second = invoke first >=> invoke second

recordingProvider :: IORef [CompletionRequest] -> (CompletionRequest -> Text) -> Provider
recordingProvider requests answer =
  Provider
    { providerName = "recording"
    , providerStructured = StructuredNative
    , providerComplete = complete
    , providerStream = nonStreamingFallback complete
    }
  where
    complete request = do
      atomicModifyIORef' requests (\seen -> (request : seen, ()))
      pure (Right (textResp (answer request)))

modelAnswer :: CompletionRequest -> Text
modelAnswer request =
  unModel (crModel request) <> "(" <> latestUserMessage request <> ")"

latestUserMessage :: CompletionRequest -> Text
latestUserMessage request =
  case listToMaybe [message | UserMsg message <- reverse (crMessages request)] of
    Nothing -> ""
    Just message -> message

firstTool :: Tool (Eff '[IOE])
firstTool = toolSync "first" "Return the input." (id :: Text -> Text)

secondTool :: Tool (Eff '[IOE])
secondTool = toolSync "second" "Return the input." (id :: Text -> Text)

measuredTask :: IORef Int -> IORef Int -> Int -> Eff '[IOE] Int
measuredTask active peak value = do
  current <- liftIO $ atomicModifyIORef' active (\count -> let next = count + 1 in (next, next))
  liftIO $ atomicModifyIORef' peak (\highest -> (max highest current, ()))
  liftIO (threadDelay 20000)
  liftIO $ atomicModifyIORef' active (\count -> (count - 1, ()))
  pure (value * 2)
