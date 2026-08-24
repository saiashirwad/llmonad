{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module LLMonad.StreamingSpec (spec) where

import Data.IORef
import Data.Text (Text)
import Effectful
import LLMonad
import Test.Hspec

runMockStream ::
    [Either LLMError CompletionResponse] ->
    Eff '[LLM, IOE] a ->
    IO (a, [CompletionRequest])
runMockStream script act = do
    (res, reqs, _, _) <- runEff (runLLMMockFull script act)
    pure (res, reqs)

spec :: Spec
spec = do
    describe "Streaming & SSE (R5 / F5.3)" $ do
        it "streamSSE invokes callback on stream events and returns final text" $ do
            chunksRef <- newIORef ([] :: [Text])
            let script = [Right (textResp "Streamed response content")]
                action = streamSSE defaultParams [] (\chunk -> modifyIORef' chunksRef (chunk :))
            (finalText, reqs) <- runMockStream script action
            finalText `shouldBe` "Streamed response content"
            length reqs `shouldBe` 1
            chunks <- readIORef chunksRef
            chunks `shouldBe` ["Streamed response content"]

        it "streamText helper streams text deltas and returns the complete text" $ do
            chunksRef <- newIORef ([] :: [Text])
            let script = [Right (textResp "Hello, world!")]
                action = streamText (\chunk -> modifyIORef' chunksRef (chunk :)) "Say hello"
            (finalText, _) <- runMockStream script action
            finalText `shouldBe` "Hello, world!"
            chunks <- readIORef chunksRef
            chunks `shouldBe` ["Hello, world!"]

        it "streamTextWith passes custom sampling parameters" $ do
            let customParams = defaultParams{paramTemperature = Just 0.7, paramMaxTokens = Just 128}
                script = [Right (textResp "Custom param response")]
                action = streamTextWith customParams (const (pure ())) "Generate with params"
            (finalText, reqs) <- runMockStream script action
            finalText `shouldBe` "Custom param response"
            case reqs of
                (req : _) -> do
                    paramTemperature (crParams req) `shouldBe` Just 0.7
                    paramMaxTokens (crParams req) `shouldBe` Just 128
                [] -> expectationFailure "Expected at least one request"
