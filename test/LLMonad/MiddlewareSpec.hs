{-# LANGUAGE OverloadedStrings #-}

module LLMonad.MiddlewareSpec (spec) where

import Data.IORef
import Data.Text (Text)
import Effectful
import LLMonad
import Test.Hspec

spec :: Spec
spec = do
    describe "model middleware" $ do
        it "caches one agent's answers without touching siblings" $ do
            requests <- newIORef []
            store <- newInMemoryCache
            let provider = recordingProvider requests (const "ok")
                cachedAgent =
                    mount (applyMiddleware (cached (Model "m") store) (model provider "m")) noTools definition
                plainAgent = mount (model provider "m") noTools definition

            (firstAnswer, secondAnswer, _) <- runEff $ do
                firstAnswer <- invoke cachedAgent "hi"
                secondAnswer <- invoke cachedAgent "hi"
                plain <- invoke plainAgent "hi"
                pure (firstAnswer, secondAnswer, plain)
            seen <- readIORef requests

            firstAnswer `shouldBe` "ok"
            secondAnswer `shouldBe` "ok"
            length seen `shouldBe` 2

        it "composes leftmost-outermost" $ do
            storeA <- newInMemoryCache
            storeB <- newInMemoryCache
            traces <- newIORef []
            let emit trace = atomicModifyIORef' traces (\ts -> (ts ++ [trace], ()))
                scripted = mockModel [Right (textResp "a"), Right (textResp "b")]
                agentFor mw = mount (applyMiddleware mw scripted) noTools definition

            -- Cache outermost: the second identical round never reaches trace.
            _ <- runEff $ do
                let agent = agentFor (cached (Model "cache-outer") storeA <> traced emit)
                _ <- invoke agent "q"
                invoke agent "q"
            cacheOuter <- countRequests <$> readIORef traces

            writeIORef traces []

            -- Trace outermost: both rounds pass through and are counted,
            -- even when the inner cache serves the second one.
            _ <- runEff $ do
                let agent = agentFor (traced emit <> cached (Model "trace-outer") storeB)
                _ <- invoke agent "q"
                invoke agent "q"
            traceOuter <- countRequests <$> readIORef traces

            cacheOuter `shouldBe` 1
            traceOuter `shouldBe` 2

definition :: AgentDef Text Text
definition = textAgent "Give a short answer." id

countRequests :: [Trace] -> Int
countRequests = length . filter isRequest
  where
    isRequest TraceRequest{} = True
    isRequest _ = False

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
