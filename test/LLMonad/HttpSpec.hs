{-# LANGUAGE OverloadedStrings #-}

module LLMonad.HttpSpec (spec) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (AsyncException (..), throwIO, try)
import LLMonad.Internal.Http
  ( defaultTimeoutMicros
  , maxResponseBodyBytes
  , parseRetryAfter
  , timeoutFor
  , trySync
  )
import Network.HTTP.Client (responseTimeoutMicro)
import Test.Hspec

spec :: Spec
spec = do
  describe "timeoutFor calculation & overflow protection" $ do
    it "uses default timeout when Nothing is provided" $ do
      timeoutFor Nothing `shouldBe` responseTimeoutMicro defaultTimeoutMicros

    it "converts positive seconds to microseconds" $ do
      timeoutFor (Just 10) `shouldBe` responseTimeoutMicro 10000000

    it "clamps negative or zero seconds to 0" $ do
      timeoutFor (Just 0) `shouldBe` responseTimeoutMicro 0
      timeoutFor (Just (-10)) `shouldBe` responseTimeoutMicro 0

    it "guards against Int overflow with large seconds" $ do
      timeoutFor (Just (maxBound :: Int)) `shouldBe` responseTimeoutMicro maxBound

  describe "parseRetryAfter" $ do
    it "parses integer seconds" $ do
      parseRetryAfter [("Retry-After", "120")] `shouldBe` Just 120

    it "handles whitespace surrounding seconds" $ do
      parseRetryAfter [("Retry-After", "  45  ")] `shouldBe` Just 45

    it "handles seconds ending with 's' or 'S'" $ do
      parseRetryAfter [("Retry-After", "60s")] `shouldBe` Just 60
      parseRetryAfter [("Retry-After", "30S")] `shouldBe` Just 30

    it "handles decimal seconds by ceiling" $ do
      parseRetryAfter [("Retry-After", "1.5")] `shouldBe` Just 2
      parseRetryAfter [("Retry-After", "0.2")] `shouldBe` Just 1

    it "returns Nothing for missing header" $ do
      parseRetryAfter [] `shouldBe` Nothing

    it "returns Nothing for non-numeric unparseable format" $ do
      parseRetryAfter [("Retry-After", "invalid_header")] `shouldBe` Nothing

  describe "trySync async exception preservation" $ do
    it "catches synchronous exceptions as Left" $ do
      res <- trySync (throwIO (userError "sync failure"))
      case res of
        Left ex -> show ex `shouldSatisfy` (\s -> length s > 0)
        Right _ -> expectationFailure "expected Left from synchronous exception"

    it "passes successful IO values as Right" $ do
      res <- trySync (pure (42 :: Int))
      case res of
        Right val -> val `shouldBe` 42
        Left _ -> expectationFailure "expected Right 42"

    it "rethrows asynchronous exceptions (e.g. ThreadKilled) without swallowing" $ do
      doneMVar <- newEmptyMVar
      tid <- forkIO $ do
        res <- try (trySync (threadDelay 10000000))
        case res of
          Left ThreadKilled -> putMVar doneMVar (Right ())
          Left other -> putMVar doneMVar (Left ("unexpected exception: " <> show other))
          Right _ -> putMVar doneMVar (Left "swallowed ThreadKilled into Right")
      threadDelay 10000
      killThread tid
      result <- takeMVar doneMVar
      result `shouldBe` Right ()

  describe "maxResponseBodyBytes constant" $ do
    it "is set to 10 MiB" $ do
      maxResponseBodyBytes `shouldBe` 10 * 1024 * 1024
