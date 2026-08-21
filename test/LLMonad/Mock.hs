{-# LANGUAGE OverloadedStrings #-}

-- | A scripted, offline provider for testing the DSL without network.
module LLMonad.Mock
  ( Mock (..)
  , newMock
  , mockProvider
  , mockProviderName
  , textResp
  , toolResp
  ) where

import Data.IORef
import Data.Text (Text)
import LLMonad.Error (LLMError (..))
import LLMonad.Provider
import LLMonad.Types

-- | Scripted responses (consumed head-first) and captured requests
-- (newest first).
data Mock = Mock
  { mockQueue :: IORef [Either LLMError CompletionResponse]
  , mockRequests :: IORef [CompletionRequest]
  }

newMock :: [Either LLMError CompletionResponse] -> IO Mock
newMock script =
  Mock
    <$> newIORef script
    <*> newIORef []

-- | A provider that replays the script. When the script runs dry it
-- answers with a 500 'ApiError'.
mockProvider :: Mock -> Provider
mockProvider m =
  Provider
    { providerName = mockProviderName
    , providerStructured = StructuredNative
    , providerComplete = completeMock m
    , providerStream = \req cb -> do
        r <- completeMock m req
        case r of
          Left e -> pure (Left e)
          Right resp -> do
            cb (SEFinished resp)
            pure (Right resp)
    }

mockProviderName :: Text
mockProviderName = "mock"

completeMock :: Mock -> CompletionRequest -> IO (Either LLMError CompletionResponse)
completeMock m req = do
  modifyIORef' (mockRequests m) (req :)
  next <- atomicModifyIORef' (mockQueue m) take1
  pure (maybe (Left (ApiError 500 "mock queue empty")) id next)
  where
    take1 [] = ([], Nothing)
    take1 (x : xs) = (xs, Just x)

-- | A plain-text assistant response.
textResp :: Text -> CompletionResponse
textResp t =
  CompletionResponse
    { crspText = t
    , crspToolCalls = []
    , crspStructuredPayload = Nothing
    , crspFinishReason = FrStop
    , crspUsage = Just (Usage 1 1)
    }

-- | An assistant response that requests tool calls.
toolResp :: [ToolCall] -> CompletionResponse
toolResp calls =
  CompletionResponse
    { crspText = ""
    , crspToolCalls = calls
    , crspStructuredPayload = Nothing
    , crspFinishReason = FrToolUse
    , crspUsage = Just (Usage 1 1)
    }
