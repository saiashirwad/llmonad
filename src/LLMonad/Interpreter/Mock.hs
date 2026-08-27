{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Pure in-memory mock interpreter for testing without network access.
module LLMonad.Interpreter.Mock (
    MockState (..),
    textResp,
    toolResp,
    structuredResp,
    runLLMMock,
    runLLMMockFull,
) where

import Control.Exception qualified as E
import Data.Aeson (Value, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import LLMonad.Core
import LLMonad.Types

-- | State tracking for the mock interpreter.
data MockState = MockState
    { mockResponses :: [Either LLMError CompletionResponse]
    , mockRequests :: [CompletionRequest]
    , mockHistory :: [ChatMessage]
    , mockSystem :: Maybe Text
    }
    deriving (Show)

-- | Construct a plain-text assistant response.
textResp :: Text -> CompletionResponse
textResp t =
    CompletionResponse
        { crspText = t
        , crspToolCalls = []
        , crspStructuredPayload = Nothing
        , crspFinishReason = FrStop
        , crspUsage = Just (Usage 1 1)
        }

-- | Construct an assistant response with tool calls.
toolResp :: [ToolCall] -> CompletionResponse
toolResp calls =
    CompletionResponse
        { crspText = ""
        , crspToolCalls = calls
        , crspStructuredPayload = Nothing
        , crspFinishReason = FrToolUse
        , crspUsage = Just (Usage 1 1)
        }

-- | Construct a response with structured JSON payload.
structuredResp :: Value -> CompletionResponse
structuredResp val =
    CompletionResponse
        { crspText = decodeUtf8With lenientDecode (LBS.toStrict (encode val))
        , crspToolCalls = []
        , crspStructuredPayload = Just val
        , crspFinishReason = FrStop
        , crspUsage = Just (Usage 1 1)
        }

{- | Run LLM computation against pre-scripted mock responses.
Returns computation result and recorded requests in chronological order.
-}
runLLMMock ::
    (IOE :> es) =>
    [Either LLMError CompletionResponse] ->
    Eff (LLM : es) a ->
    Eff es (a, [CompletionRequest])
runLLMMock script action = do
    let initState = MockState script [] [] Nothing
    (res, finalState) <- reinterpret_ (runState initState) mockHandler action
    pure (res, reverse (mockRequests finalState))

-- | Run LLM computation and return full final state.
runLLMMockFull ::
    (IOE :> es) =>
    [Either LLMError CompletionResponse] ->
    Eff (LLM : es) a ->
    Eff es (a, [CompletionRequest], [ChatMessage], Maybe Text)
runLLMMockFull script action = do
    let initState = MockState script [] [] Nothing
    (res, finalState) <- reinterpret_ (runState initState) mockHandler action
    pure (res, reverse (mockRequests finalState), mockHistory finalState, mockSystem finalState)

{- | Failures raise eagerly at the operation that consumed the script entry,
never lazily when the caller forces the response.
-}
mockHandler :: (IOE :> es) => EffectHandler_ LLM (State MockState : es)
mockHandler = \case
    GetHistory -> gets mockHistory
    SetHistory msgs -> modify (\s -> s{mockHistory = msgs})
    PushMessage msg -> modify (\s -> s{mockHistory = mockHistory s ++ [msg]})
    ClearHistory -> modify (\s -> s{mockHistory = []})
    GetSystem -> gets mockSystem
    SetSystem sys -> modify (\s -> s{mockSystem = Just sys})
    ClearSystem -> modify (\s -> s{mockSystem = Nothing})
    ChatRound p fmt specs choice -> do
        st <- get
        let req =
                CompletionRequest
                    { crModel = Model "mock-model"
                    , crSystem = mockSystem st
                    , crMessages = mockHistory st
                    , crParams = p
                    , crTools = specs
                    , crToolChoice = choice
                    , crResponseFormat = fmt
                    }
        modify (\s -> s{mockRequests = req : mockRequests s})
        case mockResponses st of
            [] -> liftIO (E.throwIO (ApiError 500 "runLLMMock: response script exhausted"))
            (Left err : rest) -> do
                modify (\s -> s{mockResponses = rest})
                liftIO (E.throwIO err)
            (Right resp : rest) -> do
                let assistantMsg = AssistantMsg (crspText resp) (crspToolCalls resp)
                modify (\s -> s{mockResponses = rest, mockHistory = mockHistory s ++ [assistantMsg]})
                pure resp
    StreamRound p fmt specs cb -> do
        st <- get
        let req =
                CompletionRequest
                    { crModel = Model "mock-model"
                    , crSystem = mockSystem st
                    , crMessages = mockHistory st
                    , crParams = p
                    , crTools = specs
                    , crToolChoice = ToolAuto
                    , crResponseFormat = fmt
                    }
        modify (\s -> s{mockRequests = req : mockRequests s})
        case mockResponses st of
            [] -> liftIO (E.throwIO (ApiError 500 "runLLMMock: response script exhausted"))
            (Left err : rest) -> do
                modify (\s -> s{mockResponses = rest})
                liftIO (E.throwIO err)
            (Right resp : rest) -> do
                let assistantMsg = AssistantMsg (crspText resp) (crspToolCalls resp)
                modify (\s -> s{mockResponses = rest, mockHistory = mockHistory s ++ [assistantMsg]})
                -- The callback is plain IO, so both events are delivered eagerly
                -- in order before the round returns its terminal response.
                liftIO $ do
                    cb (SEText (crspText resp))
                    cb (SEFinished resp)
                pure resp
