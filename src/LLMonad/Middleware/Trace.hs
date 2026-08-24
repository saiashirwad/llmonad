{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Higher-order middleware for tracing and telemetry.
module LLMonad.Middleware.Trace (
    Trace (..),
    withTrace,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Exception qualified as E
import LLMonad.Core
import LLMonad.Types

-- | Telemetry trace event.
data Trace
    = TraceRequest
        { trModel :: Model
        , trSystem :: Maybe Text
        , trMessages :: [ChatMessage]
        }
    | TraceResponse
        { trText :: Text
        , trToolCalls :: [ToolCall]
        , trUsage :: Maybe Usage
        }
    | TraceToolExecuted
        { trToolName :: Text
        , trToolOk :: Bool
        , trToolSummary :: Text
        }
    | TraceError LLMError
    deriving (Show, Eq)

-- | Interpose telemetry tracing on the LLM effect.
withTrace :: (LLM :> es, IOE :> es) => (Trace -> IO ()) -> Eff es a -> Eff es a
withTrace emitTrace = interpose_ $ \case
    ChatRound p fmt specs choice -> do
        sys <- getSystem
        hist <- getHistory
        liftIO $
            emitTrace
                TraceRequest
                    { trModel = Model "active"
                    , trSystem = sys
                    , trMessages = hist
                    }
        resp <-
            send (ChatRound p fmt specs choice) `E.catch` \(err :: LLMError) -> do
                liftIO $ emitTrace (TraceError err)
                E.throwIO err
        liftIO $
            emitTrace
                TraceResponse
                    { trText = crspText resp
                    , trToolCalls = crspToolCalls resp
                    , trUsage = crspUsage resp
                    }
        pure resp
    StreamRound p fmt specs cb -> do
        sys <- getSystem
        hist <- getHistory
        liftIO $
            emitTrace
                TraceRequest
                    { trModel = Model "active"
                    , trSystem = sys
                    , trMessages = hist
                    }
        resp <-
            send (StreamRound p fmt specs cb) `E.catch` \(err :: LLMError) -> do
                liftIO $ emitTrace (TraceError err)
                E.throwIO err
        liftIO $
            emitTrace
                TraceResponse
                    { trText = crspText resp
                    , trToolCalls = crspToolCalls resp
                    , trUsage = crspUsage resp
                    }
        pure resp
    PushMessage msg -> do
        case msg of
            ToolMsg cid content -> do
                hist <- getHistory
                let mName = findToolName cid hist
                    tName = maybe "unknown" id mName
                    isOk = not ("\"error\"" `T.isInfixOf` content)
                liftIO $
                    emitTrace
                        TraceToolExecuted
                            { trToolName = tName
                            , trToolOk = isOk
                            , trToolSummary = T.take 100 content
                            }
            _ -> pure ()
        send (PushMessage msg)
    GetHistory -> send GetHistory
    SetHistory msgs -> send (SetHistory msgs)
    ClearHistory -> send ClearHistory
    GetSystem -> send GetSystem
    SetSystem sys -> send (SetSystem sys)
    ClearSystem -> send ClearSystem
  where
    findToolName cid msgs =
        case [toolCallName c | AssistantMsg _ calls <- reverse msgs, c <- calls, toolCallId c == cid] of
            (n : _) -> Just n
            [] -> Nothing
