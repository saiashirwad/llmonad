{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

{- | Higher-order middleware for tracing and telemetry.

Attach to a single agent's runtime with 'traced', or scope over an entire
effect block with 'withTrace'. Both spellings share 'traceHandler'.
-}
module LLMonad.Middleware.Trace (
    Trace (..),
    withTrace,
    traced,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Exception qualified as E
import LLMonad.Core
import LLMonad.Middleware (Middleware (..))
import LLMonad.Model (ModelRuntime (..))
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

{- | Handler interposing tracing on the 'LLM' effect.

Requests and responses are emitted around the wrapped interpreter; a failing
round emits 'TraceError' and rethrows unchanged.
-}
traceHandler :: forall es. (LLM :> es, IOE :> es) => (Trace -> IO ()) -> EffectHandler_ LLM es
traceHandler emitTrace = \case
    ChatRound p fmt specs choice -> tracedRound (send (ChatRound p fmt specs choice))
    StreamRound p fmt specs cb -> tracedRound (send (StreamRound p fmt specs cb))
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
    -- One request/response emission shared by both round kinds.
    tracedRound ::
        (LLM :> es, IOE :> es) =>
        Eff es CompletionResponse ->
        Eff es CompletionResponse
    tracedRound action = do
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
            action
                `E.catch` \(err :: LLMError) -> do
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

    findToolName cid msgs =
        case [toolCallName c | AssistantMsg _ calls <- reverse msgs, c <- calls, toolCallId c == cid] of
            (n : _) -> Just n
            [] -> Nothing

-- | Tracing as first-class model middleware; attach to individual runtimes.
traced :: (IOE :> es) => (Trace -> IO ()) -> Middleware es
traced emitTrace = Middleware $ \(ModelRuntime run) ->
    ModelRuntime $ \action ->
        run (interpose_ (traceHandler emitTrace) action)

-- | Interpose telemetry tracing on the LLM effect.
withTrace :: (LLM :> es, IOE :> es) => (Trace -> IO ()) -> Eff es a -> Eff es a
withTrace emitTrace = interpose_ (traceHandler emitTrace)
