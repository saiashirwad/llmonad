{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Core dynamic effect for session event sourcing, audit logging, and replay.
module LLMonad.Journal (
    -- * The Journal Effect
    Journal (..),

    -- * Core Smart Constructors
    recordEvent,
    getEvents,
    clearEvents,
    recordUserMsg,
    recordModelTurn,
    recordModelTurnWithCalls,
    recordToolCall,
    recordToolCallWithId,
    recordToolResult,
    recordMetrics,
    recordTurnStart,
    recordTurnFinish,

    -- * Session Management & Replay Analytics
    resumeSession,
    resumeSessionWorld,
    replayAudit,
    replayAuditSummary,
    loadJournalFile,
    loadJournalFileWorld,
    loadJournalText,
    reconstructChatHistory,

    -- * Re-exported Types
    module LLMonad.Journal.Types,
) where

import Data.Aeson (Value)
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import LLMonad.Journal.Replay
import LLMonad.Journal.Types

-- | Dynamic effect for capturing session events, metrics, and tool calls.
data Journal :: Effect where
    RecordEvent :: JournalEvent -> Journal m ()
    GetEvents :: Journal m [JournalEvent]
    ClearEvents :: Journal m ()

type instance DispatchOf Journal = Dynamic

-- | Record a single 'JournalEvent' in the session journal.
recordEvent :: (Journal :> es) => JournalEvent -> Eff es ()
recordEvent ev = send (RecordEvent ev)

-- | Retrieve all recorded 'JournalEvent's in chronological order.
getEvents :: (Journal :> es) => Eff es [JournalEvent]
getEvents = send GetEvents

-- | Clear all recorded events from the current journal session.
clearEvents :: (Journal :> es) => Eff es ()
clearEvents = send ClearEvents

-- | Convenience helper to record a user message.
recordUserMsg :: (Journal :> es) => Text -> Eff es ()
recordUserMsg txt = recordEvent (UserMsg txt)

-- | Convenience helper to record an assistant model turn without tool calls.
recordModelTurn :: (Journal :> es) => Text -> Eff es ()
recordModelTurn txt = recordEvent (ModelTurn txt [])

-- | Convenience helper to record an assistant model turn with tool calls.
recordModelTurnWithCalls :: (Journal :> es) => Text -> [ToolCall] -> Eff es ()
recordModelTurnWithCalls txt calls = recordEvent (ModelTurn txt calls)

-- | Convenience helper to record a tool invocation where toolCallId equals toolName.
recordToolCall :: (Journal :> es) => Text -> Value -> Eff es ()
recordToolCall name args = recordEvent (ToolInvoked name name args)

-- | Convenience helper to record a tool invocation with explicit provider toolCallId.
recordToolCallWithId :: (Journal :> es) => Text -> Text -> Value -> Eff es ()
recordToolCallWithId cid name args = recordEvent (ToolInvoked cid name args)

-- | Convenience helper to record a tool execution result.
recordToolResult :: (Journal :> es) => Text -> ToolResult -> Eff es ()
recordToolResult cid res = recordEvent (ToolCompleted cid res)

-- | Convenience helper to record model execution metrics.
recordMetrics :: (Journal :> es) => ModelMetrics -> Eff es ()
recordMetrics mm = recordEvent (MetricsReported mm)

-- | Convenience helper to record the start of a turn.
recordTurnStart :: (Journal :> es) => Text -> Eff es ()
recordTurnStart tid = recordEvent (TurnStarted tid)

-- | Convenience helper to record the completion of a turn.
recordTurnFinish :: (Journal :> es) => Text -> Eff es ()
recordTurnFinish tid = recordEvent (TurnFinished tid)
