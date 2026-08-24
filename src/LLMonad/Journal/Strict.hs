{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{- | Strict replay: rerun a workflow against a recorded journal and fail the
moment it asks for something the recording does not contain. Where
"LLMonad.Interpreter.Mock" improvises when its script runs dry, strict
replay raises 'ReplayDivergence' instead — which turns recorded sessions
into regression tests that detect behavioral drift.
-}
module LLMonad.Journal.Strict (
    -- * Parsed recording (pure data)
    ReplayScript (..),
    RecordedTurn (..),
    RecordedToolCall (..),
    extractReplayScript,

    -- * Interpreters at the edges
    strictReplayRuntime,
    strictReplayToolset,
) where

import Control.Exception (throwIO)
import Data.Aeson (Value, object, (.=))
import Data.IORef
import Data.List (mapAccumL, nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic (EffectHandler, interpret)
import LLMonad.Core (LLM (..))
import LLMonad.Error (LLMError (..))
import LLMonad.Journal.Types hiding (ToolResult)
import LLMonad.Model (ModelRuntime (..))
import LLMonad.Tools
import LLMonad.Types

-- | One recorded model turn: the assistant text and the tool calls it asked for.
data RecordedTurn = RecordedTurn
    { rtText :: Text
    , rtToolCalls :: [ToolCall]
    }
    deriving (Eq, Show)

{- | One recorded tool invocation with the result it was given. The result is
absent when the journal holds the invocation but no completion for it.
-}
data RecordedToolCall = RecordedToolCall
    { rtcCallId :: Text
    , rtcName :: Text
    , rtcArguments :: Value
    , rtcResult :: Maybe ToolResult
    }
    deriving (Eq, Show)

{- | A recording parsed into the two channels strict replay checks.

The channels are matched independently, each in its own recorded order:
model turns in the order the workflow requests them, tool calls in the
order the agent executes them. Interleaving between the channels is not
verified — a drifted workflow that swaps which round called a tool while
keeping both per-channel orders still replays clean. Sessions today record
sequentially; concurrent agents make expectation order nondeterministic
and are outside what strict replay promises.
-}
data ReplayScript = ReplayScript
    { scriptTurns :: [RecordedTurn]
    , scriptToolCalls :: [RecordedToolCall]
    }
    deriving (Eq, Show)

{- | Parse journal events into a 'ReplayScript'.

Invocations pair with completions by call id, and the /n/th invocation of an
id takes the /n/th completion for that id. Pairing on the id alone would be
wrong for the ids this library itself writes: 'LLMonad.Journal.recordToolCall'
(and the 'ToolInvokedSimple' pattern) use the tool name as the call id, so
every invocation of one tool shares an id and would otherwise replay the
first result forever. Providers that mint unique ids have one completion per
id, where the two rules coincide. An invocation with no completion left to
take carries no result.
-}
extractReplayScript :: [JournalEvent] -> ReplayScript
extractReplayScript events =
    ReplayScript
        { scriptTurns = [RecordedTurn content calls | ModelTurn content calls <- events]
        , scriptToolCalls = catMaybes . snd $ mapAccumL takeResult completions events
        }
  where
    -- Completions per id, oldest first: fromListWith prepends, so undo it once.
    completions =
        Map.map reverse . Map.fromListWith (++) $
            [(callId, [result]) | ToolCompleted callId result <- events]

    takeResult unclaimed (ToolInvoked callId name args) =
        let (result, rest) = claimNext callId unclaimed
         in (rest, Just (RecordedToolCall callId name args result))
    takeResult unclaimed _ = (unclaimed, Nothing)

    -- This invocation claims the oldest completion still unclaimed for its id.
    claimNext callId unclaimed = case Map.lookup callId unclaimed of
        Just (result : later) -> (Just result, Map.insert callId later unclaimed)
        _ -> (Nothing, unclaimed)

--------------------------------------------------------------------------------
-- Model turns
--------------------------------------------------------------------------------

{- | How far into the recorded turns the session has read. The count keeps
rising past the end so a divergence can name the ordinal of the call that
actually ran off it, not just where the recording stopped.
-}
data TurnCursor = TurnCursor
    { turnsTaken :: !Int
    , turnsLeft :: [RecordedTurn]
    }

{- | A 'ModelRuntime' that answers model turns from a recording, in order.

The recording does not store requests, so requests cannot be compared — what
is verified is the shape of the conversation: how many turns happen and what
each says and asks for. Any turn past the end of the recording raises
@'ReplayDivergence'@ naming that turn's ordinal.

Two pieces of state, deliberately scoped differently:

* the position in the recording is session-wide, so a workflow with several
  agents replays against one continuous recording instead of restarting it
  per call (the deliberate difference from 'mockModel', whose script restarts
  each invocation);
* the conversation — history and system prompt — is per invocation, exactly
  as 'LLMonad.Model.model' and 'mockModel' scope it. Sharing it would let
  concurrent agents write into each other's conversations, which no other
  runtime does.

Requires 'IOE' because construction allocates the cursor invocations advance.
-}
strictReplayRuntime :: (IOE :> es) => ReplayScript -> IO (ModelRuntime es)
strictReplayRuntime script = do
    cursor <- newIORef (TurnCursor 0 (scriptTurns script))
    pure $ ModelRuntime $ \action -> do
        conversation <- liftIO (newIORef (Conversation [] Nothing))
        interpret (replayHandler cursor conversation) action

-- | One invocation's model session.
data Conversation = Conversation
    { convHistory :: [ChatMessage]
    , convSystem :: Maybe Text
    }

replayHandler ::
    forall es.
    (IOE :> es) =>
    IORef TurnCursor ->
    IORef Conversation ->
    EffectHandler LLM es
replayHandler cursor conversation _env = \case
    GetHistory -> liftIO (convHistory <$> readIORef conversation)
    SetHistory msgs -> edit (\c -> c{convHistory = msgs})
    PushMessage msg -> edit (append msg)
    ClearHistory -> edit (\c -> c{convHistory = []})
    GetSystem -> liftIO (convSystem <$> readIORef conversation)
    SetSystem sys -> edit (\c -> c{convSystem = Just sys})
    ClearSystem -> edit (\c -> c{convSystem = Nothing})
    ChatRound{} -> answer
    StreamRound _ _ _ cb -> do
        resp <- answer
        liftIO (cb (SEText (crspText resp)) >> cb (SEFinished resp))
        pure resp
  where
    edit f = liftIO (modifyIORef' conversation f)
    append msg c = c{convHistory = convHistory c ++ [msg]}

    -- Take the next recorded turn and record its reply, exactly as the live
    -- and mock interpreters do.
    answer :: Eff es CompletionResponse
    answer =
        liftIO (takeTurn cursor) >>= \case
            Left ordinal -> liftIO . throwIO . ReplayDivergence $ unrecordedTurnMsg ordinal
            Right turn -> do
                let resp = turnResponse turn
                edit (append (AssistantMsg (crspText resp) (crspToolCalls resp)))
                pure resp

{- | Advance the session cursor: the next recorded turn, or the ordinal of the
call that ran past the end of the recording.
-}
takeTurn :: IORef TurnCursor -> IO (Either Int RecordedTurn)
takeTurn cursor = atomicModifyIORef' cursor $ \c ->
    let taken = turnsTaken c + 1
     in case turnsLeft c of
            [] -> (c{turnsTaken = taken}, Left taken)
            (turn : later) -> (TurnCursor taken later, Right turn)

turnResponse :: RecordedTurn -> CompletionResponse
turnResponse t =
    CompletionResponse
        { crspText = rtText t
        , crspToolCalls = rtToolCalls t
        , crspStructuredPayload = Nothing
        , crspFinishReason = if null (rtToolCalls t) then FrStop else FrToolUse
        , crspUsage = Just (Usage 0 0)
        }

--------------------------------------------------------------------------------
-- Tool calls
--------------------------------------------------------------------------------

{- | Wrap a toolset so every invocation is answered from the recording instead
of executed: handlers are replaced by a lookup against the recorded tool
calls, consumed strictly in order, and no handler ever runs.

Each guard is one rule of the match:

* nothing left — the workflow called a tool the recording never did;
* different name next — both tools exist in the recording, but their order
  drifted;
* different arguments — same tool, same position, the model's arguments
  drifted;
* otherwise the recorded result plays back verbatim. An expectation without a
  recorded result returns a tool-error answer rather than diverging: the
  invocation itself was recorded, so there is nothing to fail on.

A tool the recording invoked but this toolset no longer advertises is added
back as a stub that diverges when called. Without it the agent loop answers
its own @unknown tool@ error and the run finishes green, so deleting a tool
the recording depended on would pass as a regression test. The stub only ever
fires when a recorded turn asks this agent for that tool, so wrapping a
toolset that never owned it stays harmless.

Like 'strictReplayRuntime', the cursor is session-wide across all invocations
sharing this toolset, and construction requires 'IOE'.
-}
strictReplayToolset :: (IOE :> es) => ReplayScript -> Toolset es -> IO (Toolset es)
strictReplayToolset script toolset = do
    cursor <- newIORef (scriptToolCalls script)
    pure . tools $ map (replayed cursor) advertised ++ map absentStub absentees
  where
    advertised = toolsetTools toolset
    advertisedNames = map (toolSpecName . toolSpec) advertised
    absentees =
        nub [name | name <- map rtcName (scriptToolCalls script), name `notElem` advertisedNames]

    replayed cursor t =
        t{toolRun = liftIO . replayedRun cursor (toolSpecName (toolSpec t))}

    absentStub name =
        Tool
            { toolSpec =
                ToolSpec
                    { toolSpecName = name
                    , toolSpecDescription = "strict replay: recorded, but absent from this toolset"
                    , toolSpecParameters = object ["type" .= ("object" :: Text), "properties" .= object []]
                    }
            , toolRun = \_args -> liftIO (throwIO (ReplayDivergence (absentToolMsg name)))
            }

replayedRun :: IORef [RecordedToolCall] -> Text -> Value -> IO ToolResult
replayedRun cursor name args = do
    expected <- atomicModifyIORef' cursor $ \case
        [] -> ([], Nothing)
        (e : rest) -> (rest, Just e)
    case expected of
        Nothing -> throwIO (ReplayDivergence (unrecordedToolMsg name))
        Just e
            | rtcName e /= name ->
                throwIO (ReplayDivergence (toolOrderMsg (rtcName e) name))
            | args /= rtcArguments e ->
                throwIO (ReplayDivergence (argsDriftMsg name))
            | otherwise ->
                pure (fromMaybe (Left (noResultMsg (rtcCallId e))) (rtcResult e))

--------------------------------------------------------------------------------
-- Divergence messages
--------------------------------------------------------------------------------

unrecordedTurnMsg :: Int -> Text
unrecordedTurnMsg n =
    "strict replay: model turn #" <> T.pack (show n) <> " was not in the recording"

unrecordedToolMsg :: Text -> Text
unrecordedToolMsg name =
    "strict replay: tool '" <> name <> "' was not in the recording"

absentToolMsg :: Text -> Text
absentToolMsg name =
    "strict replay: the recording called tool '" <> name <> "', which this toolset no longer has"

toolOrderMsg :: Text -> Text -> Text
toolOrderMsg expected actual =
    "strict replay: expected tool '"
        <> expected
        <> "' next; workflow called '"
        <> actual
        <> "'"

argsDriftMsg :: Text -> Text
argsDriftMsg name =
    "strict replay: tool '" <> name <> "' arguments differ from the recording"

noResultMsg :: Text -> Text
noResultMsg cid =
    "strict replay: no recorded result for tool call '" <> cid <> "'"
