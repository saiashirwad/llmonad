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

import Control.Exception qualified as CE
import Data.Aeson (Value)
import Data.IORef
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic (EffectHandler, interpret)
import Effectful.Exception qualified as EE
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

{- | Parse journal events into a 'ReplayScript'. Each 'ToolInvoked' pairs with
the 'ToolCompleted' sharing its call id, wherever that completion appears;
invocations without one carry no result.
-}
extractReplayScript :: [JournalEvent] -> ReplayScript
extractReplayScript events =
    ReplayScript
        { scriptTurns = [RecordedTurn content calls | ModelTurn content calls <- events]
        , scriptToolCalls = mapMaybe invoked events
        }
  where
    results = [(cid, r) | ToolCompleted cid r <- events]
    invoked (ToolInvoked cid name args) =
        Just (RecordedToolCall cid name args (lookup cid results))
    invoked _ = Nothing

{- | A 'ModelRuntime' that answers model turns from a recording, in order.

The recording does not store requests, so requests cannot be compared — what
is verified is the shape of the conversation: how many turns happen and what
each says and asks for. Any turn past the end of the recording raises
@'ReplayDivergence'@ naming the first unrecorded ordinal.

The stream is session-wide: it persists across every agent invocation sharing
this runtime, so a workflow with several agents replays against one
continuous recording instead of restarting it per call (the deliberate
difference from 'mockModel', whose script restarts each invocation).
Requires 'IOE' because construction allocates the cursor the invocations
advance.
-}
strictReplayRuntime :: (IOE :> es) => ReplayScript -> IO (ModelRuntime es)
strictReplayRuntime script = do
    st <-
        newIORef
            ReplayState
                { rpRemaining = modelScript script
                , rpHistory = []
                , rpSystem = Nothing
                }
    pure $ ModelRuntime{runModelRuntime = interpret (replayHandler script st)}

{- | The scripted answers plus nothing else: exhaustion is handled at pop time
so the divergence can name where the recording ended.
-}
modelScript :: ReplayScript -> [Either LLMError CompletionResponse]
modelScript script =
    map (Right . turnResponse) (scriptTurns script)

data ReplayState = ReplayState
    { rpRemaining :: [Either LLMError CompletionResponse]
    , rpHistory :: [ChatMessage]
    , rpSystem :: Maybe Text
    }

replayHandler :: forall es. (IOE :> es) => ReplayScript -> IORef ReplayState -> EffectHandler LLM es
replayHandler script st _env = \case
    GetHistory -> liftIO (rpHistory <$> readIORef st)
    SetHistory msgs -> liftIO $ modifyIORef' st (\s -> s{rpHistory = msgs})
    PushMessage msg ->
        liftIO $ modifyIORef' st (\s -> s{rpHistory = rpHistory s ++ [msg]})
    ClearHistory -> liftIO $ modifyIORef' st (\s -> s{rpHistory = []})
    GetSystem -> liftIO (rpSystem <$> readIORef st)
    SetSystem sys -> liftIO $ modifyIORef' st (\s -> s{rpSystem = Just sys})
    ClearSystem -> liftIO $ modifyIORef' st (\s -> s{rpSystem = Nothing})
    ChatRound{} -> do
        resp <- popResponse
        liftIO $ modifyIORef' st (\s -> s{rpHistory = rpHistory s ++ [assistantOf resp]})
        pure resp
    StreamRound _ _ _ cb -> do
        resp <- popResponse
        liftIO $ do
            modifyIORef' st (\s -> s{rpHistory = rpHistory s ++ [assistantOf resp]})
            cb (SEText (crspText resp))
            cb (SEFinished resp)
        pure resp
  where
    -- Every answer comes off one cursor; an empty cursor means the workflow
    -- made more model calls than the recording holds. All excess calls name
    -- the first position past the recording, which is the truth for each of
    -- them.
    popResponse :: Eff es CompletionResponse
    popResponse = do
        popped <- liftIO $ atomicModifyIORef' st $ \s ->
            case rpRemaining s of
                [] -> (s, Nothing)
                (r : rest) -> (s{rpRemaining = rest}, Just r)
        case popped of
            Just (Right resp) -> pure resp
            Just (Left err) -> EE.throwIO err
            Nothing ->
                EE.throwIO . ReplayDivergence . unrecordedTurnMsg $
                    length (scriptTurns script) + 1

assistantOf :: CompletionResponse -> ChatMessage
assistantOf resp = AssistantMsg (crspText resp) (crspToolCalls resp)

turnResponse :: RecordedTurn -> CompletionResponse
turnResponse t =
    CompletionResponse
        { crspText = rtText t
        , crspToolCalls = rtToolCalls t
        , crspStructuredPayload = Nothing
        , crspFinishReason = if null (rtToolCalls t) then FrStop else FrToolUse
        , crspUsage = Just (Usage 0 0)
        }

{- | Wrap a toolset so every invocation is answered from the recording instead
of executed: same advertised specs, handlers replaced by a lookup against the
recorded tool calls, consumed strictly in order.

Each guard is one rule of the match:

* nothing left — the workflow called a tool the recording never did;
* different name next — both tools exist in the recording, but their order
  drifted;
* different arguments — same tool, same position, the model's arguments
  drifted;
* otherwise the recorded result plays back verbatim. An expectation without a
  recorded result returns a tool-error answer rather than diverging: the
  invocation itself was recorded, so there is nothing to fail on.

Like 'strictReplayRuntime', the cursor is session-wide across all invocations
sharing this toolset, and construction requires 'IOE'.
-}
strictReplayToolset :: (IOE :> es) => ReplayScript -> Toolset es -> IO (Toolset es)
strictReplayToolset script ts0 = do
    queue <- newIORef (scriptToolCalls script)
    pure . tools $ map (replayed queue) (toolsetTools ts0)
  where
    replayed queue t =
        t{toolRun = liftIO . replayedRun queue (toolSpecName (toolSpec t))}

replayedRun :: IORef [RecordedToolCall] -> Text -> Value -> IO ToolResult
replayedRun queue name args = do
    popped <- atomicModifyIORef' queue $ \case
        [] -> ([], Nothing)
        (e : rest) -> (rest, Just e)
    case popped of
        Nothing -> CE.throwIO (ReplayDivergence (unrecordedToolMsg name))
        Just e
            | rtcName e /= name ->
                CE.throwIO (ReplayDivergence (toolOrderMsg (rtcName e) name))
            | args /= rtcArguments e ->
                CE.throwIO (ReplayDivergence (argsDriftMsg name))
            | otherwise ->
                pure (fromMaybe (Left (noResultMsg (rtcCallId e))) (rtcResult e))

unrecordedTurnMsg :: Int -> Text
unrecordedTurnMsg n =
    "strict replay: model turn #" <> T.pack (show n) <> " was not in the recording"

unrecordedToolMsg :: Text -> Text
unrecordedToolMsg name =
    "strict replay: tool '" <> name <> "' was not in the recording"

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
