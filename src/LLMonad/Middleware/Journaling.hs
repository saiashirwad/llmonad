{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- | Record agent traffic into the 'Journal' effect as it happens.

'journaling' is model middleware: attach it like any other middleware and
every round the attached agents run -- user prompts, requested tool calls,
tool results, and model turns -- lands in the installed journal interpreter
(@runJournalMemory@, @runJournalFile@, @runWorldMemory@/@runJournalFileWorld@),
producing events 'replayAudit' accepts and 'extractReplayScript' turns into
a regression test.

Turn bracketing is deliberately not this middleware's job: pair it with
'withRecordedTurn' at the program edge so a recorded session is audit-valid
even if the run aborts mid-turn.
-}
module LLMonad.Middleware.Journaling (
    journaling,
    withRecordedTurn,
) where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Exception qualified as E
import LLMonad.Core
import LLMonad.Journal
import LLMonad.Journal.Types qualified as JT
import LLMonad.Middleware (Middleware (..))
import LLMonad.Model (ModelRuntime (..))
import LLMonad.Types
import LLMonad.Types qualified as CoreTypes

{- | Observe every round of the wrapped runtime: requested tool calls and
model turns become 'ToolInvoked' / 'ModelTurn' events at round completion;
pushed prompts and tool results become 'UserMsg' / 'ToolCompleted'. All
operations forward to the runtime underneath unchanged.
-}
journaling :: forall es. (Journal :> es) => Middleware es
journaling = Middleware $ \(ModelRuntime run) ->
    ModelRuntime $ \action -> run (interpose_ journalHandler action)

journalHandler :: forall es. (Journal :> es, LLM :> es) => EffectHandler_ LLM es
journalHandler = \case
    GetHistory -> send GetHistory
    SetHistory msgs -> send (SetHistory msgs)
    PushMessage msg -> do
        observePush msg
        send (PushMessage msg)
    ClearHistory -> send ClearHistory
    GetSystem -> send GetSystem
    SetSystem sys -> send (SetSystem sys)
    ClearSystem -> send ClearSystem
    ChatRound p fmt specs choice -> step (send (ChatRound p fmt specs choice))
    StreamRound p fmt specs cb -> step (send (StreamRound p fmt specs cb))
  where
    step :: Eff es CompletionResponse -> Eff es CompletionResponse
    step inner = do
        resp <- inner
        recordResponse resp
        pure resp

    observePush :: ChatMessage -> Eff es ()
    observePush = \case
        CoreTypes.UserMsg t -> recordEvent (JT.UserMsg t)
        CoreTypes.ToolMsg cid content -> recordEvent (toCompleted cid content)
        CoreTypes.AssistantMsg{} ->
            -- Already captured when its round completed.
            pure ()
        CoreTypes.SystemMsg{} -> pure ()

{- | Requested tool calls first, then the assistant turn that asked for
them: tool completion events land afterwards, keeping call/result pairs
adjacent enough for 'replayAudit'.
-}
recordResponse :: (Journal :> es) => CompletionResponse -> Eff es ()
recordResponse resp = do
    mapM_
        ( \tc ->
            recordEvent
                ( ToolInvoked
                    (toolCallId tc)
                    (toolCallName tc)
                    (toolCallArguments tc)
                )
        )
        (crspToolCalls resp)
    recordEvent (ModelTurn (crspText resp) (crspToolCalls resp))

{- | Decode pushed tool-result text back into a 'ToolResult'. The text is
JSON carrying either a bare payload or an object whose only key is
@error@; undecodable text degrades to a textual error result.
-}
toCompleted :: Text -> Text -> JournalEvent
toCompleted cid content = ToolCompleted cid $ case TE.encodeUtf8 content of
    bytes -> case Aeson.eitherDecodeStrict' bytes of
        Right val@(Aeson.Object o)
            | Just (Aeson.String errText) <- KM.lookup "error" o
            , length (KM.toList o) == 1 ->
                Left errText
        Right val -> Right val
        Left _ -> Left content

{- | Run a program inside one audit-valid turn bracket, so the recorded
session passes 'replayAudit' even when the run aborts mid-way: any
exception still closes the turn.
-}
withRecordedTurn :: (Journal :> es) => Text -> Eff es a -> Eff es a
withRecordedTurn turnId action =
    recordEvent (TurnStarted turnId)
        >> action `E.finally` recordEvent (TurnFinished turnId)
