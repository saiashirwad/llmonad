{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Pure in-memory Journal interpreter for ephemeral sessions and deterministic testing.
module LLMonad.Journal.Memory (
    -- * Pure In-Memory Journal Interpreter
    runJournalMemory,
    runJournalMemoryWithState,
    runJournalMemorySimple,
    memoryJournalHandler,
) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import LLMonad.Journal (Journal (..))
import LLMonad.Journal.Types (JournalEvent, JournalState (..), initJournalState)

-- | Run the 'Journal' effect purely in memory, returning the result and all captured events.
runJournalMemory ::
    Eff (Journal : es) a ->
    Eff es (a, [JournalEvent])
runJournalMemory action = do
    (res, st) <- runJournalMemoryWithState initJournalState action
    pure (res, jsEvents st)

-- | Run the 'Journal' effect purely in memory starting from an initial 'JournalState'.
runJournalMemoryWithState ::
    JournalState ->
    Eff (Journal : es) a ->
    Eff es (a, JournalState)
runJournalMemoryWithState st = reinterpret_ (runState st) memoryJournalHandler

-- | Simplified in-memory interpreter that discards accumulated journal events on completion.
runJournalMemorySimple ::
    Eff (Journal : es) a ->
    Eff es a
runJournalMemorySimple action = do
    (res, _) <- runJournalMemory action
    pure res

-- | Dynamic handler for the 'Journal' effect implemented via local static State.
memoryJournalHandler :: EffectHandler_ Journal (State JournalState : es)
memoryJournalHandler = \case
    RecordEvent ev ->
        modify (\s -> s{jsEvents = jsEvents s ++ [ev]})
    GetEvents ->
        gets jsEvents
    ClearEvents ->
        modify (\s -> s{jsEvents = []})
