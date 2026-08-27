{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Session resume, audit verification, and replay analytics for the Journal effect.
module LLMonad.Journal.Replay (
    -- * Audit Verification & Replay Analytics
    replayAudit,
    replayAuditSummary,

    -- * Session Deserialization & Resume
    loadJournalText,
    loadJournalFile,
    loadJournalFileWorld,
    resumeSession,
    resumeSessionWorld,
    reconstructChatHistory,
) where

import Control.Exception qualified as Exception
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.List (foldl')
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Effectful
import LLMonad.Journal.Types
import LLMonad.Types (ChatMessage)
import LLMonad.Types qualified as CoreTypes
import LLMonad.World (World, doesFileExist, readFileText)
import System.Directory qualified as Directory
import System.IO.Error qualified as IO

-- | Validation state accumulator used during audit replay traversal.
data AuditState = AuditState
    { asTurnsStarted :: !Int
    , asUserMessages :: !Int
    , asModelTurns :: !Int
    , asToolInvocations :: !Int
    , asToolCompletions :: !Int
    , asPromptTokens :: !Int
    , asCompletionTokens :: !Int
    , asTotalTokens :: !Int
    , asTotalLatencyMs :: !Double
    , asOpenTurns :: ![Text]
    , asPendingToolCalls :: ![Text]
    , asValidationErrors :: ![Text]
    }

initAuditState :: AuditState
initAuditState =
    AuditState
        { asTurnsStarted = 0
        , asUserMessages = 0
        , asModelTurns = 0
        , asToolInvocations = 0
        , asToolCompletions = 0
        , asPromptTokens = 0
        , asCompletionTokens = 0
        , asTotalTokens = 0
        , asTotalLatencyMs = 0.0
        , asOpenTurns = []
        , asPendingToolCalls = []
        , asValidationErrors = []
        }

-- | Run audit verification across an event list and produce a comprehensive 'ReplaySummary'.
replayAuditSummary :: [JournalEvent] -> ReplaySummary
replayAuditSummary events =
    let finalState = foldl' processEvent initAuditState events
        postCheckedState = finalizeAudit finalState
        isValid = null (asValidationErrors postCheckedState)
     in ReplaySummary
            { rsTotalTurns = asTurnsStarted postCheckedState
            , rsUserMessages = asUserMessages postCheckedState
            , rsModelTurns = asModelTurns postCheckedState
            , rsToolInvocations = asToolInvocations postCheckedState
            , rsToolCompletions = asToolCompletions postCheckedState
            , rsPromptTokens = asPromptTokens postCheckedState
            , rsCompletionTokens = asCompletionTokens postCheckedState
            , rsTotalTokens = asTotalTokens postCheckedState
            , rsTotalLatencyMs = asTotalLatencyMs postCheckedState
            , rsIsValidSequence = isValid
            , rsValidationErrors = asValidationErrors postCheckedState
            }
  where
    processEvent :: AuditState -> JournalEvent -> AuditState
    processEvent st = \case
        TurnStarted tid ->
            let newErrors = case asOpenTurns st of
                    (activeTid : _) -> asValidationErrors st ++ ["Nested TurnStarted without closing previous turn: " <> tid <> " (active: " <> activeTid <> ")"]
                    [] -> asValidationErrors st
             in st
                    { asTurnsStarted = asTurnsStarted st + 1
                    , asOpenTurns = tid : asOpenTurns st
                    , asValidationErrors = newErrors
                    }
        TurnFinished tid ->
            case asOpenTurns st of
                [] ->
                    st{asValidationErrors = asValidationErrors st ++ ["TurnFinished with no open turn: " <> tid]}
                activeTid : rest
                    | activeTid == tid -> st{asOpenTurns = rest}
                    | otherwise ->
                        st
                            { asOpenTurns = rest
                            , asValidationErrors = asValidationErrors st ++ ["TurnFinished " <> tid <> " does not match active turn " <> activeTid]
                            }
        UserMsg _ ->
            st{asUserMessages = asUserMessages st + 1}
        ModelTurn _ _ ->
            st{asModelTurns = asModelTurns st + 1}
        ToolInvoked toolCallId _ _ ->
            st
                { asToolInvocations = asToolInvocations st + 1
                , asPendingToolCalls = asPendingToolCalls st ++ [toolCallId]
                }
        ToolCompleted toolCallId _ ->
            let pending = asPendingToolCalls st
             in case removeFirst toolCallId pending of
                    Just remaining ->
                        st
                            { asToolCompletions = asToolCompletions st + 1
                            , asPendingToolCalls = remaining
                            }
                    Nothing ->
                        st
                            { asToolCompletions = asToolCompletions st + 1
                            , asValidationErrors = asValidationErrors st ++ ["ToolCompleted without matching ToolInvoked: " <> toolCallId]
                            }
        MetricsReported mm ->
            st
                { asPromptTokens = asPromptTokens st + mmPromptTokens mm
                , asCompletionTokens = asCompletionTokens st + mmCompletionTokens mm
                , asTotalTokens = asTotalTokens st + mmTotalTokens mm
                , asTotalLatencyMs = asTotalLatencyMs st + mmLatencyMs mm
                }

    finalizeAudit :: AuditState -> AuditState
    finalizeAudit st =
        let turnErrors = ["Turn was started but never finished: " <> tid | tid <- asOpenTurns st]
            toolErrors = ["Tool invocation was never completed: " <> toolCallId | toolCallId <- asPendingToolCalls st]
         in st{asValidationErrors = asValidationErrors st ++ turnErrors ++ toolErrors}

    removeFirst :: (Eq a) => a -> [a] -> Maybe [a]
    removeFirst _ [] = Nothing
    removeFirst x (y : ys)
        | x == y = Just ys
        | otherwise = (y :) <$> removeFirst x ys

-- | Verify event stream sequence and correspondence. Returns 'Right summary' if valid or 'Left error'.
replayAudit :: [JournalEvent] -> Either Text ReplaySummary
replayAudit events =
    let summary = replayAuditSummary events
     in if rsIsValidSequence summary
            then Right summary
            else Left (T.intercalate "; " (rsValidationErrors summary))

-- | Parse newline-delimited JSON (JSONL) text into typed 'JournalEvent' records.
loadJournalText :: Text -> Either Text [JournalEvent]
loadJournalText txt =
    let rawLines = filter (not . T.null . T.strip) (T.lines txt)
        parseLine (idx, line) =
            case Aeson.eitherDecodeStrict' (TE.encodeUtf8 line) of
                Left err -> Left ("Line " <> T.pack (show idx) <> ": " <> T.pack err)
                Right ev -> Right ev
     in mapM parseLine (zip [1 :: Int ..] rawLines)

{- | Outcome of reading a journal file. Absent, corrupt, and loaded are
distinguished in the type so resume logic never has to match on error
text -- both spellings of \"missing\" and every wording change stay safe.
-}
data JournalLoad
    = JournalLoaded [JournalEvent]
    | JournalMissing
    | JournalCorrupt Text

journalFromDisk :: FilePath -> IO JournalLoad
journalFromDisk fp = do
    exists <- Directory.doesFileExist fp
    if not exists
        then pure JournalMissing
        else either JournalCorrupt JournalLoaded . loadJournalText <$> TIO.readFile fp

journalFromWorldEff :: (World :> es) => FilePath -> Eff es JournalLoad
journalFromWorldEff fp = do
    exists <- doesFileExist fp
    if not exists
        then pure JournalMissing
        else either JournalCorrupt JournalLoaded . loadJournalText <$> readFileText fp

-- | Load and deserialize a JSONL journal file from disk using standard IO.
loadJournalFile :: (IOE :> es) => FilePath -> Eff es (Either Text [JournalEvent])
loadJournalFile fp = liftIO $ do
    res <- journalFromDisk fp
    pure $ case res of
        JournalLoaded evs -> Right evs
        JournalMissing -> Left ("Journal file does not exist: " <> T.pack fp)
        JournalCorrupt err -> Left err

-- | Load and deserialize a JSONL journal file from the 'World' effect.
loadJournalFileWorld :: (World :> es) => FilePath -> Eff es (Either Text [JournalEvent])
loadJournalFileWorld fp = do
    res <- journalFromWorldEff fp
    pure $ case res of
        JournalLoaded evs -> Right evs
        JournalMissing -> Left ("Journal file does not exist: " <> T.pack fp)
        JournalCorrupt err -> Left err

{- | Restore past session journal events from a JSONL file via IO.
Fails closed (throws an IO exception) if the journal file is corrupted.
Returns empty list if the journal file does not exist yet.
-}
resumeSession :: (IOE :> es) => FilePath -> Eff es [JournalEvent]
resumeSession fp = do
    res <- liftIO (journalFromDisk fp)
    case res of
        JournalLoaded evs -> pure evs
        JournalMissing -> pure []
        JournalCorrupt err ->
            liftIO $ IO.ioError $ IO.userError ("Failed to resume corrupted session from " ++ fp ++ ": " ++ T.unpack err)

{- | Restore past session journal events using the 'World' effect.
Fails closed (throws an IO exception) if the journal file is corrupted.
Returns empty list if the journal file does not exist yet.
-}
resumeSessionWorld :: (World :> es) => FilePath -> Eff es [JournalEvent]
resumeSessionWorld fp = do
    res <- journalFromWorldEff fp
    case res of
        JournalLoaded evs -> pure evs
        JournalMissing -> pure []
        JournalCorrupt err ->
            Exception.throw (IO.userError ("Failed to resume corrupted session from " ++ fp ++ ": " ++ T.unpack err))

-- | Reconstruct a conversational 'ChatMessage' history from a stream of 'JournalEvent's.
reconstructChatHistory :: [JournalEvent] -> [ChatMessage]
reconstructChatHistory = foldl' step []
  where
    step :: [ChatMessage] -> JournalEvent -> [ChatMessage]
    step acc = \case
        UserMsg content ->
            acc ++ [CoreTypes.UserMsg content]
        ModelTurn content calls ->
            acc ++ [CoreTypes.AssistantMsg content calls]
        ToolCompleted toolCallId result ->
            let renderedResult = case result of
                    Left err -> "Error: " <> err
                    Right val -> TE.decodeUtf8 (LBS.toStrict (Aeson.encode val))
             in acc ++ [CoreTypes.ToolMsg toolCallId renderedResult]
        _ -> acc
