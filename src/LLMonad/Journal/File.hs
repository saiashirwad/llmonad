{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | File-based Journal interpreter persisting event records to JSONL files with immediate flushing.
module LLMonad.Journal.File
  ( -- * File-based Journal Interpreters
    runJournalFile
  , runJournalFileWithEvents
  , runJournalFileWorld
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef, newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import LLMonad.Journal (Journal (..))
import LLMonad.Journal.Replay (loadJournalText, resumeSession)
import LLMonad.Journal.Types (JournalEvent)
import LLMonad.World (World, doesFileExist, readFileText, writeFileText)
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.IO as IO

-- | Persist session journal events to a JSONL file on disk using standard IO with immediate line flushing.
runJournalFile ::
  (IOE :> es) =>
  FilePath ->
  Eff (Journal : es) a ->
  Eff es a
runJournalFile fp action = do
  liftIO $ Directory.createDirectoryIfMissing True (FilePath.takeDirectory fp)
  exists <- liftIO $ Directory.doesFileExist fp
  initialEvents <- if exists
    then liftIO $ do
      content <- TIO.readFile fp
      case loadJournalText content of
        Left _ -> pure []
        Right evs -> pure evs
    else pure []
  eventsRef <- liftIO $ newIORef initialEvents
  interpret (fileJournalHandler fp eventsRef) action

-- | Run file journal interpreter and return both the computation result and the final list of events.
runJournalFileWithEvents ::
  (IOE :> es) =>
  FilePath ->
  Eff (Journal : es) a ->
  Eff es (a, [JournalEvent])
runJournalFileWithEvents fp action = do
  res <- runJournalFile fp action
  evs <- resumeSession fp
  pure (res, evs)

fileJournalHandler ::
  (IOE :> es) =>
  FilePath ->
  IORef [JournalEvent] ->
  EffectHandler Journal es
fileJournalHandler fp eventsRef _ = \case
  RecordEvent ev -> liftIO $ do
    let lineBytes = LBS.toStrict (Aeson.encode ev)
    IO.withFile fp IO.AppendMode $ \h -> do
      IO.hSetBuffering h IO.LineBuffering
      BS.hPut h (lineBytes <> "\n")
      IO.hFlush h
    atomicModifyIORef' eventsRef (\evs -> (evs ++ [ev], ()))

  GetEvents -> liftIO $ do
    readIORef eventsRef

  ClearEvents -> liftIO $ do
    atomicWriteIORef eventsRef []
    IO.writeFile fp ""

-- | Persist session journal events via the 'World' effect (compatible with MemoryWorld and WorktreeWorld).
runJournalFileWorld ::
  forall es a.
  (World :> es) =>
  FilePath ->
  Eff (Journal : es) a ->
  Eff es a
runJournalFileWorld fp action = do
  exists <- doesFileExist fp
  initialText <- if exists then readFileText fp else pure ""
  let initialEvents = case loadJournalText initialText of
        Left _ -> []
        Right evs -> evs
  fmap fst $ reinterpret (runState (initialEvents, initialText)) worldJournalHandler action
  where
    worldJournalHandler :: EffectHandler Journal (State ([JournalEvent], Text) : es)
    worldJournalHandler _ = \case
      RecordEvent ev -> do
        (evs, rawText) <- get
        let lineText = TE.decodeUtf8 (LBS.toStrict (Aeson.encode ev))
        let newText = if T.null rawText then lineText <> "\n" else rawText <> lineText <> "\n"
        let newEvs = evs ++ [ev]
        put (newEvs, newText)
        writeFileText fp newText

      GetEvents -> do
        gets (fst :: ([JournalEvent], Text) -> [JournalEvent])

      ClearEvents -> do
        put ([] :: [JournalEvent], "" :: Text)
        writeFileText fp ""
