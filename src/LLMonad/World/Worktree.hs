{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Ephemeral Git worktree sandboxing interpreter for isolated execution.
module LLMonad.World.Worktree (
    -- * Worktree Sandboxing Interpreter
    runWorldWorktree,
    runWorldWorktreeSimple,
    setupWorktree,
    cleanupWorktree,
    gatherWorktreeSummary,
) where

import Control.Exception qualified as CE
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Effectful
import Effectful.Exception qualified as EE
import LLMonad.World (World)
import LLMonad.World.Local (runWorldLocal)
import LLMonad.World.Types
import System.Directory (getTemporaryDirectory, removeDirectory, removeDirectoryRecursive)
import System.Directory qualified as SD
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName)
import System.IO.Temp (createTempDirectory)
import System.Process (readProcessWithExitCode)

{- | Run World computations inside an isolated ephemeral Git worktree.
Creates worktree before execution and guarantees complete cleanup upon return or failure.
-}
runWorldWorktree ::
    (IOE :> es) =>
    WorktreeConfig ->
    Eff (World : es) a ->
    Eff es (a, WorktreeSummary)
runWorldWorktree config action = do
    (tempDir, branchName) <- liftIO $ setupWorktree config
    res <-
        ( do
            val <- runWorldLocal tempDir action
            summary <- liftIO $ gatherWorktreeSummary tempDir branchName config
            pure (val, summary)
        )
            `EE.catch` ( \(e :: EE.SomeException) -> do
                            liftIO $ cleanupWorktree config tempDir branchName
                            EE.throwIO e
                       )
    liftIO $ cleanupWorktree config tempDir branchName
    pure res

-- | Simplified worktree execution returning Either Text a.
runWorldWorktreeSimple ::
    (IOE :> es) =>
    FilePath ->
    Text ->
    Eff (World : es) a ->
    Eff es (Either Text a)
runWorldWorktreeSimple repo baseRef action = do
    let config = (defaultWorktreeConfig repo){wtBaseRef = baseRef}
    ( do
            (val, _) <- runWorldWorktree config action
            pure (Right val)
        )
        `EE.catch` (\(e :: EE.SomeException) -> pure (Left (T.pack (show e))))

-- | Provision a new temporary Git worktree and branch.
setupWorktree :: WorktreeConfig -> IO (FilePath, Text)
setupWorktree config = do
    let repo = wtBaseRepo config
    let baseRef = T.unpack (wtBaseRef config)
    sysTmp <- getTemporaryDirectory
    tempDir <- createTempDirectory sysTmp "llmonad-wt-"
    -- Remove temporary empty directory so git worktree add can initialize it
    removeDirectory tempDir
    now <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%Y%m%d%H%M%S%q" now
    let uniqueTag = T.pack (takeFileName tempDir)
    let branch = wtBranchPrefix config <> "-" <> T.pack timeStr <> "-" <> uniqueTag
    (code, _, err) <- readProcessWithExitCode "git" ["-C", repo, "worktree", "add", "-b", T.unpack branch, tempDir, baseRef] ""
    case code of
        ExitSuccess -> pure (tempDir, branch)
        ExitFailure n -> CE.throwIO (WorldGitError ("git worktree add failed (exit " <> T.pack (show n) <> "): " <> T.pack err))

-- | Clean up the worktree and delete its ephemeral branch.
cleanupWorktree :: WorktreeConfig -> FilePath -> Text -> IO ()
cleanupWorktree config tempDir branch = do
    let repo = wtBaseRepo config
    _ <- readProcessWithExitCode "git" ["-C", repo, "worktree", "remove", "--force", tempDir] ""
    _ <- readProcessWithExitCode "git" ["-C", repo, "worktree", "prune"] ""
    exists <- SD.doesDirectoryExist tempDir
    when exists $ CE.catch (removeDirectoryRecursive tempDir) (\(_ :: CE.SomeException) -> pure ())
    when (wtAutoClean config) $ do
        _ <- readProcessWithExitCode "git" ["-C", repo, "branch", "-D", T.unpack branch] ""
        pure ()

-- | Collect diff and status summaries before worktree removal.
gatherWorktreeSummary :: FilePath -> Text -> WorktreeConfig -> IO WorktreeSummary
gatherWorktreeSummary tempDir branch config = do
    let baseRef = T.unpack (wtBaseRef config)
    (diffCode, diffOut, _) <- readProcessWithExitCode "git" ["-C", tempDir, "diff", baseRef] ""
    (statCode, statOut, _) <- readProcessWithExitCode "git" ["-C", tempDir, "status", "--porcelain"] ""
    let changedFiles = [drop 3 l | l <- lines statOut, not (null l)]
    let exitVal = if diffCode == ExitSuccess && statCode == ExitSuccess then 0 else 1
    pure
        WorktreeSummary
            { wsPath = tempDir
            , wsBranch = branch
            , wsDiff = T.pack diffOut
            , wsFilesChanged = changedFiles
            , wsExitCode = exitVal
            }
