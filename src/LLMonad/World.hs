{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Core dynamic effect for filesystem, directory traversal, search, and process execution.
module LLMonad.World
  ( -- * The World Effect
    World (..)

    -- * Core Smart Constructors
  , readFileText
  , readFileSlice
  , writeFileText
  , deleteFile
  , createDirectory
  , listDirectory
  , searchFiles
  , findFiles
  , doesPathExist
  , doesFileExist
  , doesDirectoryExist
  , getWorkspaceRoot
  , runCommand
  , execShell

    -- * Simplified World Aliases
  , readFileWorld
  , writeFileWorld
  , deleteFileWorld
  , listDirWorld
  , findFilesWorld
  , grepFilesWorld
  , runCommandWorld
  , getCurrentDirWorld

    -- * Re-exported Types
  , module LLMonad.World.Types
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import Effectful.Dispatch.Dynamic
import LLMonad.World.Types

-- | Dynamic effect capturing filesystem, search, and process operations.
data World :: Effect where
  -- | Read entire UTF-8 text file.
  ReadFileText       :: FilePath -> World m Text
  -- | Read a 1-indexed line slice [startLine, endLine] of a file.
  ReadFileSlice      :: FilePath -> Maybe Int -> Maybe Int -> World m Text
  -- | Write or overwrite UTF-8 text file, creating parent directories.
  WriteFileText      :: FilePath -> Text -> World m ()
  -- | Delete an existing file.
  DeleteFile         :: FilePath -> World m ()
  -- | Create directory path (optionally creating parents).
  CreateDirectory    :: FilePath -> Bool -> World m ()
  -- | List direct children of a directory with metadata.
  ListDirectory      :: FilePath -> World m [DirEntry]
  -- | Search file contents with pattern matching.
  SearchFiles        :: SearchOptions -> World m [SearchMatch]
  -- | Find file paths by name pattern or depth.
  FindFiles          :: FindOptions -> World m [FilePath]
  -- | Check if any path (file or directory) exists.
  DoesPathExist      :: FilePath -> World m Bool
  -- | Check if path is a file.
  DoesFileExist      :: FilePath -> World m Bool
  -- | Check if path is a directory.
  DoesDirectoryExist :: FilePath -> World m Bool
  -- | Retrieve canonical workspace root directory.
  GetWorkspaceRoot   :: World m FilePath
  -- | Execute command process with arguments and options.
  RunCommand         :: CommandSpec -> World m ProcessResult

type instance DispatchOf World = Dynamic

-- | Read entire file content as Text.
readFileText :: (World :> es) => FilePath -> Eff es Text
readFileText fp = send (ReadFileText fp)

-- | Read 1-indexed line slice [startLine, endLine] of a file.
readFileSlice :: (World :> es) => FilePath -> Maybe Int -> Maybe Int -> Eff es Text
readFileSlice fp start end = send (ReadFileSlice fp start end)

-- | Write UTF-8 text to a file.
writeFileText :: (World :> es) => FilePath -> Text -> Eff es ()
writeFileText fp content = send (WriteFileText fp content)

-- | Delete a file from the workspace.
deleteFile :: (World :> es) => FilePath -> Eff es ()
deleteFile fp = send (DeleteFile fp)

-- | Create a directory.
createDirectory :: (World :> es) => FilePath -> Bool -> Eff es ()
createDirectory fp createParents = send (CreateDirectory fp createParents)

-- | List entries in a directory.
listDirectory :: (World :> es) => FilePath -> Eff es [DirEntry]
listDirectory fp = send (ListDirectory fp)

-- | Search files matching text or regex pattern.
searchFiles :: (World :> es) => SearchOptions -> Eff es [SearchMatch]
searchFiles opts = send (SearchFiles opts)

-- | Discover files matching find options.
findFiles :: (World :> es) => FindOptions -> Eff es [FilePath]
findFiles opts = send (FindFiles opts)

-- | Check if a path exists.
doesPathExist :: (World :> es) => FilePath -> Eff es Bool
doesPathExist fp = send (DoesPathExist fp)

-- | Check if a path exists and is a file.
doesFileExist :: (World :> es) => FilePath -> Eff es Bool
doesFileExist fp = send (DoesFileExist fp)

-- | Check if a path exists and is a directory.
doesDirectoryExist :: (World :> es) => FilePath -> Eff es Bool
doesDirectoryExist fp = send (DoesDirectoryExist fp)

-- | Get the workspace root directory.
getWorkspaceRoot :: (World :> es) => Eff es FilePath
getWorkspaceRoot = send GetWorkspaceRoot

-- | Run an external command with specifications.
runCommand :: (World :> es) => CommandSpec -> Eff es ProcessResult
runCommand spec = send (RunCommand spec)

-- | Run a shell command string.
execShell :: (World :> es) => Text -> Eff es ProcessResult
execShell cmd = send (RunCommand (CommandSpec "/bin/sh" ["-c", cmd] Nothing Nothing Nothing Nothing))

-- | Alias: Read entire file content.
readFileWorld :: (World :> es) => FilePath -> Eff es Text
readFileWorld = readFileText

-- | Alias: Write entire file content.
writeFileWorld :: (World :> es) => FilePath -> Text -> Eff es ()
writeFileWorld = writeFileText

-- | Alias: Delete file.
deleteFileWorld :: (World :> es) => FilePath -> Eff es ()
deleteFileWorld = deleteFile

-- | Alias: List directory filenames.
listDirWorld :: (World :> es) => FilePath -> Eff es [FilePath]
listDirWorld fp = do
  entries <- listDirectory fp
  pure (map deName entries)

-- | Alias: Find files matching pattern.
findFilesWorld :: (World :> es) => FilePath -> Text -> Eff es [FilePath]
findFilesWorld dir pat =
  findFiles (FindOptions dir (if T.null pat then Nothing else Just pat) Nothing FindAny [])

-- | Alias: Grep files matching pattern.
grepFilesWorld :: (World :> es) => FilePath -> Text -> Eff es [(FilePath, Int, Text)]
grepFilesWorld dir query = do
  matches <- searchFiles (SearchOptions query dir False False Nothing [] [])
  pure [(smFile m, smLineNumber m, smLineContent m) | m <- matches]

-- | Alias: Run command with program, args, cwd, and timeout.
runCommandWorld :: (World :> es) => Text -> [Text] -> Maybe FilePath -> Maybe Int -> Eff es ProcessResult
runCommandWorld prog args mcwd mtimeout =
  runCommand (CommandSpec prog args mcwd Nothing mtimeout Nothing)

-- | Alias: Get current working/workspace directory.
getCurrentDirWorld :: (World :> es) => Eff es FilePath
getCurrentDirWorld = getWorkspaceRoot
