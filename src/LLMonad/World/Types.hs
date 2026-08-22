{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Common data types, options, and error representations for the World effect.
module LLMonad.World.Types
  ( -- * Directory & File Entries
    DirEntry (..)
  , FileEntry
  , pattern FileEntry
  , feName
  , fePath
  , feIsDir
  , feSize
  , feModified

    -- * Search & Pattern Matching
  , SearchOptions (..)
  , SearchMatch (..)
  , FindOptions (..)
  , FindTypeFilter (..)

    -- * Command Execution
  , CommandSpec (..)
  , ProcessResult (..)
  , exitCode
  , stdoutText
  , stderrText
  , durationMs
  , timedOut

    -- * Error Representation
  , WorldError (..)
  , prettyWorldError

    -- * Git Worktree Sandboxing
  , WorktreeConfig (..)
  , defaultWorktreeConfig
  , WorktreeSummary (..)

    -- * In-Memory Virtual State
  , MemoryWorldState (..)
  , initMemoryWorld
  ) where

import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import System.FilePath (normalise)

-- | Detailed directory or file entry metadata.
data DirEntry = DirEntry
  { deName     :: !FilePath
  , dePath     :: !FilePath
  , deIsDir    :: !Bool
  , deSize     :: !Integer
  , deModified :: !(Maybe UTCTime)
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Type alias for file entries.
type FileEntry = DirEntry

-- | Bidirectional pattern synonym for FileEntry.
pattern FileEntry :: FilePath -> FilePath -> Bool -> Integer -> Maybe UTCTime -> FileEntry
pattern FileEntry n p d s m = DirEntry n p d s m

feName :: DirEntry -> FilePath
feName = deName

fePath :: DirEntry -> FilePath
fePath = dePath

feIsDir :: DirEntry -> Bool
feIsDir = deIsDir

feSize :: DirEntry -> Integer
feSize = deSize

feModified :: DirEntry -> Maybe UTCTime
feModified = deModified

-- | Options for searching file contents by plain text or regex.
data SearchOptions = SearchOptions
  { soQuery           :: !Text
  , soSearchDir       :: !FilePath
  , soIsRegex         :: !Bool
  , soCaseInsensitive :: !Bool
  , soMaxMatches      :: !(Maybe Int)
  , soIncludes        :: ![Text]
  , soExcludes        :: ![Text]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | A single search hit within a file.
data SearchMatch = SearchMatch
  { smFile        :: !FilePath
  , smLineNumber  :: !Int
  , smLineContent :: !Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Options for discovering files and directories by path/glob.
data FindOptions = FindOptions
  { foSearchDir  :: !FilePath
  , foPattern    :: !(Maybe Text)
  , foMaxDepth   :: !(Maybe Int)
  , foTypeFilter :: !FindTypeFilter
  , foExcludes   :: ![Text]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Type filter for file discovery.
data FindTypeFilter
  = FindAny
  | FindFilesOnly
  | FindDirsOnly
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Process execution specification.
data CommandSpec = CommandSpec
  { cmdProgram   :: !Text
  , cmdArgs      :: ![Text]
  , cmdCwd       :: !(Maybe FilePath)
  , cmdEnv       :: !(Maybe [(Text, Text)])
  , cmdTimeoutMs :: !(Maybe Int)
  , cmdStdin     :: !(Maybe Text)
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Result of process execution.
data ProcessResult = ProcessResult
  { prExitCode   :: !Int
  , prStdout     :: !Text
  , prStderr     :: !Text
  , prDurationMs :: !Double
  , prTimedOut   :: !Bool
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

exitCode :: ProcessResult -> Int
exitCode = prExitCode

stdoutText :: ProcessResult -> Text
stdoutText = prStdout

stderrText :: ProcessResult -> Text
stderrText = prStderr

durationMs :: ProcessResult -> Double
durationMs = prDurationMs

timedOut :: ProcessResult -> Bool
timedOut = prTimedOut

-- | Structured error type for World operations.
data WorldError
  = WorldFileNotFound !FilePath
  | WorldDirectoryNotFound !FilePath
  | WorldPermissionDenied !FilePath !Text
  | WorldPathOutsideWorkspace !FilePath !FilePath
  | WorldFileAlreadyExists !FilePath
  | WorldIsADirectory !FilePath
  | WorldNotADirectory !FilePath
  | WorldCommandTimeout !Int !Text
  | WorldCommandFailed !Text !Int !Text
  | WorldGitError !Text
  | WorldIOError !Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON, Exception)

-- | Format WorldError into a human-readable text message.
prettyWorldError :: WorldError -> Text
prettyWorldError = \case
  WorldFileNotFound fp -> "File not found: " <> T.pack fp
  WorldDirectoryNotFound fp -> "Directory not found: " <> T.pack fp
  WorldPermissionDenied fp reason -> "Permission denied for " <> T.pack fp <> ": " <> reason
  WorldPathOutsideWorkspace fp root -> "Path escapes workspace root: " <> T.pack fp <> " (root: " <> T.pack root <> ")"
  WorldFileAlreadyExists fp -> "File already exists: " <> T.pack fp
  WorldIsADirectory fp -> "Path is a directory: " <> T.pack fp
  WorldNotADirectory fp -> "Path is not a directory: " <> T.pack fp
  WorldCommandTimeout ms prog -> "Command timed out after " <> T.pack (show ms) <> "ms: " <> prog
  WorldCommandFailed prog code err -> "Command '" <> prog <> "' exited with code " <> T.pack (show code) <> ": " <> err
  WorldGitError msg -> "Git error: " <> msg
  WorldIOError msg -> "IO error: " <> msg

-- | Configuration for ephemeral Git worktree sandboxing.
data WorktreeConfig = WorktreeConfig
  { wtBaseRepo     :: !FilePath
  , wtBaseRef      :: !Text
  , wtBranchPrefix :: !Text
  , wtAutoClean    :: !Bool
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Default configuration for worktrees.
defaultWorktreeConfig :: FilePath -> WorktreeConfig
defaultWorktreeConfig repo = WorktreeConfig
  { wtBaseRepo     = repo
  , wtBaseRef      = "HEAD"
  , wtBranchPrefix = "llmonad-sandbox"
  , wtAutoClean    = True
  }

-- | Summary of changes and execution inside a Git worktree.
data WorktreeSummary = WorktreeSummary
  { wsPath         :: !FilePath
  , wsBranch       :: !Text
  , wsDiff         :: !Text
  , wsFilesChanged :: ![FilePath]
  , wsExitCode     :: !Int
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | State tracking for pure in-memory simulated World interpreter.
data MemoryWorldState = MemoryWorldState
  { mwsFiles           :: !(Map FilePath Text)
  , mwsWorkingDir      :: !FilePath
  , mwsCommandHandlers :: !(Map Text (CommandSpec -> ProcessResult))
  , mwsCommandHistory  :: ![CommandSpec]
  } deriving (Generic)

-- | Initialize in-memory world state with a set of file contents.
initMemoryWorld :: [(FilePath, Text)] -> MemoryWorldState
initMemoryWorld initialFiles = MemoryWorldState
  { mwsFiles           = Map.fromList [(dropWhile (== '/') (normalise p), c) | (p, c) <- initialFiles]
  , mwsWorkingDir      = "/"
  , mwsCommandHandlers = Map.empty
  , mwsCommandHistory  = []
  }
