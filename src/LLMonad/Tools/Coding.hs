{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Standard coding agent tools for reading, editing, searching, and running commands.
-- Built directly on the 'World' effect.
module LLMonad.Tools.Coding
  ( -- * Standard Coding Tools
    viewFileTool
  , editFileTool
  , grepSearchTool
  , findByNameTool
  , listDirTool
  , runCommandTool
  , readOnlyCodingToolset
  , standardCodingToolset
  , standardCodingTools

    -- * Pure & Effectful Runner Functions
  , runViewFile
  , runEditFile
  , runGrepSearch
  , runFindByName
  , runListDir
  , runRunCommand

    -- * Tool Argument Types
  , ViewFileArgs (..)
  , EditFileArgs (..)
  , GrepSearchArgs (..)
  , FindByNameArgs (..)
  , ListDirArgs (..)
  , RunCommandArgs (..)

    -- * Tool Result Types
  , ViewFileLine (..)
  , ViewFileResult (..)
  , EditFileResult (..)
  , GrepMatch (..)
  , GrepSearchResult (..)
  , FileEntryInfo (..)
  , FindByNameResult (..)
  , DirEntryInfo (..)
  , ListDirResult (..)
  , RunCommandResult (..)

    -- * Diff Utilities
  , computeDiffSnippet
  , computeModifiedLines

    -- * JSON Helpers
  , oneOf
  , orElse
  ) where

import Control.Applicative (asum)
import Control.Monad (forM)
import Data.Aeson
  ( FromJSON (..)
  , Key
  , Object
  , ToJSON (..)
  , toJSON
  , withObject
  , (.:)
  , (.:?)
  )
import Data.Aeson.Types (Parser)
import Data.Algorithm.Diff (PolyDiff (..), getDiff)
import Data.List (nub, sort)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import qualified Effectful.Exception as E
import GHC.Generics (Generic)
import LLMonad.Schema (ToSchema (..))
import LLMonad.Tools (Tool, ToolResult, Toolset, tool', tools)
import System.FilePath (makeRelative, (</>))
import LLMonad.World
  ( CommandSpec (..)
  , DirEntry (..)
  , FindOptions (..)
  , FindTypeFilter (..)
  , ProcessResult (..)
  , SearchMatch (..)
  , SearchOptions (..)
  , doesDirectoryExist
  , doesFileExist
  , findFiles
  , listDirectory
  , readFileText
  , runCommand
  , searchFiles
  , writeFileText
  , World
  , WorldError
  , prettyWorldError
  )

--------------------------------------------------------------------------------
-- 0. JSON Helpers
--------------------------------------------------------------------------------

-- | Try each key name in order; return the value of the first key that is present.
--   Missing keys are skipped. A present key of the wrong type is an error.
oneOf :: FromJSON a => Object -> [Key] -> Parser (Maybe a)
oneOf o keys = asum <$> traverse (o .:?) keys

-- | Use the optional result if it is present; otherwise run the fallback parser.
orElse :: Parser (Maybe a) -> Parser a -> Parser a
orElse p fallback = p >>= maybe fallback pure

-- | Build a coding tool that reports expected World failures to the model.
worldTool ::
  forall a es.
  (FromJSON a, ToSchema a) =>
  Text ->
  Text ->
  (a -> Eff es ToolResult) ->
  Tool (Eff es)
worldTool name description run =
  tool' name description $ \args ->
    E.catch (run args) $ \(err :: WorldError) ->
      pure (Left (prettyWorldError err))

--------------------------------------------------------------------------------
-- 1. viewFile Tool
--------------------------------------------------------------------------------

-- | Arguments for reading file contents with line slicing.
data ViewFileArgs = ViewFileArgs
  { filePath      :: !FilePath
  , startLine     :: !(Maybe Int)
  , endLine       :: !(Maybe Int)
  , contentOffset :: !(Maybe Int)
  } deriving (Show, Eq, Generic, ToJSON, ToSchema)

instance FromJSON ViewFileArgs where
  parseJSON = withObject "ViewFileArgs" $ \o ->
    ViewFileArgs
      <$> (oneOf o ["filePath", "file_path"] `orElse` (o .: "path"))
      <*> oneOf o ["startLine", "start_line"]
      <*> oneOf o ["endLine", "end_line"]
      <*> oneOf o ["contentOffset", "content_offset"]

-- | A single indexed line of file text.
data ViewFileLine = ViewFileLine
  { lineIndex :: !Int
  , lineText  :: !Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

-- | Result of reading and slicing a file.
data ViewFileResult = ViewFileResult
  { vfrPath          :: !FilePath
  , vfrTotalLines    :: !Int
  , vfrTotalBytes    :: !Int
  , vfrStartLine     :: !Int
  , vfrEndLine       :: !Int
  , vfrLines         :: ![ViewFileLine]
  , vfrIsTruncated   :: !Bool
  , vfrContentOffset :: !Int
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

-- | Execute viewFile operation against the 'World' effect.
runViewFile :: (World :> es) => ViewFileArgs -> Eff es (Either Text ViewFileResult)
runViewFile ViewFileArgs{..} = do
  let fp = filePath
  isDir <- doesDirectoryExist fp
  if isDir
    then pure (Left ("Path is a directory, not a file: " <> T.pack fp))
    else do
      fileExists <- doesFileExist fp
      if not fileExists
        then pure (Left ("File not found: " <> T.pack fp))
        else do
          content <- readFileText fp
          let totalBytes = T.length content
          let isBinary = T.any (== '\0') content
          if isBinary
            then pure $ Right ViewFileResult
                  { vfrPath          = fp
                  , vfrTotalLines    = 0
                  , vfrTotalBytes    = totalBytes
                  , vfrStartLine     = 0
                  , vfrEndLine       = 0
                  , vfrLines         = [ViewFileLine 0 "<binary content>"]
                  , vfrIsTruncated   = False
                  , vfrContentOffset = 0
                  }
            else do
              let allLines = if T.null content then [] else T.lines content
              let totalLines = length allLines
              let start = max 1 (fromMaybe 1 startLine)
              let maxAllowedEnd = start + 799
              let requestedEnd = fromMaybe (max 1 totalLines) endLine
              let end = max start (min maxAllowedEnd requestedEnd)
              let rawSlice = if start > totalLines
                    then []
                    else take (max 0 (end - start + 1)) (drop (start - 1) allLines)
              let indexedLines = [ViewFileLine idx line | (idx, line) <- zip [start..] rawSlice]
              let offset = max 0 (fromMaybe 0 contentOffset)
              let offsetLines = drop offset indexedLines
              let maxBytes = 46080
              let (finalLines, isTrunc) = truncateLines maxBytes offsetLines
              let actualStartLine = case finalLines of
                    (firstLine : _) -> lineIndex firstLine
                    []              -> start
              let actualEndLine = case reverse finalLines of
                    (lastLine : _)  -> lineIndex lastLine
                    []              -> start
              pure $ Right ViewFileResult
                { vfrPath          = fp
                , vfrTotalLines    = totalLines
                , vfrTotalBytes    = totalBytes
                , vfrStartLine     = actualStartLine
                , vfrEndLine       = actualEndLine
                , vfrLines         = finalLines
                , vfrIsTruncated   = isTrunc
                , vfrContentOffset = offset
                }

truncateLines :: Int -> [ViewFileLine] -> ([ViewFileLine], Bool)
truncateLines maxBytes linesList = go 0 [] linesList
  where
    go _ acc [] = (reverse acc, False)
    go curBytes acc (l:ls) =
      let lineBytes = T.length (lineText l) + 8
       in if curBytes + lineBytes > maxBytes && not (null acc)
            then (reverse acc, True)
            else go (curBytes + lineBytes) (l:acc) ls

-- | Type-safe 'Tool' for viewing files.
viewFileTool :: (World :> es) => Tool (Eff es)
viewFileTool = worldTool "view_file" "Read file contents with 1-indexed line slicing, max 800 lines" $ \args ->
  runViewFile args >>= \case
    Left err  -> pure (Left err)
    Right res -> pure (Right (toJSON res))

--------------------------------------------------------------------------------
-- 2. editFile Tool
--------------------------------------------------------------------------------

-- | Arguments for exact string replacement in a file.
data EditFileArgs = EditFileArgs
  { targetFile         :: !FilePath
  , instruction        :: !(Maybe Text)
  , targetContent      :: !Text
  , replacementContent :: !Text
  , startLine          :: !(Maybe Int)
  , endLine            :: !(Maybe Int)
  , allowMultiple      :: !(Maybe Bool)
  } deriving (Show, Eq, Generic, ToJSON, ToSchema)

instance FromJSON EditFileArgs where
  parseJSON = withObject "EditFileArgs" $ \o ->
    EditFileArgs
      <$> (oneOf o ["targetFile", "target_file", "filePath", "file_path"] `orElse` (o .: "path"))
      <*> o .:? "instruction"
      <*> (oneOf o ["targetContent", "target_content", "target", "oldContent", "old_content"] `orElse` pure "")
      <*> (oneOf o ["replacementContent", "replacement_content", "replacement", "newContent", "new_content"] `orElse` pure "")
      <*> oneOf o ["startLine", "start_line"]
      <*> oneOf o ["endLine", "end_line"]
      <*> oneOf o ["allowMultiple", "allow_multiple"]

-- | Result of editing a file.
data EditFileResult = EditFileResult
  { efrPath          :: !FilePath
  , efrReplacedCount :: !Int
  , efrLinesModified :: ![Int]
  , efrDiffSnippet   :: !Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

-- | Execute editFile operation against the 'World' effect.
runEditFile :: (World :> es) => EditFileArgs -> Eff es (Either Text EditFileResult)
runEditFile EditFileArgs{..} = do
  let fp = targetFile
  isDir <- doesDirectoryExist fp
  if isDir
    then pure (Left ("Path is a directory: " <> T.pack fp))
    else do
      exists <- doesFileExist fp
      if not exists
        then if T.null targetContent
          then do
            writeFileText fp replacementContent
            let diff = computeDiffSnippet fp "" replacementContent
            let modLines = if T.null replacementContent then [] else [1 .. length (T.lines replacementContent)]
            pure $ Right (EditFileResult fp 1 modLines diff)
          else pure (Left ("File not found: " <> T.pack fp))
        else do
          content <- readFileText fp
          if targetContent == replacementContent
            then pure $ Right (EditFileResult fp 0 [] "")
            else do
              let hasLineBounds = isJust startLine || isJust endLine
              if hasLineBounds
                then do
                  let allLines = T.lines content
                  let totalL = length allLines
                  let s = max 1 (fromMaybe 1 startLine)
                  let e = min totalL (fromMaybe totalL endLine)
                  if s > totalL || s > e
                    then pure (Left "Specified line range is invalid or out of bounds")
                    else do
                      let (preLines, rest) = splitAt (s - 1) allLines
                      let (targetLines, postLines) = splitAt (e - s + 1) rest
                      let targetChunk = T.intercalate "\n" targetLines
                      let occurrences = countSubstrings targetContent targetChunk
                      if occurrences == 0
                        then pure (Left "Target content not found in specified range")
                        else if occurrences > 1 && allowMultiple /= Just True
                          then pure (Left "Target content matched multiple times in specified range. Specify narrower line bounds or set allowMultiple to true")
                          else do
                            let replacedChunk = if allowMultiple == Just True
                                  then T.replace targetContent replacementContent targetChunk
                                  else replaceFirst targetContent replacementContent targetChunk
                            let newTargetLines = T.splitOn "\n" replacedChunk
                            let newAllLines = preLines ++ newTargetLines ++ postLines
                            let hadTrailingNewline = T.isSuffixOf "\n" content
                            let newContent = T.intercalate "\n" newAllLines <> (if hadTrailingNewline && not (null newAllLines) then "\n" else "")
                            writeFileText fp newContent
                            let diff = computeDiffSnippet fp content newContent
                            let modLines = computeModifiedLines content newContent
                            let count = if allowMultiple == Just True then occurrences else 1
                            pure $ Right (EditFileResult fp count modLines diff)
                else do
                  let occurrences = countSubstrings targetContent content
                  if occurrences == 0
                    then pure (Left ("Target content not found in file: " <> T.pack fp))
                    else if occurrences > 1 && allowMultiple /= Just True
                      then pure (Left ("Target content matched " <> T.pack (show occurrences) <> " times in file. Specify line bounds or set allowMultiple to true"))
                      else do
                        let newContent = if allowMultiple == Just True
                              then T.replace targetContent replacementContent content
                              else replaceFirst targetContent replacementContent content
                        writeFileText fp newContent
                        let diff = computeDiffSnippet fp content newContent
                        let modLines = computeModifiedLines content newContent
                        let count = if allowMultiple == Just True then occurrences else 1
                        pure $ Right (EditFileResult fp count modLines diff)

-- | Replace first occurrence of needle with replacement in haystack.
replaceFirst :: Text -> Text -> Text -> Text
replaceFirst needle replacement haystack =
  let (before, after) = T.breakOn needle haystack
   in if T.null after
        then haystack
        else before <> replacement <> T.drop (T.length needle) after

-- | Count occurrences of a non-empty substring.
countSubstrings :: Text -> Text -> Int
countSubstrings needle haystack
  | T.null needle = 0
  | otherwise = length (T.breakOnAll needle haystack)

-- | Compute unified diff snippet.
computeDiffSnippet :: FilePath -> Text -> Text -> Text
computeDiffSnippet fp oldContent newContent =
  let oldLines = T.lines oldContent
      newLines = T.lines newContent
      diffs = getDiff oldLines newLines
      formatLine = \case
        First l  -> "- " <> l
        Second l -> "+ " <> l
        Both l _ -> "  " <> l
      formatted = map formatLine diffs
      header = "--- a/" <> T.pack fp <> "\n+++ b/" <> T.pack fp <> "\n"
   in header <> T.unlines formatted

-- | Compute 1-indexed modified line numbers.
computeModifiedLines :: Text -> Text -> [Int]
computeModifiedLines oldContent newContent =
  let oldLines = T.lines oldContent
      newLines = T.lines newContent
      diffs = getDiff oldLines newLines
      go _ [] = []
      go lineNum (Both _ _ : rest) = go (lineNum + 1) rest
      go lineNum (First _ : rest)  = lineNum : go lineNum rest
      go lineNum (Second _ : rest) = lineNum : go (lineNum + 1) rest
   in sort (nub (go 1 diffs))

-- | Type-safe 'Tool' for editing files.
editFileTool :: (World :> es) => Tool (Eff es)
editFileTool = worldTool "edit_file" "Perform exact string replacement in a file with line validation" $ \args ->
  runEditFile args >>= \case
    Left err  -> pure (Left err)
    Right res -> pure (Right (toJSON res))

--------------------------------------------------------------------------------
-- 3. grepSearch Tool
--------------------------------------------------------------------------------

-- | Arguments for searching file contents.
data GrepSearchArgs = GrepSearchArgs
  { query           :: !Text
  , searchPath      :: !(Maybe FilePath)
  , isRegex         :: !(Maybe Bool)
  , caseInsensitive :: !(Maybe Bool)
  , includes        :: !(Maybe [Text])
  , matchPerLine    :: !(Maybe Bool)
  , maxMatches      :: !(Maybe Int)
  } deriving (Show, Eq, Generic, ToJSON, ToSchema)

instance FromJSON GrepSearchArgs where
  parseJSON = withObject "GrepSearchArgs" $ \o ->
    GrepSearchArgs
      <$> (oneOf o ["query", "pattern"] `orElse` (o .: "search_term"))
      <*> oneOf o ["searchPath", "search_path", "path", "directory"]
      <*> oneOf o ["isRegex", "is_regex"]
      <*> oneOf o ["caseInsensitive", "case_insensitive"]
      <*> oneOf o ["includes", "include"]
      <*> oneOf o ["matchPerLine", "match_per_line"]
      <*> oneOf o ["maxMatches", "max_matches"]

-- | A single search match.
data GrepMatch = GrepMatch
  { gmFilePath   :: !FilePath
  , gmLineNumber :: !(Maybe Int)
  , gmLineText   :: !(Maybe Text)
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

-- | Result of grep search.
data GrepSearchResult = GrepSearchResult
  { gsrMatches     :: ![GrepMatch]
  , gsrTotalCount  :: !Int
  , gsrIsTruncated :: !Bool
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

-- | Execute grep search operation against the 'World' effect.
runGrepSearch :: (World :> es) => GrepSearchArgs -> Eff es (Either Text GrepSearchResult)
runGrepSearch GrepSearchArgs{..} = do
  let dir = fromMaybe "." searchPath
  let maxM = fromMaybe 50 maxMatches
  let isReg = fromMaybe False isRegex
  let caseIns = fromMaybe False caseInsensitive
  let inc = fromMaybe [] includes
  let mpl = fromMaybe True matchPerLine
  let searchOpts = SearchOptions query dir isReg caseIns (Just (maxM + 1)) inc []
  results <- searchFiles searchOpts
  let allMatches = if mpl
        then [GrepMatch (smFile m) (Just (smLineNumber m)) (Just (smLineContent m)) | m <- results]
        else [GrepMatch f Nothing Nothing | f <- nub (map smFile results)]
  let isTrunc = length allMatches > maxM
  let finalMatches = take maxM allMatches
  pure $ Right (GrepSearchResult finalMatches (length allMatches) isTrunc)

-- | Type-safe 'Tool' for searching file contents.
grepSearchTool :: (World :> es) => Tool (Eff es)
grepSearchTool = worldTool "grep_search" "Search file contents by plain text or regex query" $ \args ->
  runGrepSearch args >>= \case
    Left err  -> pure (Left err)
    Right res -> pure (Right (toJSON res))

--------------------------------------------------------------------------------
-- 4. findByName Tool
--------------------------------------------------------------------------------

-- | Arguments for discovering files and directories by name pattern.
data FindByNameArgs = FindByNameArgs
  { pattern         :: !(Maybe Text)
  , searchDirectory :: !(Maybe FilePath)
  , itemType        :: !(Maybe Text)
  , maxDepth        :: !(Maybe Int)
  , excludes        :: !(Maybe [Text])
  , fullPath        :: !(Maybe Bool)
  } deriving (Show, Eq, Generic, ToJSON, ToSchema)

instance FromJSON FindByNameArgs where
  parseJSON = withObject "FindByNameArgs" $ \o ->
    FindByNameArgs
      <$> oneOf o ["pattern", "glob", "name"]
      <*> oneOf o ["searchDirectory", "search_directory", "searchDir", "search_dir", "directory", "path"]
      <*> oneOf o ["itemType", "item_type", "typeFilter", "type_filter", "type"]
      <*> oneOf o ["maxDepth", "max_depth", "depth"]
      <*> oneOf o ["excludes", "exclude"]
      <*> oneOf o ["fullPath", "full_path"]

-- | Discovered file or directory metadata.
data FileEntryInfo = FileEntryInfo
  { feiPath         :: !FilePath
  , feiType         :: !Text
  , feiSizeBytes    :: !(Maybe Integer)
  , feiModifiedTime :: !(Maybe Text)
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

-- | Result of findByName file discovery.
data FindByNameResult = FindByNameResult
  { fbrEntries     :: ![FileEntryInfo]
  , fbrTotalCount  :: !Int
  , fbrIsTruncated :: !Bool
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

-- | Execute findByName operation against the 'World' effect.
runFindByName :: (World :> es) => FindByNameArgs -> Eff es (Either Text FindByNameResult)
runFindByName FindByNameArgs{..} = do
  let dir = fromMaybe "." searchDirectory
  let filterType = case itemType of
        Just "file"      -> FindFilesOnly
        Just "files"     -> FindFilesOnly
        Just "directory" -> FindDirsOnly
        Just "dir"       -> FindDirsOnly
        Just "dirs"      -> FindDirsOnly
        _                -> FindAny
  let exc = fromMaybe [] excludes
  let findOpts = FindOptions dir pattern maxDepth filterType exc
  foundPaths <- findFiles findOpts
  entries <- mapM (\fp -> do
    isDir <- doesDirectoryExist fp
    pure $ FileEntryInfo fp (if isDir then "directory" else "file") Nothing Nothing
    ) foundPaths
  let maxCount = 100
  let isTrunc = length entries > maxCount
  let finalEntries = take maxCount entries
  pure $ Right (FindByNameResult finalEntries (length entries) isTrunc)

-- | Type-safe 'Tool' for discovering files by name pattern.
findByNameTool :: (World :> es) => Tool (Eff es)
findByNameTool = worldTool "find_by_name" "Find files and directories by name pattern and depth" $ \args ->
  runFindByName args >>= \case
    Left err  -> pure (Left err)
    Right res -> pure (Right (toJSON res))

--------------------------------------------------------------------------------
-- 5. listDir Tool
--------------------------------------------------------------------------------

-- | Arguments for listing directory contents.
data ListDirArgs = ListDirArgs
  { directoryPath :: !FilePath
  , recursive     :: !(Maybe Bool)
  , maxDepth      :: !(Maybe Int)
  } deriving (Show, Eq, Generic, ToJSON, ToSchema)

instance FromJSON ListDirArgs where
  parseJSON = withObject "ListDirArgs" $ \o ->
    ListDirArgs
      <$> (oneOf o ["directoryPath", "directory_path", "dir", "path"] `orElse` pure ".")
      <*> o .:? "recursive"
      <*> oneOf o ["maxDepth", "max_depth"]

-- | Directory entry item info.
data DirEntryInfo = DirEntryInfo
  { deiName       :: !Text
  , deiIsDir      :: !Bool
  , deiSizeBytes  :: !(Maybe Integer)
  , deiChildCount :: !(Maybe Int)
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

-- | Result of listDir.
data ListDirResult = ListDirResult
  { ldrPath    :: !FilePath
  , ldrEntries :: ![DirEntryInfo]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

-- | Execute listDir operation against the 'World' effect.
runListDir :: (World :> es) => ListDirArgs -> Eff es (Either Text ListDirResult)
runListDir ListDirArgs{..} = do
  let dir = directoryPath
  isDir <- doesDirectoryExist dir
  if not isDir
    then pure (Left ("Directory not found: " <> T.pack dir))
    else do
      let isRec = recursive == Just True || maybe False (> 1) maxDepth
      let effMaxDepth = case (recursive, maxDepth) of
            (Just True, Nothing) -> Nothing
            (Just True, Just d)  -> Just d
            (_, Just d)          -> Just d
            (_, Nothing)         -> Just 1
      entries <- if isRec
        then collectDirEntries dir dir 1 effMaxDepth
        else do
          rawEntries <- listDirectory dir
          pure [ DirEntryInfo
                   { deiName       = T.pack (deName e)
                   , deiIsDir      = deIsDir e
                   , deiSizeBytes  = if deIsDir e then Nothing else Just (deSize e)
                   , deiChildCount = Nothing
                   }
               | e <- rawEntries
               ]
      pure $ Right (ListDirResult dir entries)

collectDirEntries :: (World :> es) => FilePath -> FilePath -> Int -> Maybe Int -> Eff es [DirEntryInfo]
collectDirEntries baseDir currentDir currentDepth maxD = do
  rawEntries <- listDirectory currentDir
  subResults <- forM rawEntries $ \e -> do
    let childPath = currentDir </> deName e
    let relName = makeRelative baseDir childPath
    let isD = deIsDir e
    let sz = if isD then Nothing else Just (deSize e)
    let entryInfo = DirEntryInfo
          { deiName       = T.pack relName
          , deiIsDir      = isD
          , deiSizeBytes  = sz
          , deiChildCount = Nothing
          }
    if isD && maybe True (currentDepth + 1 <=) maxD
      then do
        children <- collectDirEntries baseDir childPath (currentDepth + 1) maxD
        pure (entryInfo : children)
      else pure [entryInfo]
  pure (concat subResults)

-- | Type-safe 'Tool' for listing directory contents.
listDirTool :: (World :> es) => Tool (Eff es)
listDirTool = worldTool "list_dir" "List files and subdirectories within a directory" $ \args ->
  runListDir args >>= \case
    Left err  -> pure (Left err)
    Right res -> pure (Right (toJSON res))

--------------------------------------------------------------------------------
-- 6. runCommand Tool
--------------------------------------------------------------------------------

-- | Arguments for running external shell commands.
data RunCommandArgs = RunCommandArgs
  { commandLine       :: !Text
  , cwd               :: !(Maybe FilePath)
  , timeoutMs         :: !(Maybe Int)
  , isDaemon          :: !(Maybe Bool)
  , waitMsBeforeAsync :: !(Maybe Int)
  } deriving (Show, Eq, Generic, ToJSON, ToSchema)

instance FromJSON RunCommandArgs where
  parseJSON = withObject "RunCommandArgs" $ \o ->
    RunCommandArgs
      <$> (oneOf o ["commandLine", "command_line", "command"] `orElse` (o .: "cmd"))
      <*> oneOf o ["cwd", "working_directory", "dir"]
      <*> oneOf o ["timeoutMs", "timeout_ms", "timeout"]
      <*> oneOf o ["isDaemon", "is_daemon", "daemon"]
      <*> oneOf o ["waitMsBeforeAsync", "wait_ms_before_async", "wait_ms"]

-- | Discriminated union result of command execution.
data RunCommandResult
  = CommandCompleted
      { rcrExitCode   :: !Int
      , rcrStdout     :: !Text
      , rcrStderr     :: !Text
      , rcrDurationMs :: !Double
      }
  | CommandBackgrounded
      { rcrTaskId        :: !Text
      , rcrInitialStdout :: !Text
      , rcrInitialStderr :: !Text
      , rcrStatus        :: !Text
      }
  | CommandTimedOut
      { rcrTimeoutMs     :: !Int
      , rcrPartialStdout :: !Text
      , rcrPartialStderr :: !Text
      }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

-- | Execute runCommand operation against the 'World' effect.
runRunCommand :: (World :> es) => RunCommandArgs -> Eff es (Either Text RunCommandResult)
runRunCommand RunCommandArgs{..} = do
  let cmd = commandLine
  let defaultTimeoutMs = 30000
  let effectiveTimeoutMs = Just (fromMaybe defaultTimeoutMs timeoutMs)
  let spec = CommandSpec "/bin/sh" ["-c", cmd] cwd Nothing effectiveTimeoutMs Nothing
  res <- runCommand spec
  if prTimedOut res
    then pure $ Right (CommandTimedOut (fromMaybe defaultTimeoutMs timeoutMs) (prStdout res) (prStderr res))
    else pure $ Right (CommandCompleted (prExitCode res) (prStdout res) (prStderr res) (prDurationMs res))

-- | Type-safe 'Tool' for running shell commands.
runCommandTool :: (World :> es) => Tool (Eff es)
runCommandTool = worldTool "run_command" "Execute a shell command with timeout and process tracking" $ \args ->
  runRunCommand args >>= \case
    Left err  -> pure (Left err)
    Right res -> pure (Right (toJSON res))

--------------------------------------------------------------------------------
-- Standard Coding Tools Collection
--------------------------------------------------------------------------------

-- | The canonical suite of six standard coding tools.
standardCodingTools :: (World :> es) => [Tool (Eff es)]
standardCodingTools =
  [ viewFileTool
  , editFileTool
  , grepSearchTool
  , findByNameTool
  , listDirTool
  , runCommandTool
  ]

-- | Project inspection tools with no write or process authority.
readOnlyCodingToolset :: (World :> es) => Toolset es
readOnlyCodingToolset =
  tools [viewFileTool, grepSearchTool, findByNameTool, listDirTool]

-- | All standard project tools.
standardCodingToolset :: (World :> es) => Toolset es
standardCodingToolset = tools standardCodingTools
