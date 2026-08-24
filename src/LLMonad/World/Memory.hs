{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Pure in-memory simulated World interpreter for deterministic testing with zero disk I/O.
module LLMonad.World.Memory (
    -- * Pure Memory World Interpreter
    runWorldMemory,
    runWorldMemorySimple,
    runWorldMemoryWithFiles,
    initMemoryWorld,
    defaultCommandHandlers,
) where

import Control.Exception (throw)
import Data.List (isPrefixOf, nub, sort, tails)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import LLMonad.World (World (..))
import LLMonad.World.Types
import System.FilePath (makeRelative, normalise, takeDirectory, (</>))

-- | Run World effect purely in memory using provided state.
runWorldMemory ::
    MemoryWorldState ->
    Eff (World : es) a ->
    Eff es (a, MemoryWorldState)
runWorldMemory st action = do
    reinterpret (runState st) memoryWorldHandler action

-- | Simplified in-memory interpreter starting from empty state.
runWorldMemorySimple :: Eff (World : es) a -> Eff es a
runWorldMemorySimple action = do
    (res, _) <- runWorldMemory (initMemoryWorld []) action
    pure res

-- | Run in-memory interpreter initialized with a list of file paths and contents.
runWorldMemoryWithFiles ::
    [(FilePath, Text)] ->
    Eff (World : es) a ->
    Eff es (a, MemoryWorldState)
runWorldMemoryWithFiles files = runWorldMemory (initMemoryWorld files)

memoryWorldHandler :: EffectHandler World (State MemoryWorldState : es)
memoryWorldHandler _ = \case
    ReadFileText fp -> do
        st <- get
        let key = canonicalMemKey (mwsWorkingDir st) fp
        case Map.lookup key (mwsFiles st) of
            Just content -> pure content
            Nothing ->
                if isMemDir (mwsFiles st) key
                    then throw (WorldIsADirectory fp)
                    else throw (WorldFileNotFound fp)
    ReadFileSlice fp mStart mEnd -> do
        st <- get
        let key = canonicalMemKey (mwsWorkingDir st) fp
        case Map.lookup key (mwsFiles st) of
            Nothing ->
                if isMemDir (mwsFiles st) key
                    then throw (WorldIsADirectory fp)
                    else throw (WorldFileNotFound fp)
            Just content -> do
                let allLines = T.lines content
                let total = length allLines
                let s = maybe 1 (max 1) mStart
                let e = maybe total (max s) mEnd
                let slice = take (e - s + 1) (drop (s - 1) allLines)
                pure (T.unlines slice)
    WriteFileText fp content -> do
        st <- get
        let key = canonicalMemKey (mwsWorkingDir st) fp
        if isMemDir (mwsFiles st) key
            then throw (WorldIsADirectory fp)
            else modify (\s -> s{mwsFiles = Map.insert key content (mwsFiles s)})
    DeleteFile fp -> do
        st <- get
        let key = canonicalMemKey (mwsWorkingDir st) fp
        case Map.lookup key (mwsFiles st) of
            Nothing -> throw (WorldFileNotFound fp)
            Just _ -> modify (\s -> s{mwsFiles = Map.delete key (mwsFiles s)})
    CreateDirectory _ _ ->
        pure ()
    ListDirectory fp -> do
        st <- get
        let prefix = canonicalMemKey (mwsWorkingDir st) fp
        if not (isMemDir (mwsFiles st) prefix)
            then throw (WorldDirectoryNotFound fp)
            else do
                let files = Map.keys (mwsFiles st)
                let childEntries = getChildEntries prefix files
                pure childEntries
    SearchFiles opts -> do
        st <- get
        let searchPrefix = canonicalMemKey (mwsWorkingDir st) (soSearchDir opts)
        if not (isMemDir (mwsFiles st) searchPrefix)
            then throw (WorldDirectoryNotFound (soSearchDir opts))
            else do
                let query = soQuery opts
                let isCaseInsensitive = soCaseInsensitive opts
                let isRegex = soIsRegex opts
                let maxMatches = soMaxMatches opts
                let includes = soIncludes opts
                let excludes = soExcludes opts

                let allFiles = Map.toList (mwsFiles st)
                let inDir (k, _) = null searchPrefix || (searchPrefix ++ "/") `isPrefixOf` k || searchPrefix == k

                let filterPatterns (k, _) =
                        let hasInc = null includes || any (`T.isInfixOf` T.pack k) includes
                            hasExc = not (null excludes) && any (`T.isInfixOf` T.pack k) excludes
                         in hasInc && not hasExc

                let targetFiles = filter (\item -> inDir item && filterPatterns item) allFiles

                let matches =
                        concatMap
                            ( \(path, content) ->
                                let fileLines = zip [1 ..] (T.lines content)
                                 in [ SearchMatch path lineNum lineText
                                    | (lineNum, lineText) <- fileLines
                                    , matchLineMem query isCaseInsensitive isRegex lineText
                                    ]
                            )
                            targetFiles

                pure (maybe matches (`take` matches) maxMatches)
    FindFiles opts -> do
        st <- get
        let searchPrefix = canonicalMemKey (mwsWorkingDir st) (foSearchDir opts)
        if not (isMemDir (mwsFiles st) searchPrefix)
            then throw (WorldDirectoryNotFound (foSearchDir opts))
            else do
                let pat = foPattern opts
                let ftype = foTypeFilter opts
                let excludes = foExcludes opts
                let maxDepth = foMaxDepth opts

                let allEntries = getAllMemEntries searchPrefix (Map.keys (mwsFiles st))
                let keepEntry (path, isDir, depth) =
                        let name = T.pack (makeRelative (takeDirectory path) path)
                            typeOk = case ftype of
                                FindAny -> True
                                FindFilesOnly -> not isDir
                                FindDirsOnly -> isDir
                            patOk = case pat of
                                Nothing -> True
                                Just p -> p `T.isInfixOf` name || p `T.isInfixOf` T.pack path
                            exclOk = not (any (`T.isInfixOf` T.pack path) excludes)
                            depthOk = maybe True (depth <=) maxDepth
                         in typeOk && patOk && exclOk && depthOk

                let filtered = filter keepEntry allEntries
                pure (map (\(p, _, _) -> p) filtered)
    DoesPathExist fp -> do
        st <- get
        let key = canonicalMemKey (mwsWorkingDir st) fp
        pure (Map.member key (mwsFiles st) || isMemDir (mwsFiles st) key)
    DoesFileExist fp -> do
        st <- get
        let key = canonicalMemKey (mwsWorkingDir st) fp
        pure (Map.member key (mwsFiles st))
    DoesDirectoryExist fp -> do
        st <- get
        let key = canonicalMemKey (mwsWorkingDir st) fp
        pure (null key || isMemDir (mwsFiles st) key)
    GetWorkspaceRoot -> do
        gets mwsWorkingDir
    RunCommand spec -> do
        st <- get
        modify (\s -> s{mwsCommandHistory = mwsCommandHistory s ++ [spec]})
        case Map.lookup (cmdProgram spec) (mwsCommandHandlers st) of
            Just handler -> pure (handler spec)
            Nothing -> pure (simulateCommand spec st)

-- | Normalize path relative to working directory into a uniform key without leading slash.
canonicalMemKey :: FilePath -> FilePath -> FilePath
canonicalMemKey wd fp = case fp of
    "" -> ""
    "." -> ""
    '/' : _ -> dropWhile (== '/') (normalise fp)
    _ -> dropWhile (== '/') (normalise (wd </> fp))

-- | Check if path acts as a directory prefix for any existing files.
isMemDir :: Map FilePath Text -> FilePath -> Bool
isMemDir files dirKey
    | null dirKey = True
    | otherwise =
        let prefix = dirKey ++ "/"
         in any (prefix `isPrefixOf`) (Map.keys files)

-- | Compute direct child DirEntries for a directory prefix.
getChildEntries :: FilePath -> [FilePath] -> [DirEntry]
getChildEntries dirPrefix allFiles =
    let relevantFiles =
            if null dirPrefix
                then allFiles
                else [drop (length dirPrefix + 1) f | f <- allFiles, (dirPrefix ++ "/") `isPrefixOf` f]
        firstSegments = [takeWhile (/= '/') f | f <- relevantFiles, not (null f)]
        uniqueNames = sort (nub firstSegments)
     in [ DirEntry
            { deName = name
            , dePath = if null dirPrefix then name else dirPrefix </> name
            , deIsDir = any (\f -> (name ++ "/") `isPrefixOf` f) relevantFiles
            , deSize = 0
            , deModified = Nothing
            }
        | name <- uniqueNames
        ]

-- | Collect all nested entries under prefix with depth and directory flag.
getAllMemEntries :: FilePath -> [FilePath] -> [(FilePath, Bool, Int)]
getAllMemEntries prefix allFiles =
    let filtered = if null prefix then allFiles else [f | f <- allFiles, (prefix ++ "/") `isPrefixOf` f]
        fileEntries = [(f, False, pathDepth (if null prefix then f else makeRelative prefix f)) | f <- filtered]
        allParentDirs p =
            let d = takeDirectory p
             in if d == "." || d == "" || d == "/"
                    then []
                    else d : allParentDirs d
        allDirs = nub (concatMap allParentDirs filtered)
        dirEntries =
            [ (d, True, pathDepth (if null prefix then d else makeRelative prefix d))
            | d <- allDirs
            , null prefix || (d /= prefix && ((prefix ++ "/") `isPrefixOf` d))
            ]
     in fileEntries ++ dirEntries

pathDepth :: FilePath -> Int
pathDepth p = length (filter (== '/') (normalise p))

matchLineMem :: Text -> Bool -> Bool -> Text -> Bool
matchLineMem query isCaseInsensitive isRegex line =
    let q = if isCaseInsensitive then T.toLower query else query
        l = if isCaseInsensitive then T.toLower line else line
     in if isRegex
            then simpleGlobMatch (T.unpack q) (T.unpack l)
            else q `T.isInfixOf` l

-- | Wildcard glob matching supporting '*' wildcards anywhere in the line.
simpleGlobMatch :: String -> String -> Bool
simpleGlobMatch pat str = any (globMatch pat) (tails str)
  where
    globMatch "" _ = True
    globMatch ('*' : ps) s =
        globMatch ps s || case s of
            "" -> False
            (_ : rest) -> globMatch ('*' : ps) rest
    globMatch (p : ps) (s : rest)
        | p == s = globMatch ps rest
        | otherwise = False
    globMatch _ _ = False

-- | Simulate default command executions.
simulateCommand :: CommandSpec -> MemoryWorldState -> ProcessResult
simulateCommand spec st = case cmdProgram spec of
    prog | prog `elem` ["sh", "/bin/sh", "bash", "/bin/bash", "/usr/bin/env"] ->
        case cmdArgs spec of
            ["-c", inner] -> simulateShell inner spec st
            ("sh" : "-c" : inner : _) -> simulateShell inner spec st
            _ -> ProcessResult 0 ("simulated output for: " <> prog) "" 1.0 False
    "echo" ->
        ProcessResult 0 (T.unwords (cmdArgs spec) <> "\n") "" 1.0 False
    "pwd" ->
        ProcessResult 0 (T.pack (mwsWorkingDir st) <> "\n") "" 1.0 False
    "cat" ->
        let contents =
                [ Map.findWithDefault "" (canonicalMemKey (mwsWorkingDir st) (T.unpack arg)) (mwsFiles st)
                | arg <- cmdArgs spec
                ]
         in ProcessResult 0 (T.concat contents) "" 1.0 False
    "true" ->
        ProcessResult 0 "" "" 1.0 False
    "false" ->
        ProcessResult 1 "" "command failed" 1.0 False
    other ->
        ProcessResult 0 ("simulated output for: " <> other) "" 1.0 False

simulateShell :: Text -> CommandSpec -> MemoryWorldState -> ProcessResult
simulateShell inner _ st
    | "echo " `T.isPrefixOf` inner =
        let msg = T.drop 5 inner
            cleanMsg =
                if (T.isPrefixOf "\"" msg && T.isSuffixOf "\"" msg) || (T.isPrefixOf "'" msg && T.isSuffixOf "'" msg)
                    then T.init (T.tail msg)
                    else msg
         in ProcessResult 0 (cleanMsg <> "\n") "" 1.0 False
    | inner == "echo" =
        ProcessResult 0 "\n" "" 1.0 False
    | inner == "pwd" =
        ProcessResult 0 (T.pack (mwsWorkingDir st) <> "\n") "" 1.0 False
    | inner == "true" =
        ProcessResult 0 "" "" 1.0 False
    | inner == "false" =
        ProcessResult 1 "" "command failed" 1.0 False
    | "cat " `T.isPrefixOf` inner =
        let path = T.unpack (T.strip (T.drop 4 inner))
            content = Map.findWithDefault "" (canonicalMemKey (mwsWorkingDir st) path) (mwsFiles st)
         in ProcessResult 0 content "" 1.0 False
    | otherwise =
        ProcessResult 0 ("simulated output for: " <> inner) "" 1.0 False

-- | Default empty command handler map.
defaultCommandHandlers :: Map Text (CommandSpec -> ProcessResult)
defaultCommandHandlers = Map.empty
