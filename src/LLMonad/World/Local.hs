{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Real workspace I/O interpreter for the World effect with timeout-guarded process execution.
module LLMonad.World.Local
  ( -- * Local World Interpreter
    runWorldLocal
  , resolveLocalPath
  , runLocalProcess
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, wait)
import qualified Control.Exception as E
import Control.Monad (forM, when)
import Data.List (sort, tails)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Effectful
import Effectful.Dispatch.Dynamic
import LLMonad.World (World (..))
import LLMonad.World.Types
import System.Directory
  ( canonicalizePath
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getFileSize
  , getModificationTime
  , removeFile
  )
import qualified System.Directory as SD
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, makeRelative, normalise, takeDirectory, (</>))
import System.IO (hClose, hFlush)
import System.Process
  ( CreateProcess (..)
  , StdStream (..)
  , createProcess
  , getProcessExitCode
  , interruptProcessGroupOf
  , proc
  , terminateProcess
  , waitForProcess
  )

-- | Run World effect in a real local workspace folder.
runWorldLocal :: (IOE :> es) => FilePath -> Eff (World : es) a -> Eff es a
runWorldLocal root action = do
  canonicalRoot <- liftIO (canonicalizePath root)
  interpret (localWorldHandler canonicalRoot) action

localWorldHandler :: (IOE :> es) => FilePath -> EffectHandler World es
localWorldHandler canonicalRoot _ = \case
  ReadFileText fp -> liftIO $ do
    let target = resolveLocalPath canonicalRoot fp
    isDir <- doesDirectoryExist target
    if isDir
      then E.throwIO (WorldIsADirectory fp)
      else do
        exists <- doesFileExist target
        if not exists
          then E.throwIO (WorldFileNotFound fp)
          else E.catch (TIO.readFile target) (\(e :: E.SomeException) -> E.throwIO (WorldIOError (T.pack (show e))))

  ReadFileSlice fp mStart mEnd -> liftIO $ do
    let target = resolveLocalPath canonicalRoot fp
    isDir <- doesDirectoryExist target
    if isDir
      then E.throwIO (WorldIsADirectory fp)
      else do
        exists <- doesFileExist target
        if not exists
          then E.throwIO (WorldFileNotFound fp)
          else do
            fullContent <- E.catch (TIO.readFile target) (\(e :: E.SomeException) -> E.throwIO (WorldIOError (T.pack (show e))))
            let allLines = T.lines fullContent
            let total = length allLines
            let s = maybe 1 (max 1) mStart
            let e = maybe total (max s) mEnd
            let slice = take (e - s + 1) (drop (s - 1) allLines)
            pure (T.unlines slice)

  WriteFileText fp content -> liftIO $ do
    let target = resolveLocalPath canonicalRoot fp
    isDir <- doesDirectoryExist target
    if isDir
      then E.throwIO (WorldIsADirectory fp)
      else do
        createDirectoryIfMissing True (takeDirectory target)
        E.catch (TIO.writeFile target content) (\(e :: E.SomeException) -> E.throwIO (WorldIOError (T.pack (show e))))

  DeleteFile fp -> liftIO $ do
    let target = resolveLocalPath canonicalRoot fp
    exists <- doesFileExist target
    if not exists
      then E.throwIO (WorldFileNotFound fp)
      else E.catch (removeFile target) (\(e :: E.SomeException) -> E.throwIO (WorldIOError (T.pack (show e))))

  CreateDirectory fp createParents -> liftIO $ do
    let target = resolveLocalPath canonicalRoot fp
    E.catch (createDirectoryIfMissing createParents target) (\(e :: E.SomeException) -> E.throwIO (WorldIOError (T.pack (show e))))

  ListDirectory fp -> liftIO $ do
    let target = resolveLocalPath canonicalRoot fp
    isDir <- doesDirectoryExist target
    if not isDir
      then E.throwIO (WorldDirectoryNotFound fp)
      else do
        names <- SD.listDirectory target
        entries <- forM (sort names) $ \name -> do
          let itemTarget = target </> name
          isD <- doesDirectoryExist itemTarget
          sz <- if isD then pure 0 else E.catch (getFileSize itemTarget) (\(_ :: E.SomeException) -> pure 0)
          mtime <- E.catch (Just <$> getModificationTime itemTarget) (\(_ :: E.SomeException) -> pure Nothing)
          pure DirEntry
            { deName     = name
            , dePath     = fp </> name
            , deIsDir    = isD
            , deSize     = sz
            , deModified = mtime
            }
        pure entries

  SearchFiles opts -> liftIO $ do
    let searchRoot = resolveLocalPath canonicalRoot (soSearchDir opts)
    exists <- doesDirectoryExist searchRoot
    if not exists
      then E.throwIO (WorldDirectoryNotFound (soSearchDir opts))
      else do
        allFiles <- collectFilesRecursively searchRoot
        let query = soQuery opts
        let isCaseInsensitive = soCaseInsensitive opts
        let maxMatches = soMaxMatches opts
        let includes = soIncludes opts
        let excludes = soExcludes opts

        let filterByPatterns path =
              let rel = makeRelative canonicalRoot path
                  hasInclude = null includes || any (`T.isInfixOf` T.pack rel) includes
                  hasExclude = not (null excludes) && any (`T.isInfixOf` T.pack rel) excludes
              in hasInclude && not hasExclude

        let matchingFiles = sort (filter filterByPatterns allFiles)

        matches <- forM matchingFiles $ \file -> do
          let relFile = makeRelative canonicalRoot file
          mContent <- E.catch (Just <$> TIO.readFile file) (\(_ :: E.SomeException) -> pure Nothing)
          case mContent of
            Nothing -> pure []
            Just content -> do
              let fileLines = zip [1 ..] (T.lines content)
              let fileMatches = [ SearchMatch relFile lineNum lineText
                                | (lineNum, lineText) <- fileLines
                                , matchLine query isCaseInsensitive (soIsRegex opts) lineText
                                ]
              pure fileMatches

        let flattened = concat matches
        pure (maybe flattened (`take` flattened) maxMatches)

  FindFiles opts -> liftIO $ do
    let searchRoot = resolveLocalPath canonicalRoot (foSearchDir opts)
    exists <- doesDirectoryExist searchRoot
    if not exists
      then E.throwIO (WorldDirectoryNotFound (foSearchDir opts))
      else do
        items <- collectFindEntries searchRoot (foMaxDepth opts)
        let pat = foPattern opts
        let ftype = foTypeFilter opts
        let excludes = foExcludes opts

        let keepEntry (path, isDir) =
              let rel = makeRelative canonicalRoot path
                  name = T.pack (makeRelative (takeDirectory path) path)
                  typeOk = case ftype of
                    FindAny -> True
                    FindFilesOnly -> not isDir
                    FindDirsOnly -> isDir
                  patOk = case pat of
                    Nothing -> True
                    Just p -> p `T.isInfixOf` name || p `T.isInfixOf` T.pack rel
                  exclOk = not (any (`T.isInfixOf` T.pack rel) excludes)
              in typeOk && patOk && exclOk

        let filtered = filter keepEntry items
        pure (map (makeRelative canonicalRoot . fst) filtered)

  DoesPathExist fp -> liftIO $ do
    let target = resolveLocalPath canonicalRoot fp
    fExists <- doesFileExist target
    dExists <- doesDirectoryExist target
    pure (fExists || dExists)

  DoesFileExist fp -> liftIO $ do
    let target = resolveLocalPath canonicalRoot fp
    doesFileExist target

  DoesDirectoryExist fp -> liftIO $ do
    let target = resolveLocalPath canonicalRoot fp
    doesDirectoryExist target

  GetWorkspaceRoot -> pure canonicalRoot

  RunCommand spec -> liftIO $ runLocalProcess spec canonicalRoot

-- | Resolve relative or absolute path against workspace root.
resolveLocalPath :: FilePath -> FilePath -> FilePath
resolveLocalPath root fp
  | isAbsolute fp = normalise fp
  | otherwise     = normalise (root </> fp)

-- | Execute an external process with environment, working directory, and timeout guards.
runLocalProcess :: CommandSpec -> FilePath -> IO ProcessResult
runLocalProcess spec defaultCwd = do
  startT <- getPOSIXTime
  let progStr = T.unpack (cmdProgram spec)
  let argsStr = map T.unpack (cmdArgs spec)
  let cwdStr  = maybe (Just defaultCwd) (Just . resolveLocalPath defaultCwd) (cmdCwd spec)
  let envList = fmap (map (\(k, v) -> (T.unpack k, T.unpack v))) (cmdEnv spec)
  let cp = (proc progStr argsStr)
        { cwd = cwdStr
        , env = envList
        , std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        , create_group = True
        }

  let timeoutUs = fmap (\ms -> ms * 1000) (cmdTimeoutMs spec)

  (minH, moutH, merrH, ph) <- createProcess cp
  case (minH, cmdStdin spec) of
    (Just inH, Just inText) -> do
      E.catch (TIO.hPutStr inH inText >> hFlush inH >> hClose inH) (\(_ :: E.SomeException) -> pure ())
    (Just inH, Nothing) -> hClose inH
    (Nothing, _) -> pure ()

  case (moutH, merrH) of
    (Just outH, Just errH) -> do
      outAsync <- async (TIO.hGetContents outH)
      errAsync <- async (TIO.hGetContents errH)

      (code, wasTimeout) <- case timeoutUs of
        Nothing -> do
          c <- waitForProcess ph
          pure (c, False)
        Just totalUs ->
          let pollLoop remainingUs
                | remainingUs <= 0 = do
                    E.catch (interruptProcessGroupOf ph) (\(_ :: E.SomeException) -> pure ())
                    E.catch (terminateProcess ph) (\(_ :: E.SomeException) -> pure ())
                    pure (ExitFailure (-1), True)
                | otherwise = do
                    mExit <- E.catch (getProcessExitCode ph) (\(_ :: E.SomeException) -> pure (Just (ExitFailure (-1))))
                    case mExit of
                      Just c -> pure (c, False)
                      Nothing -> do
                        let step = min remainingUs 5000 -- 5ms
                        threadDelay step
                        pollLoop (remainingUs - step)
          in pollLoop totalUs

      (outText, errText) <- if wasTimeout
        then do
          cancel outAsync
          cancel errAsync
          pure ("", "Process timed out")
        else do
          o <- wait outAsync
          e <- wait errAsync
          pure (o, e)
      hClose outH
      hClose errH

      endT <- getPOSIXTime
      let elapsedMs = realToFrac (endT - startT) * 1000.0

      if wasTimeout
        then pure ProcessResult
          { prExitCode   = -1
          , prStdout     = ""
          , prStderr     = "Process timed out"
          , prDurationMs = elapsedMs
          , prTimedOut   = True
          }
        else do
          let exitVal = case code of
                ExitSuccess -> 0
                ExitFailure n -> n
          pure ProcessResult
            { prExitCode   = exitVal
            , prStdout     = outText
            , prStderr     = errText
            , prDurationMs = elapsedMs
            , prTimedOut   = False
            }

    _ -> do
      code <- waitForProcess ph
      endT <- getPOSIXTime
      let elapsedMs = realToFrac (endT - startT) * 1000.0
      let exitVal = case code of
            ExitSuccess -> 0
            ExitFailure n -> n
      pure ProcessResult
        { prExitCode   = exitVal
        , prStdout     = ""
        , prStderr     = ""
        , prDurationMs = elapsedMs
        , prTimedOut   = False
        }

-- | Recursively collect all files under a directory, ignoring hidden directories.
collectFilesRecursively :: FilePath -> IO [FilePath]
collectFilesRecursively dir = do
  names <- SD.listDirectory dir
  let filtered = sort (filter (\n -> not (n == ".git" || n == "dist-newstyle" || n == ".agents")) names)
  paths <- forM filtered $ \name -> do
    let path = dir </> name
    isD <- doesDirectoryExist path
    if isD
      then collectFilesRecursively path
      else pure [path]
  pure (concat paths)

-- | Recursively collect entries with directory flag and depth tracking.
collectFindEntries :: FilePath -> Maybe Int -> IO [(FilePath, Bool)]
collectFindEntries root maxDepth = go root 0
  where
    go currentDir currentDepth = do
      when (maybe False (currentDepth >) maxDepth) $ pure ()
      names <- SD.listDirectory currentDir
      let filtered = filter (\n -> not (n == ".git" || n == "dist-newstyle" || n == ".agents")) names
      results <- forM filtered $ \name -> do
        let path = currentDir </> name
        isD <- doesDirectoryExist path
        let self = [(path, isD)]
        sub <- if isD && maybe True (currentDepth + 1 <=) maxDepth
                 then go path (currentDepth + 1)
                 else pure []
        pure (self ++ sub)
      pure (concat results)

-- | Check if a line matches the search query.
matchLine :: Text -> Bool -> Bool -> Text -> Bool
matchLine query isCaseInsensitive isRegex line =
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
    globMatch ('*' : ps) s = globMatch ps s || case s of
      "" -> False
      (_ : rest) -> globMatch ('*' : ps) rest
    globMatch (p : ps) (s : rest)
      | p == s    = globMatch ps rest
      | otherwise = False
    globMatch _ _ = False
