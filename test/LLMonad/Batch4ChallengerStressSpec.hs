{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | In-Depth Empirical Challenger Stress Suite for Batch 4:
-- Covers:
-- 1. High-Volume Concurrent Streaming & Deadlock Prevention (>1 MB stdin/stdout, OS pipe buffer overflow)
-- 2. Process Supervision, Deadlines, Signal Handling, and Zombie Reaping
-- 3. Coding Tools Default Timeouts, UTF-8 Multibyte & Truncation Edge Cases
-- 4. Directory Traversal & Search Depth Limits
module LLMonad.Batch4ChallengerStressSpec (spec) where

import Control.Concurrent.Async (forConcurrently_)
import Control.Monad (forM_)
import qualified Data.Text as T
import Effectful
import LLMonad
import System.Exit (ExitCode (..))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Hspec

spec :: Spec
spec = describe "Batch 4 Empirical Challenger Stress Suite" $ do

  describe "1. High-Volume Concurrent Streaming & Deadlock Prevention (>1 MB)" $ do
    it "streams 2 MB stdin through cat to stdout concurrently without deadlocking" $ do
      withSystemTempDirectory "b4-stress-2mb" $ \tmpDir -> do
        let line = T.replicate 100 "0123456789" <> "\n" -- 1001 chars
        let payload = T.concat (replicate 2000 line)     -- ~2,002,000 chars (> 2 MB)
        let specCat = CommandSpec "cat" [] Nothing Nothing (Just 10000) (Just payload)
        res <- runEff $ runWorldLocal tmpDir (runCommand specCat)
        prExitCode res `shouldBe` 0
        prTimedOut res `shouldBe` False
        T.length (prStdout res) `shouldBe` T.length payload
        prStdout res `shouldBe` payload

    it "streams 5 MB stdin through cat without pipe exhaustion deadlock" $ do
      withSystemTempDirectory "b4-stress-5mb" $ \tmpDir -> do
        let block = T.replicate 500 "ABCDEFGHIJ" <> "\n" -- 5001 chars
        let payload = T.concat (replicate 1000 block)    -- ~5,001,000 chars (> 5 MB)
        let specCat = CommandSpec "cat" [] Nothing Nothing (Just 15000) (Just payload)
        res <- runEff $ runWorldLocal tmpDir (runCommand specCat)
        prExitCode res `shouldBe` 0
        prTimedOut res `shouldBe` False
        T.length (prStdout res) `shouldBe` T.length payload

    it "handles child process producing 2 MB stdout without stdin" $ do
      withSystemTempDirectory "b4-stress-out-only" $ \tmpDir -> do
        let shCmd = "dd if=/dev/zero bs=1024 count=2048 2>/dev/null | tr '\\000' 'A'"
        let specGen = CommandSpec "/bin/sh" ["-c", T.pack shCmd] Nothing Nothing (Just 10000) Nothing
        res <- runEff $ runWorldLocal tmpDir (runCommand specGen)
        prExitCode res `shouldBe` 0
        prTimedOut res `shouldBe` False
        T.length (prStdout res) `shouldBe` (2048 * 1024)

    it "handles child process producing 1 MB stderr and 1 MB stdout simultaneously" $ do
      withSystemTempDirectory "b4-stress-both" $ \tmpDir -> do
        let shCmd = "dd if=/dev/zero bs=1024 count=1024 2>/dev/null | tr '\\000' 'O'; dd if=/dev/zero bs=1024 count=1024 2>/dev/null | tr '\\000' 'E' >&2"
        let specBoth = CommandSpec "/bin/sh" ["-c", T.pack shCmd] Nothing Nothing (Just 10000) Nothing
        res <- runEff $ runWorldLocal tmpDir (runCommand specBoth)
        prExitCode res `shouldBe` 0
        prTimedOut res `shouldBe` False
        T.length (prStdout res) `shouldBe` (1024 * 1024)
        T.length (prStderr res) `shouldBe` (1024 * 1024)

    it "handles early child exit (head -n 5) with 2 MB stdin without crash or hang" $ do
      withSystemTempDirectory "b4-stress-epipe" $ \tmpDir -> do
        let line = "line of text to be truncated\n"
        let payload = T.concat (replicate 80000 line) -- ~2.2 MB
        let specHead = CommandSpec "head" ["-n", "5"] Nothing Nothing (Just 5000) (Just payload)
        res <- runEff $ runWorldLocal tmpDir (runCommand specHead)
        prExitCode res `shouldBe` 0
        prTimedOut res `shouldBe` False
        length (T.lines (prStdout res)) `shouldBe` 5

    it "concurrently executes 20 local process commands without deadlock" $ do
      withSystemTempDirectory "b4-concur-cmd" $ \tmpDir -> do
        forConcurrently_ [1 .. 20 :: Int] $ \i -> do
          let specI = CommandSpec "echo" [T.pack (show i)] Nothing Nothing (Just 5000) Nothing
          res <- runEff $ runWorldLocal tmpDir (runCommand specI)
          prExitCode res `shouldBe` 0
          prStdout res `shouldBe` (T.pack (show i) <> "\n")

  describe "2. Process Supervision, Deadlines, Signal Handling & Zombie Reaping" $ do
    it "terminates long-sleeping process exactly at timeout threshold" $ do
      withSystemTempDirectory "b4-stress-timeout" $ \tmpDir -> do
        let specSleep = CommandSpec "sleep" ["10"] Nothing Nothing (Just 300) Nothing
        res <- runEff $ runWorldLocal tmpDir (runCommand specSleep)
        prTimedOut res `shouldBe` True
        prExitCode res `shouldBe` (-1)
        prStderr res `shouldBe` "Process timed out"
        prDurationMs res `shouldSatisfy` (\d -> d >= 250 && d <= 1500)

    it "terminates multi-process group trees on timeout" $ do
      withSystemTempDirectory "b4-stress-tree" $ \tmpDir -> do
        let cmd = "sleep 10 & sleep 10 & sleep 10 & wait"
        let specTree = CommandSpec "/bin/sh" ["-c", T.pack cmd] Nothing Nothing (Just 300) Nothing
        res <- runEff $ runWorldLocal tmpDir (runCommand specTree)
        prTimedOut res `shouldBe` True
        prExitCode res `shouldBe` (-1)

    it "terminates process that ignores SIGINT (trap '' INT) via process termination" $ do
      withSystemTempDirectory "b4-stress-sigint-ignore" $ \tmpDir -> do
        let cmd = "trap '' INT; sleep 10"
        let specIgnore = CommandSpec "/bin/sh" ["-c", T.pack cmd] Nothing Nothing (Just 300) Nothing
        res <- runEff $ runWorldLocal tmpDir (runCommand specIgnore)
        prTimedOut res `shouldBe` True
        prExitCode res `shouldBe` (-1)
        prStderr res `shouldBe` "Process timed out"
        prDurationMs res `shouldSatisfy` (\d -> d < 2000)

    it "reaps all child processes and leaves no zombies across repeated timeouts" $ do
      withSystemTempDirectory "b4-stress-zombies" $ \tmpDir -> do
        forM_ [1 .. 10 :: Int] $ \_ -> do
          let specSleep = CommandSpec "sleep" ["5"] Nothing Nothing (Just 100) Nothing
          res <- runEff $ runWorldLocal tmpDir (runCommand specSleep)
          prTimedOut res `shouldBe` True
          prExitCode res `shouldBe` (-1)
        (psExitCode, psOut, _) <- readProcessWithExitCode "ps" ["-ax", "-o", "stat,command"] ""
        psExitCode `shouldBe` ExitSuccess
        let defuncts = filter (\l -> "defunct" `T.isInfixOf` T.pack l || " Z " `T.isInfixOf` (" " <> T.pack l <> " ")) (lines psOut)
        let relevantDefuncts = filter (\l -> "sleep" `T.isInfixOf` T.pack l) defuncts
        relevantDefuncts `shouldBe` []

    it "executes processes without timeout (Nothing) to completion" $ do
      withSystemTempDirectory "b4-stress-notimeout" $ \tmpDir -> do
        let specEcho = CommandSpec "echo" ["hello no timeout"] Nothing Nothing Nothing Nothing
        res <- runEff $ runWorldLocal tmpDir (runCommand specEcho)
        prExitCode res `shouldBe` 0
        prTimedOut res `shouldBe` False
        T.strip (prStdout res) `shouldBe` "hello no timeout"

    it "handles non-zero exit codes (exit 42) and captures stderr" $ do
      withSystemTempDirectory "b4-exit-code" $ \tmpDir -> do
        let specFail = CommandSpec "/bin/sh" ["-c", "echo 'failed step' >&2; exit 42"] Nothing Nothing (Just 5000) Nothing
        res <- runEff $ runWorldLocal tmpDir (runCommand specFail)
        prExitCode res `shouldBe` 42
        prTimedOut res `shouldBe` False
        prStderr res `shouldBe` "failed step\n"

  describe "3. Coding Tools Default Timeouts, UTF-8 Multibyte & Truncation Edge Cases" $ do
    it "runRunCommand applies default 30s timeout when timeoutMs is Nothing" $ do
      let st = initMemoryWorld []
      (res, finalSt) <- runEff $ runWorldMemory st $ do
        runRunCommand (RunCommandArgs "echo bounded default" Nothing Nothing Nothing Nothing)
      case res of
        Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
        Right (CommandCompleted code stdout _ _) -> do
          code `shouldBe` 0
          stdout `shouldBe` "bounded default\n"
          case mwsCommandHistory finalSt of
            (cmd : _) -> cmdTimeoutMs cmd `shouldBe` Just 30000
            []        -> expectationFailure "Expected recorded command in history"
        Right other -> expectationFailure ("Expected CommandCompleted, got: " <> show other)

    it "runRunCommand returns CommandTimedOut when timeout is reached" $ do
      withSystemTempDirectory "b4-stress-cmd-timeout" $ \tmpDir -> do
        res <- runEff $ runWorldLocal tmpDir $ do
          runRunCommand (RunCommandArgs "sleep 5" Nothing (Just 200) Nothing Nothing)
        case res of
          Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
          Right (CommandTimedOut toMs _ err) -> do
            toMs `shouldBe` 200
            err `shouldBe` "Process timed out"
          Right other -> expectationFailure ("Expected CommandTimedOut, got: " <> show other)

    it "handles UTF-8 multibyte characters (emojis, non-ASCII) across byte truncation boundary in viewFile" $ do
      let lineTemplate i = "line_" <> T.pack (show (i :: Int)) <> "_🚀_λ_日本語_🎉_" <> T.replicate 80 "🔥"
      let content = T.unlines [ lineTemplate i | i <- [1 .. 500] ]
      let st = initMemoryWorld [("unicode_large.txt", content)]
      (res, _) <- runEff $ runWorldMemory st $ do
        runViewFile (ViewFileArgs "unicode_large.txt" (Just 1) (Just 500) Nothing)
      case res of
        Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
        Right vfr -> do
          vfrIsTruncated vfr `shouldBe` True
          let linesList = vfrLines vfr
          length linesList `shouldSatisfy` (> 0)
          length linesList `shouldSatisfy` (< 500)
          vfrStartLine vfr `shouldBe` 1
          vfrEndLine vfr `shouldBe` length linesList
          vfrEndLine vfr `shouldBe` lineIndex (last linesList)

    it "handles inverted range (startLine > endLine) clamping end to start" $ do
      let content = "line 1\nline 2\nline 3\nline 4\nline 5\n"
      let st = initMemoryWorld [("f.txt", content)]
      (res, _) <- runEff $ runWorldMemory st $ do
        runViewFile (ViewFileArgs "f.txt" (Just 4) (Just 2) Nothing)
      case res of
        Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
        Right vfr -> do
          vfrLines vfr `shouldBe` [ViewFileLine 4 "line 4"]
          vfrStartLine vfr `shouldBe` 4
          vfrEndLine vfr `shouldBe` 4

    it "handles negative startLine clamped to 1" $ do
      let content = "alpha\nbeta\ngamma\n"
      let st = initMemoryWorld [("f.txt", content)]
      (res, _) <- runEff $ runWorldMemory st $ do
        runViewFile (ViewFileArgs "f.txt" (Just (-10)) (Just 2) Nothing)
      case res of
        Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
        Right vfr -> do
          vfrStartLine vfr `shouldBe` 1
          vfrEndLine vfr `shouldBe` 2
          map lineText (vfrLines vfr) `shouldBe` ["alpha", "beta"]

    it "handles binary file with embedded null characters" $ do
      let binaryContent = "prefix\0null_byte\0suffix"
      let st = initMemoryWorld [("binary.dat", binaryContent)]
      (res, _) <- runEff $ runWorldMemory st $ do
        runViewFile (ViewFileArgs "binary.dat" Nothing Nothing Nothing)
      case res of
        Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
        Right vfr -> do
          vfrTotalLines vfr `shouldBe` 0
          vfrStartLine vfr `shouldBe` 0
          vfrEndLine vfr `shouldBe` 0
          vfrLines vfr `shouldBe` [ViewFileLine 0 "<binary content>"]

  describe "4. Directory Traversal & Search Depth Limits" $ do
    it "lists directory non-recursively when recursive is Nothing or False and maxDepth is Nothing" $ do
      let files =
            [ ("root/file1.txt", "1")
            , ("root/sub1/file2.txt", "2")
            , ("root/sub1/sub2/file3.txt", "3")
            ]
      let st = initMemoryWorld files
      (res, _) <- runEff $ runWorldMemory st $ do
        runListDir (ListDirArgs "root" Nothing Nothing)
      case res of
        Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
        Right ldr -> do
          let names = map deiName (ldrEntries ldr)
          names `shouldBe` ["file1.txt", "sub1"]

    it "lists directory recursively with maxDepth limit = 1" $ do
      let files =
            [ ("root/file1.txt", "1")
            , ("root/sub1/file2.txt", "2")
            , ("root/sub1/sub2/file3.txt", "3")
            ]
      let st = initMemoryWorld files
      (res, _) <- runEff $ runWorldMemory st $ do
        runListDir (ListDirArgs "root" (Just True) (Just 1))
      case res of
        Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
        Right ldr -> do
          let names = map deiName (ldrEntries ldr)
          names `shouldBe` ["file1.txt", "sub1"]

    it "lists directory recursively with maxDepth limit = 2" $ do
      let files =
            [ ("root/file1.txt", "1")
            , ("root/sub1/file2.txt", "2")
            , ("root/sub1/sub2/file3.txt", "3")
            , ("root/sub1/sub2/sub3/file4.txt", "4")
            ]
      let st = initMemoryWorld files
      (res, _) <- runEff $ runWorldMemory st $ do
        runListDir (ListDirArgs "root" (Just True) (Just 2))
      case res of
        Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
        Right ldr -> do
          let names = map deiName (ldrEntries ldr)
          names `shouldBe` ["file1.txt", "sub1", "sub1/file2.txt", "sub1/sub2"]

    it "lists directory recursively with maxDepth limit = 3" $ do
      let files =
            [ ("root/file1.txt", "1")
            , ("root/sub1/file2.txt", "2")
            , ("root/sub1/sub2/file3.txt", "3")
            , ("root/sub1/sub2/sub3/file4.txt", "4")
            ]
      let st = initMemoryWorld files
      (res, _) <- runEff $ runWorldMemory st $ do
        runListDir (ListDirArgs "root" (Just True) (Just 3))
      case res of
        Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
        Right ldr -> do
          let names = map deiName (ldrEntries ldr)
          names `shouldBe` ["file1.txt", "sub1", "sub1/file2.txt", "sub1/sub2", "sub1/sub2/file3.txt", "sub1/sub2/sub3"]

    it "lists directory recursively with unbounded depth when recursive = Just True and maxDepth = Nothing" $ do
      let files =
            [ ("root/a.txt", "a")
            , ("root/d1/b.txt", "b")
            , ("root/d1/d2/c.txt", "c")
            , ("root/d1/d2/d3/d.txt", "d")
            ]
      let st = initMemoryWorld files
      (res, _) <- runEff $ runWorldMemory st $ do
        runListDir (ListDirArgs "root" (Just True) Nothing)
      case res of
        Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
        Right ldr -> do
          let names = map deiName (ldrEntries ldr)
          names `shouldBe` ["a.txt", "d1", "d1/b.txt", "d1/d2", "d1/d2/c.txt", "d1/d2/d3", "d1/d2/d3/d.txt"]

    it "returns error on listDir with non-existent directory" $ do
      let st = initMemoryWorld []
      (res, _) <- runEff $ runWorldMemory st $ do
        runListDir (ListDirArgs "non_existent_folder" Nothing Nothing)
      case res of
        Left err -> err `shouldBe` "Directory not found: non_existent_folder"
        Right _  -> expectationFailure "Expected Left error for non-existent directory"

    it "findByName respects maxDepth, itemType, and excludes in local workspace" $ do
      withSystemTempDirectory "b4-stress-find" $ \tmpDir -> do
        runEff $ runWorldLocal tmpDir $ do
          createDirectory "src/Internal" True
          createDirectory "test" True
          createDirectory "dist-ignored" True
          writeFileText "src/Main.hs" "main"
          writeFileText "src/Internal/Core.hs" "core"
          writeFileText "test/Spec.hs" "spec"
          writeFileText "dist-ignored/Trash.hs" "trash"

          resFiles <- runFindByName (FindByNameArgs (Just ".hs") (Just ".") (Just "file") (Just 1) (Just ["dist-ignored"]) Nothing)
          case resFiles of
            Left err -> liftIO $ expectationFailure ("Unexpected error: " <> T.unpack err)
            Right fbr -> do
              let paths = map feiPath (fbrEntries fbr)
              liftIO $ paths `shouldMatchList` ["src/Main.hs", "test/Spec.hs"]

          resDirs <- runFindByName (FindByNameArgs Nothing (Just ".") (Just "directory") (Just 1) (Just ["dist-ignored"]) Nothing)
          case resDirs of
            Left err -> liftIO $ expectationFailure ("Unexpected error: " <> T.unpack err)
            Right fbr -> do
              let paths = map feiPath (fbrEntries fbr)
              liftIO $ paths `shouldMatchList` ["src", "src/Internal", "test"]

    it "grepSearch supports regex, case-insensitivity, and line matching" $ do
      withSystemTempDirectory "b4-stress-grep" $ \tmpDir -> do
        runEff $ runWorldLocal tmpDir $ do
          createDirectory "pkg" True
          writeFileText "pkg/Foo.hs" "module Foo where\n\nimport Data.Text\nfooVal :: Int\nfooVal = 42\n"
          writeFileText "pkg/Bar.hs" "module Bar where\n\nimport Data.Text\nbarVal :: Int\nbarVal = 100\n"

          res <- runGrepSearch (GrepSearchArgs "FOO*" (Just "pkg") (Just True) (Just True) Nothing (Just True) (Just 10))
          case res of
            Left err -> liftIO $ expectationFailure ("Unexpected error: " <> T.unpack err)
            Right gsr -> do
              liftIO $ gsrTotalCount gsr `shouldBe` 3
              let linesFound = [ (gmFilePath m, gmLineNumber m) | m <- gsrMatches gsr ]
              liftIO $ linesFound `shouldBe` [("pkg/Foo.hs", Just 1), ("pkg/Foo.hs", Just 4), ("pkg/Foo.hs", Just 5)]
