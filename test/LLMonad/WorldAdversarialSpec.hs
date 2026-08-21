{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Adversarial stress test suite for LLMonad.World across Local, Worktree, and Memory interpreters.
module LLMonad.WorldAdversarialSpec (spec) where

import Control.Concurrent.Async (forConcurrently_)
import Control.Monad (forM_)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Effectful
import qualified Effectful.Exception as EE
import LLMonad.World
import LLMonad.World.Local
import LLMonad.World.Memory
import LLMonad.World.Worktree
import qualified System.Directory as SD
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Hspec

spec :: Spec
spec = describe "LLMonad.World Adversarial Stress Suite" $ do

  describe "1. Deep Hierarchies, Boundary Paths & Edge Cases" $ do
    it "handles deep directory nesting (20 levels) in Local interpreter" $ do
      withSystemTempDirectory "llmonad-adv-deep" $ \tmpDir -> do
        let deepPath = "d1/d2/d3/d4/d5/d6/d7/d8/d9/d10/d11/d12/d13/d14/d15/d16/d17/d18/d19/d20/target.txt"
        let content = "deeply nested payload content"
        runEff $ runWorldLocal tmpDir $ do
          writeFileText deepPath content
          readBack <- readFileText deepPath
          pExists <- doesPathExist deepPath
          fExists <- doesFileExist deepPath
          dExists <- doesDirectoryExist deepPath
          liftIO $ do
            readBack `shouldBe` content
            pExists `shouldBe` True
            fExists `shouldBe` True
            dExists `shouldBe` False

    it "handles deep directory nesting in Memory interpreter" $ do
      let deepPath = "a/b/c/d/e/f/g/h/i/j/file.txt"
      let content = "virtual deep content"
      (res, finalSt) <- runEff $ runWorldMemory (initMemoryWorld []) $ do
        writeFileText deepPath content
        r <- readFileText deepPath
        slice <- readFileSlice deepPath (Just 1) (Just 1)
        pExists <- doesPathExist deepPath
        fExists <- doesFileExist deepPath
        dExists <- doesDirectoryExist "a/b/c"
        pure (r, slice, pExists, fExists, dExists)

      let (r, slice, pe, fe, de) = res
      r `shouldBe` content
      slice `shouldBe` (content <> "\n")
      pe `shouldBe` True
      fe `shouldBe` True
      de `shouldBe` True
      Map.lookup deepPath (mwsFiles finalSt) `shouldBe` Just content

    it "handles Unicode characters and whitespace in file paths and content" $ do
      withSystemTempDirectory "llmonad-adv-unicode" $ \tmpDir -> do
        let uniPath = "docs/日本語/🚀 file with spaces.md"
        let uniContent = "λ Haskell effectful 🤖 🚀\nLine 2: こんにちは\n"
        runEff $ runWorldLocal tmpDir $ do
          writeFileText uniPath uniContent
          readBack <- readFileText uniPath
          slice <- readFileSlice uniPath (Just 2) (Just 2)
          matches <- searchFiles (SearchOptions "こんにちは" "docs" False True Nothing [] [])
          liftIO $ do
            readBack `shouldBe` uniContent
            slice `shouldBe` "Line 2: こんにちは\n"
            length matches `shouldBe` 1

    it "handles 0-byte empty files and out-of-bounds slice queries" $ do
      withSystemTempDirectory "llmonad-adv-empty" $ \tmpDir -> do
        runEff $ runWorldLocal tmpDir $ do
          writeFileText "empty.txt" ""
          content <- readFileText "empty.txt"
          slice1 <- readFileSlice "empty.txt" (Just 1) (Just 10)
          slice2 <- readFileSlice "empty.txt" (Just 50) (Just 100)
          liftIO $ do
            content `shouldBe` ""
            slice1 `shouldBe` ""
            slice2 `shouldBe` ""

    it "throws WorldFileNotFound when deleting non-existent files" $ do
      withSystemTempDirectory "llmonad-adv-notfound" $ \tmpDir -> do
        let action = runEff $ runWorldLocal tmpDir (deleteFile "no/such/file.txt")
        action `shouldThrow` (\case WorldFileNotFound "no/such/file.txt" -> True; _ -> False)

    it "throws WorldIsADirectory when reading or writing a directory path" $ do
      withSystemTempDirectory "llmonad-adv-isdirectory" $ \tmpDir -> do
        let readAction = runEff $ runWorldLocal tmpDir $ do
              createDirectory "my-folder" True
              readFileText "my-folder"
        readAction `shouldThrow` (\case WorldIsADirectory "my-folder" -> True; _ -> False)

        let writeAction = runEff $ runWorldLocal tmpDir $ do
              createDirectory "existing-dir" True
              writeFileText "existing-dir" "new text"
        writeAction `shouldThrow` (\case WorldIsADirectory "existing-dir" -> True; _ -> False)

    it "correctly reports path existence predicate consistency" $ do
      withSystemTempDirectory "llmonad-adv-exists" $ \tmpDir -> do
        runEff $ runWorldLocal tmpDir $ do
          writeFileText "file.txt" "content"
          createDirectory "folder" True
          fPath <- doesPathExist "file.txt"
          fFile <- doesFileExist "file.txt"
          fDir  <- doesDirectoryExist "file.txt"
          dPath <- doesPathExist "folder"
          dFile <- doesFileExist "folder"
          dDir  <- doesDirectoryExist "folder"
          nPath <- doesPathExist "nonexistent"
          nFile <- doesFileExist "nonexistent"
          nDir  <- doesDirectoryExist "nonexistent"
          liftIO $ do
            fPath `shouldBe` True
            fFile `shouldBe` True
            fDir  `shouldBe` False
            dPath `shouldBe` True
            dFile `shouldBe` False
            dDir  `shouldBe` True
            nPath `shouldBe` False
            nFile `shouldBe` False
            nDir  `shouldBe` False

  describe "2. Process Execution, Stdin, Exit Codes & Timeouts" $ do
    it "captures non-zero exit codes correctly" $ do
      withSystemTempDirectory "llmonad-adv-exitcode" $ \tmpDir -> do
        runEff $ runWorldLocal tmpDir $ do
          res <- execShell "exit 42"
          liftIO $ do
            prExitCode res `shouldBe` 42
            prTimedOut res `shouldBe` False

    it "captures stderr output separately from stdout" $ do
      withSystemTempDirectory "llmonad-adv-stderr" $ \tmpDir -> do
        runEff $ runWorldLocal tmpDir $ do
          res <- execShell "echo standard out; echo error stream >&2"
          liftIO $ do
            prExitCode res `shouldBe` 0
            prStdout res `shouldBe` "standard out\n"
            prStderr res `shouldBe` "error stream\n"

    it "pipes stdin input into child process" $ do
      withSystemTempDirectory "llmonad-adv-stdin" $ \tmpDir -> do
        runEff $ runWorldLocal tmpDir $ do
          let specIn = CommandSpec "cat" [] Nothing Nothing Nothing (Just "stdin payload text")
          res <- runCommand specIn
          liftIO $ do
            prExitCode res `shouldBe` 0
            prStdout res `shouldBe` "stdin payload text"

    it "passes custom environment variables to child process" $ do
      withSystemTempDirectory "llmonad-adv-env" $ \tmpDir -> do
        runEff $ runWorldLocal tmpDir $ do
          let envVars = [("LLMONAD_TEST_VAR", "secret_12345")]
          let specEnv = CommandSpec "/bin/sh" ["-c", "echo $LLMONAD_TEST_VAR"] Nothing (Just envVars) Nothing Nothing
          res <- runCommand specEnv
          liftIO $ do
            prExitCode res `shouldBe` 0
            prStdout res `shouldBe` "secret_12345\n"

    it "terminates long-running process when timeout expires and sets timedOut = True" $ do
      withSystemTempDirectory "llmonad-adv-timeout" $ \tmpDir -> do
        runEff $ runWorldLocal tmpDir $ do
          -- sleep 10 seconds with 50ms timeout
          let specSleep = CommandSpec "sleep" ["10"] Nothing Nothing (Just 50) Nothing
          res <- runCommand specSleep
          liftIO $ do
            prTimedOut res `shouldBe` True
            prExitCode res `shouldBe` (-1)
            prStderr res `shouldBe` "Process timed out"

    it "completes successfully without timeout when command finishes within budget" $ do
      withSystemTempDirectory "llmonad-adv-notimeout" $ \tmpDir -> do
        runEff $ runWorldLocal tmpDir $ do
          let specFast = CommandSpec "echo" ["fast-result"] Nothing Nothing (Just 3000) Nothing
          res <- runCommand specFast
          liftIO $ do
            prTimedOut res `shouldBe` False
            prExitCode res `shouldBe` 0
            prStdout res `shouldBe` "fast-result\n"

    it "handles rapid sequential command executions cleanly" $ do
      withSystemTempDirectory "llmonad-adv-rapid" $ \tmpDir -> do
        runEff $ runWorldLocal tmpDir $ do
          results <- mapM (\i -> execShell ("echo " <> T.pack (show i))) ([1 .. 15] :: [Int])
          liftIO $ do
            length results `shouldBe` 15
            map prExitCode results `shouldBe` replicate 15 0
            map (T.strip . prStdout) results `shouldBe` map (T.pack . show) ([1 .. 15] :: [Int])

  describe "3. Git Worktree Lifecycle & Teardown Guarantees" $ do
    it "collects accurate diff for modified, added, and deleted files" $ do
      withSystemTempDirectory "llmonad-adv-wt-diff" $ \repoDir -> do
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "init"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.email", "adv@llmonad.org"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.name", "Adv Test"] ""
        writeFile (repoDir </> "existing.txt") "original line 1\noriginal line 2\n"
        writeFile (repoDir </> "to_delete.txt") "delete me\n"
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "add", "."] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "commit", "-m", "Initial state"] ""

        let cfg = defaultWorktreeConfig repoDir
        (_, summary) <- runEff $ runWorldWorktree cfg $ do
          writeFileText "existing.txt" "original line 1\nmodified line 2\n"
          writeFileText "created.txt" "new file\n"
          deleteFile "to_delete.txt"
          pure ()

        wsExitCode summary `shouldBe` 0
        wsDiff summary `shouldSatisfy` ("modified line 2" `T.isInfixOf`)
        -- Base repo must remain untouched
        baseExist <- readFile (repoDir </> "existing.txt")
        baseExist `shouldBe` "original line 1\noriginal line 2\n"
        baseToDelete <- SD.doesFileExist (repoDir </> "to_delete.txt")
        baseToDelete `shouldBe` True
        -- Temp worktree folder must be destroyed
        wtDirExists <- SD.doesDirectoryExist (wsPath summary)
        wtDirExists `shouldBe` False

    it "guarantees teardown on exception and removes ephemeral branch and directory" $ do
      withSystemTempDirectory "llmonad-adv-wt-exc" $ \repoDir -> do
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "init"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.email", "adv@llmonad.org"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.name", "Adv Test"] ""
        writeFile (repoDir </> "code.txt") "v1\n"
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "add", "code.txt"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "commit", "-m", "Init"] ""

        let cfg = defaultWorktreeConfig repoDir
        let failingAction = runEff $ runWorldWorktree cfg $ do
              writeFileText "code.txt" "dirty corrupted changes\n"
              writeFileText "untracked.txt" "untracked dirty\n"
              EE.throwIO (WorldIOError "failure inside sandbox")

        failingAction `shouldThrow` (\case WorldIOError "failure inside sandbox" -> True; _ -> False)

        -- Check that worktree list has no leaked worktrees
        (_, wtList, _) <- readProcessWithExitCode "git" ["-C", repoDir, "worktree", "list", "--porcelain"] ""
        let wtEntries = filter ("worktree " `T.isPrefixOf`) (T.lines (T.pack wtList))
        length wtEntries `shouldBe` 1

        -- Check that branch list has no leaked sandbox branches
        (_, branchList, _) <- readProcessWithExitCode "git" ["-C", repoDir, "branch"] ""
        T.pack branchList `shouldNotSatisfy` ("llmonad-sandbox" `T.isInfixOf`)

    it "supports sequential worktrees without collision" $ do
      withSystemTempDirectory "llmonad-adv-wt-seq" $ \repoDir -> do
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "init"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.email", "adv@llmonad.org"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.name", "Adv Test"] ""
        writeFile (repoDir </> "base.txt") "base\n"
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "add", "base.txt"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "commit", "-m", "Init"] ""

        let cfg = defaultWorktreeConfig repoDir
        forM_ [1 .. 3] $ \i -> do
          (val, _) <- runEff $ runWorldWorktree cfg $ do
            writeFileText "iter.txt" (T.pack (show (i :: Int)))
            readFileText "iter.txt"
          val `shouldBe` T.pack (show (i :: Int))

        (_, wtList, _) <- readProcessWithExitCode "git" ["-C", repoDir, "worktree", "list", "--porcelain"] ""
        let wtEntries = filter ("worktree " `T.isPrefixOf`) (T.lines (T.pack wtList))
        length wtEntries `shouldBe` 1

    it "supports concurrent worktrees without branch name collision" $ do
      withSystemTempDirectory "llmonad-adv-wt-concurrent" $ \repoDir -> do
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "init"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.email", "adv@llmonad.org"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.name", "Adv Test"] ""
        writeFile (repoDir </> "base.txt") "base\n"
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "add", "base.txt"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "commit", "-m", "Init"] ""

        let cfg = defaultWorktreeConfig repoDir
        forConcurrently_ [1 .. 5] $ \i -> do
          (val, summary) <- runEff $ runWorldWorktree cfg $ do
            writeFileText "thread.txt" (T.pack (show (i :: Int)))
            readFileText "thread.txt"
          val `shouldBe` T.pack (show (i :: Int))
          wsExitCode summary `shouldBe` 0

        (_, wtList, _) <- readProcessWithExitCode "git" ["-C", repoDir, "worktree", "list", "--porcelain"] ""
        let wtEntries = filter ("worktree " `T.isPrefixOf`) (T.lines (T.pack wtList))
        length wtEntries `shouldBe` 1

        (_, branchList, _) <- readProcessWithExitCode "git" ["-C", repoDir, "branch"] ""
        T.pack branchList `shouldNotSatisfy` ("llmonad-sandbox" `T.isInfixOf`)

    it "throws WorldGitError when given an invalid base ref" $ do
      withSystemTempDirectory "llmonad-adv-wt-invalid-ref" $ \repoDir -> do
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "init"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.email", "adv@llmonad.org"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.name", "Adv Test"] ""
        writeFile (repoDir </> "base.txt") "base\n"
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "add", "base.txt"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "commit", "-m", "Init"] ""

        let cfg = (defaultWorktreeConfig repoDir) { wtBaseRef = "invalid-ref-nonexistent-12345" }
        let action = runEff $ runWorldWorktree cfg (readFileText "base.txt")
        action `shouldThrow` (\case WorldGitError _ -> True; _ -> False)

  describe "4. Determinism & Search Features in Memory Interpreter" $ do
    it "ensures identical state across deterministic runs" $ do
      let ops = do
            writeFileText "src/A.hs" "module A where\nx = 1\n"
            writeFileText "src/B.hs" "module B where\ny = 2\n"
            deleteFile "src/A.hs"
            writeFileText "src/C.hs" "module C where\nz = 3\n"
            _ <- runCommand (CommandSpec "echo" ["test"] Nothing Nothing Nothing Nothing)
            _ <- runCommand (CommandSpec "cat" ["src/B.hs"] Nothing Nothing Nothing Nothing)
            getWorkspaceRoot

      (r1, st1) <- runEff $ runWorldMemory (initMemoryWorld []) ops
      (r2, st2) <- runEff $ runWorldMemory (initMemoryWorld []) ops

      r1 `shouldBe` r2
      mwsFiles st1 `shouldBe` mwsFiles st2
      length (mwsCommandHistory st1) `shouldBe` length (mwsCommandHistory st2)

    it "supports searchFiles with includes and excludes in Memory" $ do
      let files =
            [ ("src/App.hs", "import Data.Text\nmain = putStrLn \"hello\"\n")
            , ("src/App_test.hs", "testMain = putStrLn \"hello test\"\n")
            , ("docs/Guide.md", "hello world in guide\n")
            , ("vendor/Lib.hs", "hello in vendor\n")
            ]
      (matches, _) <- runEff $ runWorldMemory (initMemoryWorld files) $ do
        searchFiles (SearchOptions "hello" "" False True Nothing [".hs"] ["vendor", "_test"])

      length matches `shouldBe` 1
      case matches of
        (m:_) -> smFile m `shouldBe` "src/App.hs"
        []    -> expectationFailure "matches list was empty"

    it "throws WorldDirectoryNotFound when listing non-existent directory in Memory" $ do
      let action = runEff $ runWorldMemory (initMemoryWorld []) $ do
            listDirectory "non_existent_folder"
      action `shouldThrow` (\case WorldDirectoryNotFound "non_existent_folder" -> True; _ -> False)

    it "throws WorldDirectoryNotFound when searching non-existent directory in Memory" $ do
      let action = runEff $ runWorldMemory (initMemoryWorld []) $ do
            searchFiles (SearchOptions "query" "missing_dir" False False Nothing [] [])
      action `shouldThrow` (\case WorldDirectoryNotFound "missing_dir" -> True; _ -> False)

    it "throws WorldDirectoryNotFound when finding in non-existent directory in Memory" $ do
      let action = runEff $ runWorldMemory (initMemoryWorld []) $ do
            findFiles (FindOptions "missing_dir" Nothing Nothing FindAny [])
      action `shouldThrow` (\case WorldDirectoryNotFound "missing_dir" -> True; _ -> False)

    it "supports regex and wildcard glob pattern searching in Memory" $ do
      let files =
            [ ("src/A.hs", "functionAlphaBar = 10\n")
            , ("src/B.hs", "functionBeta = 20\n")
            ]
      (matches, _) <- runEff $ runWorldMemory (initMemoryWorld files) $ do
        searchFiles (SearchOptions "function*Bar" "src" True False Nothing [] [])
      length matches `shouldBe` 1
      case matches of
        (m:_) -> smFile m `shouldBe` "src/A.hs"
        []    -> expectationFailure "matches list was empty"

    it "supports findFiles with depth limit and directory filters in Memory" $ do
      let files =
            [ ("root.txt", "root")
            , ("level1/a.txt", "a")
            , ("level1/level2/b.txt", "b")
            , ("level1/level2/level3/c.txt", "c")
            ]
      (shallowFiles, _) <- runEff $ runWorldMemory (initMemoryWorld files) $ do
        findFiles (FindOptions "" Nothing (Just 2) FindFilesOnly [])

      (onlyDirs, _) <- runEff $ runWorldMemory (initMemoryWorld files) $ do
        findFiles (FindOptions "" Nothing Nothing FindDirsOnly [])

      sort shallowFiles `shouldBe` ["level1/a.txt", "level1/level2/b.txt", "root.txt"]
      sort onlyDirs `shouldBe` ["level1", "level1/level2", "level1/level2/level3"]

