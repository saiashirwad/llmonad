{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Comprehensive test suite verifying LLMonad.World across Local, Worktree, and Memory interpreters.
module LLMonad.WorldSpec (spec) where

import Control.Concurrent.Async (forConcurrently_)
import Control.Monad (forM_)
import Data.Aeson (decode, encode)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Exception qualified as EE
import LLMonad.World
import LLMonad.World.Local
import LLMonad.World.Memory
import LLMonad.World.Worktree
import System.Directory qualified as SD
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Hspec

spec :: Spec
spec = describe "LLMonad.World (Milestone 1)" $ do
    describe "Types and JSON Serialization" $ do
        it "roundtrips ProcessResult via JSON" $ do
            let pr = ProcessResult 0 "output text\n" "error text\n" 12.5 False
            decode (encode pr) `shouldBe` Just pr

        it "roundtrips DirEntry via JSON" $ do
            let de = DirEntry "file.txt" "dir/file.txt" False 1024 Nothing
            decode (encode de) `shouldBe` Just de

        it "roundtrips SearchMatch via JSON" $ do
            let sm = SearchMatch "src/Main.hs" 42 "main = putStrLn \"hello\""
            decode (encode sm) `shouldBe` Just sm

        it "roundtrips WorldError constructors via JSON" $ do
            let err1 = WorldFileNotFound "/tmp/test.txt"
            let err2 = WorldCommandTimeout 5000 "long-task"
            let err3 = WorldGitError "branch not found"
            decode (encode err1) `shouldBe` Just err1
            decode (encode err2) `shouldBe` Just err2
            decode (encode err3) `shouldBe` Just err3

        it "provides prettyWorldError descriptions" $ do
            prettyWorldError (WorldFileNotFound "foo.txt") `shouldBe` "File not found: foo.txt"
            prettyWorldError (WorldCommandTimeout 200 "sleep") `shouldBe` "Command timed out after 200ms: sleep"

    describe "Pure In-Memory World Interpreter (runWorldMemory)" $ do
        it "performs read, write, slice, and delete on virtual files" $ do
            let initialFiles =
                    [ ("src/Main.hs", "module Main where\n\nmain :: IO ()\nmain = pure ()\n")
                    , ("README.md", "# Project\nPure memory test\n")
                    ]
            (res, finalSt) <- runEff $ runWorldMemory (initMemoryWorld initialFiles) $ do
                m1 <- readFileText "src/Main.hs"
                slice <- readFileSlice "src/Main.hs" (Just 3) (Just 4)
                writeFileText "notes.txt" "important note"
                deleteFile "README.md"
                r1 <- doesFileExist "README.md"
                r2 <- doesFileExist "notes.txt"
                pure (m1, slice, r1, r2)

            let (mainContent, sliceText, readmeExists, notesExists) = res
            mainContent `shouldSatisfy` ("module Main" `T.isPrefixOf`)
            sliceText `shouldBe` "main :: IO ()\nmain = pure ()\n"
            readmeExists `shouldBe` False
            notesExists `shouldBe` True
            Map.lookup "notes.txt" (mwsFiles finalSt) `shouldBe` Just "important note"
            Map.member "README.md" (mwsFiles finalSt) `shouldBe` False

        it "throws WorldFileNotFound for missing virtual files" $ do
            let action = runEff $ runWorldMemory (initMemoryWorld []) (readFileText "missing.txt")
            action `shouldThrow` (\case WorldFileNotFound "missing.txt" -> True; _ -> False)

        it "throws WorldIsADirectory when reading a directory as a file in memory" $ do
            let initial = [("dir/sub/file.txt", "content")]
            let action = runEff $ runWorldMemory (initMemoryWorld initial) (readFileText "dir/sub")
            action `shouldThrow` (\case WorldIsADirectory "dir/sub" -> True; _ -> False)

        it "lists directory contents and calculates virtual subdirectories" $ do
            let initial =
                    [ ("app/Main.hs", "main")
                    , ("src/A.hs", "a")
                    , ("src/B.hs", "b")
                    , ("src/Sub/C.hs", "c")
                    ]
            (entries, _) <- runEff $ runWorldMemory (initMemoryWorld initial) $ do
                listDirectory "src"
            map deName entries `shouldBe` ["A.hs", "B.hs", "Sub"]
            map deIsDir entries `shouldBe` [False, False, True]

        it "searches virtual files by content pattern" $ do
            let initial =
                    [ ("a.txt", "Hello World\nSecond line\n")
                    , ("b.txt", "hello agent\nanother line\n")
                    , ("c.txt", "unrelated text\n")
                    ]
            (matches, _) <- runEff $ runWorldMemory (initMemoryWorld initial) $ do
                searchFiles (SearchOptions "hello" "" False True Nothing [] [])
            length matches `shouldBe` 2
            map smFile matches `shouldBe` ["a.txt", "b.txt"]

        it "finds files matching glob and type filters in memory" $ do
            let initial =
                    [ ("src/Lib.hs", "lib")
                    , ("src/Internal/Core.hs", "core")
                    , ("test/Spec.hs", "spec")
                    ]
            (found, _) <- runEff $ runWorldMemory (initMemoryWorld initial) $ do
                findFiles (FindOptions "src" (Just "Core") Nothing FindFilesOnly [])
            found `shouldBe` ["src/Internal/Core.hs"]

        it "simulates standard command execution and records command history" $ do
            let initial = [("greet.txt", "Hello from file")]
            (res, finalSt) <- runEff $ runWorldMemory (initMemoryWorld initial) $ do
                p1 <- runCommand (CommandSpec "echo" ["hello", "world"] Nothing Nothing Nothing Nothing)
                p2 <- runCommand (CommandSpec "cat" ["greet.txt"] Nothing Nothing Nothing Nothing)
                p3 <- runCommand (CommandSpec "pwd" [] Nothing Nothing Nothing Nothing)
                pure (p1, p2, p3)

            let (rEcho, rCat, rPwd) = res
            prStdout rEcho `shouldBe` "hello world\n"
            prStdout rCat `shouldBe` "Hello from file"
            prStdout rPwd `shouldBe` "/\n"
            length (mwsCommandHistory finalSt) `shouldBe` 3

        it "supports custom simulated command handlers in memory" $ do
            let customHandler cmdSpec = ProcessResult 0 ("custom: " <> cmdProgram cmdSpec) "" 5.0 False
            let st = (initMemoryWorld []){mwsCommandHandlers = Map.fromList [("custom-tool", customHandler)]}
            (res, _) <- runEff $ runWorldMemory st $ do
                runCommand (CommandSpec "custom-tool" ["--flag"] Nothing Nothing Nothing Nothing)
            prStdout res `shouldBe` "custom: custom-tool"

    describe "Local Workspace World Interpreter (runWorldLocal)" $ do
        it "performs real file I/O operations inside temporary directory" $ do
            withSystemTempDirectory "llmonad-local-test" $ \tmpDir -> do
                runEff $ runWorldLocal tmpDir $ do
                    writeFileText "hello.txt" "Hello World\nLine 2\nLine 3\n"
                    content <- readFileText "hello.txt"
                    slice <- readFileSlice "hello.txt" (Just 2) (Just 3)
                    existsBefore <- doesFileExist "hello.txt"
                    deleteFile "hello.txt"
                    existsAfter <- doesFileExist "hello.txt"
                    liftIO $ do
                        content `shouldBe` "Hello World\nLine 2\nLine 3\n"
                        slice `shouldBe` "Line 2\nLine 3\n"
                        existsBefore `shouldBe` True
                        existsAfter `shouldBe` False

        it "throws WorldFileNotFound for non-existent file on disk" $ do
            withSystemTempDirectory "llmonad-local-test" $ \tmpDir -> do
                let action = runEff $ runWorldLocal tmpDir (readFileText "does-not-exist.txt")
                action `shouldThrow` (\case WorldFileNotFound "does-not-exist.txt" -> True; _ -> False)

        it "creates parent directories automatically when writing nested files" $ do
            withSystemTempDirectory "llmonad-local-test" $ \tmpDir -> do
                runEff $ runWorldLocal tmpDir $ do
                    writeFileText "a/b/c/nested.txt" "nested content"
                    content <- readFileText "a/b/c/nested.txt"
                    liftIO $ content `shouldBe` "nested content"

        it "lists directory items with accurate metadata" $ do
            withSystemTempDirectory "llmonad-local-test" $ \tmpDir -> do
                runEff $ runWorldLocal tmpDir $ do
                    writeFileText "file1.txt" "alpha"
                    writeFileText "file2.txt" "beta gamma"
                    createDirectory "subdir" True
                    entries <- listDirectory "."
                    liftIO $ do
                        map deName entries `shouldBe` ["file1.txt", "file2.txt", "subdir"]
                        map deIsDir entries `shouldBe` [False, False, True]

        it "searches file contents matching plain text and case sensitivity" $ do
            withSystemTempDirectory "llmonad-local-test" $ \tmpDir -> do
                runEff $ runWorldLocal tmpDir $ do
                    writeFileText "a.txt" "alpha target beta\nno match\n"
                    writeFileText "b.txt" "another line\nTARGET line\n"
                    matches <- searchFiles (SearchOptions "target" "." False True Nothing [] [])
                    liftIO $ do
                        length matches `shouldBe` 2
                        map smLineNumber matches `shouldBe` [1, 2]

        it "finds files matching name pattern and max depth" $ do
            withSystemTempDirectory "llmonad-local-test" $ \tmpDir -> do
                runEff $ runWorldLocal tmpDir $ do
                    writeFileText "root.hs" "root"
                    writeFileText "dir1/sub.hs" "sub"
                    writeFileText "dir1/dir2/deep.hs" "deep"
                    allHs <- findFiles (FindOptions "." (Just ".hs") Nothing FindFilesOnly [])
                    shallowHs <- findFiles (FindOptions "." (Just ".hs") (Just 1) FindFilesOnly [])
                    liftIO $ do
                        sort allHs `shouldBe` ["dir1/dir2/deep.hs", "dir1/sub.hs", "root.hs"]
                        sort shallowHs `shouldBe` ["dir1/sub.hs", "root.hs"]

        it "executes processes with environment and cwd" $ do
            withSystemTempDirectory "llmonad-local-test" $ \tmpDir -> do
                runEff $ runWorldLocal tmpDir $ do
                    writeFileText "input.txt" "piped data"
                    res <- runCommand (CommandSpec "echo" ["hello", "world"] Nothing Nothing Nothing Nothing)
                    liftIO $ do
                        prExitCode res `shouldBe` 0
                        prStdout res `shouldBe` "hello world\n"
                        prTimedOut res `shouldBe` False

        it "terminates long-running processes when command timeout expires" $ do
            withSystemTempDirectory "llmonad-local-test" $ \tmpDir -> do
                runEff $ runWorldLocal tmpDir $ do
                    -- sleep 5 seconds with 100ms timeout
                    res <- runCommand (CommandSpec "sleep" ["5"] Nothing Nothing (Just 100) Nothing)
                    liftIO $ do
                        prTimedOut res `shouldBe` True
                        prExitCode res `shouldBe` (-1)

        it "supports simplified World smart constructors" $ do
            withSystemTempDirectory "llmonad-local-test" $ \tmpDir -> do
                runEff $ runWorldLocal tmpDir $ do
                    writeFileWorld "test.txt" "smart constructor test"
                    txt <- readFileWorld "test.txt"
                    dir <- getCurrentDirWorld
                    files <- listDirWorld "."
                    found <- findFilesWorld "." "test"
                    grepMatches <- grepFilesWorld "." "smart"
                    cmdRes <- runCommandWorld "echo" ["ok"] Nothing Nothing
                    deleteFileWorld "test.txt"
                    liftIO $ do
                        txt `shouldBe` "smart constructor test"
                        dir `shouldSatisfy` (not . null)
                        files `shouldBe` ["test.txt"]
                        found `shouldBe` ["test.txt"]
                        length grepMatches `shouldBe` 1
                        prExitCode cmdRes `shouldBe` 0
                        prStdout cmdRes `shouldBe` "ok\n"

    describe "Git Worktree Sandboxing Interpreter (runWorldWorktree)" $ do
        it "creates an isolated worktree, executes changes, collects diff, and cleans up" $ do
            withSystemTempDirectory "llmonad-git-repo" $ \repoDir -> do
                -- Initialize a git repo with an initial commit
                _ <- readProcessWithExitCode "git" ["-C", repoDir, "init"] ""
                _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.email", "test@llmonad.org"] ""
                _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.name", "LLMonad Test"] ""
                writeFile (repoDir </> "base.txt") "base version\n"
                _ <- readProcessWithExitCode "git" ["-C", repoDir, "add", "base.txt"] ""
                _ <- readProcessWithExitCode "git" ["-C", repoDir, "commit", "-m", "Initial commit"] ""

                let cfg = (defaultWorktreeConfig repoDir){wtBaseRef = "HEAD"}
                (resultVal, summary) <- runEff $ runWorldWorktree cfg $ do
                    -- Read base file inside sandbox
                    baseContent <- readFileText "base.txt"
                    -- Modify file inside sandbox
                    writeFileText "base.txt" "modified in worktree\n"
                    -- Add a new file inside sandbox
                    writeFileText "new_feature.txt" "new feature code\n"
                    pure baseContent

                resultVal `shouldBe` "base version\n"
                wsExitCode summary `shouldBe` 0
                wsDiff summary `shouldSatisfy` ("modified in worktree" `T.isInfixOf`)
                wsFilesChanged summary `shouldSatisfy` (\f -> "new_feature.txt" `elem` f || "base.txt" `elem` f)

                -- Verify base repository is unmodified
                baseInRepo <- readFile (repoDir </> "base.txt")
                baseInRepo `shouldBe` "base version\n"
                newFileInRepo <- SD.doesFileExist (repoDir </> "new_feature.txt")
                newFileInRepo `shouldBe` False

                -- Verify temporary worktree path has been completely cleaned up
                wtPathExists <- SD.doesDirectoryExist (wsPath summary)
                wtPathExists `shouldBe` False

        it "guarantees complete worktree cleanup even when computation throws an exception" $ do
            withSystemTempDirectory "llmonad-git-repo-fail" $ \repoDir -> do
                _ <- readProcessWithExitCode "git" ["-C", repoDir, "init"] ""
                _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.email", "test@llmonad.org"] ""
                _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.name", "LLMonad Test"] ""
                writeFile (repoDir </> "file.txt") "content\n"
                _ <- readProcessWithExitCode "git" ["-C", repoDir, "add", "file.txt"] ""
                _ <- readProcessWithExitCode "git" ["-C", repoDir, "commit", "-m", "Init"] ""

                let cfg = defaultWorktreeConfig repoDir
                let failingAction = runEff $ runWorldWorktree cfg $ do
                        writeFileText "file.txt" "corrupted"
                        EE.throwIO (WorldIOError "deliberate test failure")

                failingAction `shouldThrow` (\case WorldIOError "deliberate test failure" -> True; _ -> False)

                -- Verify git worktrees list in base repo is clean (only main worktree remaining)
                (_, wtList, _) <- readProcessWithExitCode "git" ["-C", repoDir, "worktree", "list", "--porcelain"] ""
                let count = length (filter ("worktree " `isPrefixOfText`) (T.lines (T.pack wtList)))
                count `shouldBe` 1

    describe "Adversarial & Boundary Verification" $ do
        describe "Memory virtual filesystem slice operations" $ do
            it "handles startLine <= 0 by clamping to line 1" $ do
                let st = initMemoryWorld [("file.txt", "line1\nline2\nline3\n")]
                (res0, _) <- runEff $ runWorldMemory st $ readFileSlice "file.txt" (Just 0) (Just 2)
                (resNeg, _) <- runEff $ runWorldMemory st $ readFileSlice "file.txt" (Just (-5)) (Just 2)
                res0 `shouldBe` "line1\nline2\n"
                resNeg `shouldBe` "line1\nline2\n"

            it "handles startLine > endLine by clamping end to start" $ do
                let st = initMemoryWorld [("file.txt", "line1\nline2\nline3\nline4\nline5\n")]
                (res, _) <- runEff $ runWorldMemory st $ readFileSlice "file.txt" (Just 4) (Just 2)
                res `shouldBe` "line4\n"

            it "returns empty text when startLine > total lines" $ do
                let st = initMemoryWorld [("file.txt", "line1\nline2\n")]
                (res, _) <- runEff $ runWorldMemory st $ readFileSlice "file.txt" (Just 10) (Just 20)
                res `shouldBe` ""

            it "handles endLine > total lines by reading until EOF" $ do
                let st = initMemoryWorld [("file.txt", "line1\nline2\nline3\n")]
                (res, _) <- runEff $ runWorldMemory st $ readFileSlice "file.txt" (Just 2) (Just 100)
                res `shouldBe` "line2\nline3\n"

            it "handles empty files" $ do
                let st = initMemoryWorld [("empty.txt", "")]
                (res, _) <- runEff $ runWorldMemory st $ readFileSlice "empty.txt" Nothing Nothing
                res `shouldBe` ""

            it "handles Nothing Nothing, Just Nothing, and Nothing Just" $ do
                let st = initMemoryWorld [("file.txt", "l1\nl2\nl3\nl4\n")]
                (rAll, _) <- runEff $ runWorldMemory st $ readFileSlice "file.txt" Nothing Nothing
                (rFrom2, _) <- runEff $ runWorldMemory st $ readFileSlice "file.txt" (Just 2) Nothing
                (rUntil2, _) <- runEff $ runWorldMemory st $ readFileSlice "file.txt" Nothing (Just 2)
                rAll `shouldBe` "l1\nl2\nl3\nl4\n"
                rFrom2 `shouldBe` "l2\nl3\nl4\n"
                rUntil2 `shouldBe` "l1\nl2\n"

        describe "Search/grep pattern matching with special characters" $ do
            it "handles special regex/glob characters as literal query in Memory" $ do
                let files =
                        [ ("config.json", "{\"key\": \"val\", \"count\": [1, 2, 3]}")
                        , ("math.txt", "x + y * z = 100 ($USD)")
                        , ("regex.txt", "pattern .* and [a-z]+ and \\d{3}")
                        , ("unicode.txt", "Haskell λ monad 🚀 中文测试")
                        ]
                let st = initMemoryWorld files
                (m1, _) <- runEff $ runWorldMemory st $ searchFiles (SearchOptions "[1, 2, 3]" "" False False Nothing [] [])
                (m2, _) <- runEff $ runWorldMemory st $ searchFiles (SearchOptions "($USD)" "" False False Nothing [] [])
                (m3, _) <- runEff $ runWorldMemory st $ searchFiles (SearchOptions ".*" "" False False Nothing [] [])
                (m4, _) <- runEff $ runWorldMemory st $ searchFiles (SearchOptions "λ monad 🚀" "" False False Nothing [] [])
                (m5, _) <- runEff $ runWorldMemory st $ searchFiles (SearchOptions "中文" "" False False Nothing [] [])

                map smFile m1 `shouldBe` ["config.json"]
                map smFile m2 `shouldBe` ["math.txt"]
                map smFile m3 `shouldBe` ["regex.txt"]
                map smFile m4 `shouldBe` ["unicode.txt"]
                map smFile m5 `shouldBe` ["unicode.txt"]

            it "respects maxMatches limit across multiple files in Memory" $ do
                let files = [("f1.txt", "hit\nhit\n"), ("f2.txt", "hit\nhit\n")]
                let st = initMemoryWorld files
                (matches, _) <- runEff $ runWorldMemory st $ searchFiles (SearchOptions "hit" "" False False (Just 3) [] [])
                length matches `shouldBe` 3

            it "respects includes and excludes filters in Memory" $ do
                let files =
                        [ ("src/Main.hs", "findme")
                        , ("src/Internal.hs", "findme")
                        , ("test/Spec.hs", "findme")
                        , ("doc/README.md", "findme")
                        ]
                let st = initMemoryWorld files
                (matches, _) <-
                    runEff
                        $ runWorldMemory st
                        $ searchFiles (SearchOptions "findme" "" False False Nothing [".hs"] ["Internal"])
                map smFile matches `shouldBe` ["src/Main.hs", "test/Spec.hs"]

        describe "File deletion and overwriting semantics" $ do
            it "overwrites existing files with shorter and empty content correctly" $ do
                let st = initMemoryWorld [("target.txt", "this is a very long original content\n")]
                (res, finalSt) <- runEff $ runWorldMemory st $ do
                    writeFileText "target.txt" "short"
                    c1 <- readFileText "target.txt"
                    writeFileText "target.txt" ""
                    c2 <- readFileText "target.txt"
                    pure (c1, c2)
                fst res `shouldBe` "short"
                snd res `shouldBe` ""
                Map.lookup "target.txt" (mwsFiles finalSt) `shouldBe` Just ""

            it "throws WorldIsADirectory when writing to a directory path in Memory" $ do
                let st = initMemoryWorld [("dir/file.txt", "content")]
                let action = runEff $ runWorldMemory st (writeFileText "dir" "new content")
                action `shouldThrow` (\case WorldIsADirectory "dir" -> True; _ -> False)

            it "throws WorldFileNotFound when deleting a non-existent file or directory in Memory" $ do
                let st = initMemoryWorld [("dir/file.txt", "content")]
                let action1 = runEff $ runWorldMemory st (deleteFile "missing.txt")
                let action2 = runEff $ runWorldMemory st (deleteFile "dir")
                action1 `shouldThrow` (\case WorldFileNotFound "missing.txt" -> True; _ -> False)
                action2 `shouldThrow` (\case WorldFileNotFound "dir" -> True; _ -> False)

            it "overwrites and deletes files correctly in Local workspace" $ do
                withSystemTempDirectory "llmonad-local-overwrite" $ \tmpDir -> do
                    runEff $ runWorldLocal tmpDir $ do
                        writeFileText "f.txt" "long initial text"
                        writeFileText "f.txt" "short"
                        c1 <- readFileText "f.txt"
                        deleteFile "f.txt"
                        fExists <- doesFileExist "f.txt"
                        liftIO $ do
                            c1 `shouldBe` "short"
                            fExists `shouldBe` False

        describe "Concurrency Behaviors" $ do
            it "handles concurrent reads and writes safely in Local workspace" $ do
                withSystemTempDirectory "llmonad-local-concurrent" $ \tmpDir -> do
                    let numThreads = 10
                    runEff $ runWorldLocal tmpDir $ do
                        -- Pre-populate files
                        forM_ [1 .. numThreads] $ \i ->
                            writeFileText (T.unpack ("worker_" <> T.pack (show (i :: Int)) <> ".txt")) ("initial_" <> T.pack (show (i :: Int)))

                    -- Concurrently read and update separate files
                    forConcurrently_ [1 .. numThreads] $ \i -> do
                        runEff $ runWorldLocal tmpDir $ do
                            let fn = T.unpack ("worker_" <> T.pack (show (i :: Int)) <> ".txt")
                            val <- readFileText fn
                            writeFileText fn (val <> "_updated")

                    -- Verify all updates succeeded
                    runEff $ runWorldLocal tmpDir $ do
                        forM_ [1 .. numThreads] $ \i -> do
                            let fn = T.unpack ("worker_" <> T.pack (show (i :: Int)) <> ".txt")
                            content <- readFileText fn
                            liftIO $ content `shouldBe` ("initial_" <> T.pack (show (i :: Int)) <> "_updated")

            it "handles multiple sequential worktrees in a Git repo" $ do
                withSystemTempDirectory "llmonad-git-seq" $ \repoDir -> do
                    _ <- readProcessWithExitCode "git" ["-C", repoDir, "init"] ""
                    _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.email", "test@llmonad.org"] ""
                    _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.name", "LLMonad Test"] ""
                    writeFile (repoDir </> "base.txt") "base\n"
                    _ <- readProcessWithExitCode "git" ["-C", repoDir, "add", "base.txt"] ""
                    _ <- readProcessWithExitCode "git" ["-C", repoDir, "commit", "-m", "Init"] ""

                    forM_ [1 .. 3] $ \i -> do
                        let cfg = (defaultWorktreeConfig repoDir){wtBranchPrefix = "seq-test-" <> T.pack (show (i :: Int))}
                        (res, summary) <- runEff $ runWorldWorktree cfg $ do
                            writeFileText "sandbox.txt" ("run " <> T.pack (show (i :: Int)))
                            readFileText "sandbox.txt"
                        res `shouldBe` ("run " <> T.pack (show (i :: Int)))
                        wsExitCode summary `shouldBe` 0

        describe "Workspace Root Containment & Process Supervision Guarantees" $ do
            it "throws WorldPathOutsideWorkspace on relative .. escape" $ do
                withSystemTempDirectory "llmonad-local-escape" $ \tmpDir -> do
                    let action = runEff $ runWorldLocal tmpDir (readFileText "../escape.txt")
                    action `shouldThrow` (\case WorldPathOutsideWorkspace "../escape.txt" _ -> True; _ -> False)

            it "throws WorldPathOutsideWorkspace on absolute path escape" $ do
                withSystemTempDirectory "llmonad-local-escape-abs" $ \tmpDir -> do
                    let action = runEff $ runWorldLocal tmpDir (readFileText "/etc/hosts")
                    action `shouldThrow` (\case WorldPathOutsideWorkspace "/etc/hosts" _ -> True; _ -> False)

            it "executes process with concurrent stdin writing and stdout reading without pipe deadlock" $ do
                withSystemTempDirectory "llmonad-local-supervision" $ \tmpDir -> do
                    let bigPayload = T.replicate 2000 "01234567890123456789012345678901234567890123456789\n" -- 100KB+
                    let specBi = CommandSpec "cat" [] Nothing Nothing (Just 5000) (Just bigPayload)
                    res <- runEff $ runWorldLocal tmpDir (runCommand specBi)
                    prExitCode res `shouldBe` 0
                    prTimedOut res `shouldBe` False
                    prStdout res `shouldBe` bigPayload

isPrefixOfText :: Text -> Text -> Bool
isPrefixOfText p t = p `T.isPrefixOf` t
