{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Comprehensive Empirical Challenger Suite for Batch 4:
Workspace Path Containment, Process Supervision, and Coding Tools Limits.
-}
module LLMonad.Batch4ChallengerSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful
import LLMonad.Tools.Coding
import LLMonad.World
import LLMonad.World.Local
import LLMonad.World.Memory
import System.Directory qualified as SD
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Hspec

spec :: Spec
spec = describe "Batch 4 Empirical Challenger Suite" $ do
    describe "1. Workspace Path Containment Stress Tests" $ do
        it "rejects single-level parent escape (../) in readFileText" $ do
            withSystemTempDirectory "b4-contain-test" $ \tmpDir -> do
                let action = runEff $ runWorldLocal tmpDir (readFileText "../outside.txt")
                action `shouldThrow` (\case WorldPathOutsideWorkspace "../outside.txt" _ -> True; _ -> False)

        it "rejects multi-level parent escapes (../../../../etc/passwd)" $ do
            withSystemTempDirectory "b4-contain-test" $ \tmpDir -> do
                let action = runEff $ runWorldLocal tmpDir (readFileText "../../../../../../etc/passwd")
                action `shouldThrow` (\case WorldPathOutsideWorkspace "../../../../../../etc/passwd" _ -> True; _ -> False)

        it "rejects absolute paths /etc/passwd and /tmp/secret" $ do
            withSystemTempDirectory "b4-contain-test" $ \tmpDir -> do
                let actPasswd = runEff $ runWorldLocal tmpDir (readFileText "/etc/passwd")
                let actSecret = runEff $ runWorldLocal tmpDir (readFileText "/tmp/secret")
                actPasswd `shouldThrow` (\case WorldPathOutsideWorkspace "/etc/passwd" _ -> True; _ -> False)
                actSecret `shouldThrow` (\case WorldPathOutsideWorkspace "/tmp/secret" _ -> True; _ -> False)

        it "rejects symlinks pointing to parent directory" $ do
            withSystemTempDirectory "b4-contain-parent" $ \tmpDir -> do
                -- tmpDir/link_parent -> ..
                _ <- readProcessWithExitCode "ln" ["-s", "..", tmpDir </> "link_parent"] ""
                let action = runEff $ runWorldLocal tmpDir (readFileText "link_parent/secret.txt")
                action `shouldThrow` (\case WorldPathOutsideWorkspace "link_parent/secret.txt" _ -> True; _ -> False)

        it "rejects symlinks pointing to /etc" $ do
            withSystemTempDirectory "b4-contain-etc" $ \tmpDir -> do
                _ <- readProcessWithExitCode "ln" ["-s", "/etc", tmpDir </> "link_etc"] ""
                let action = runEff $ runWorldLocal tmpDir (readFileText "link_etc/hosts")
                action `shouldThrow` (\case WorldPathOutsideWorkspace "link_etc/hosts" _ -> True; _ -> False)

        it "rejects nested symlinks (link1 -> link2 -> /etc/hosts)" $ do
            withSystemTempDirectory "b4-contain-nested-sym" $ \tmpDir -> do
                withSystemTempDirectory "b4-outside-target" $ \outsideDir -> do
                    writeFile (outsideDir </> "secret.txt") "secret data"
                    _ <- readProcessWithExitCode "ln" ["-s", outsideDir </> "secret.txt", tmpDir </> "sym_step2"] ""
                    _ <- readProcessWithExitCode "ln" ["-s", tmpDir </> "sym_step2", tmpDir </> "sym_step1"] ""
                    let action = runEff $ runWorldLocal tmpDir (readFileText "sym_step1")
                    action `shouldThrow` (\case WorldPathOutsideWorkspace "sym_step1" _ -> True; _ -> False)

        it "rejects symlinks inside subdirectories pointing to escaping paths" $ do
            withSystemTempDirectory "b4-contain-subdir-sym" $ \tmpDir -> do
                withSystemTempDirectory "b4-outside-target" $ \outsideDir -> do
                    writeFile (outsideDir </> "file.txt") "target data"
                    SD.createDirectoryIfMissing True (tmpDir </> "sub1" </> "sub2")
                    _ <- readProcessWithExitCode "ln" ["-s", outsideDir, tmpDir </> "sub1" </> "sub2" </> "esc_link"] ""
                    let action = runEff $ runWorldLocal tmpDir (readFileText "sub1/sub2/esc_link/file.txt")
                    action `shouldThrow` (\case WorldPathOutsideWorkspace "sub1/sub2/esc_link/file.txt" _ -> True; _ -> False)

        it "rejects non-existent target files in escaping directories" $ do
            withSystemTempDirectory "b4-contain-nonexist-esc" $ \tmpDir -> do
                _ <- readProcessWithExitCode "ln" ["-s", "/tmp", tmpDir </> "sym_tmp"] ""
                let action1 = runEff $ runWorldLocal tmpDir (readFileText "sym_tmp/nonexistent_file_12345.txt")
                let action2 = runEff $ runWorldLocal tmpDir (readFileText "../nonexistent_dir/file.txt")
                action1 `shouldThrow` (\case WorldPathOutsideWorkspace "sym_tmp/nonexistent_file_12345.txt" _ -> True; _ -> False)
                action2 `shouldThrow` (\case WorldPathOutsideWorkspace "../nonexistent_dir/file.txt" _ -> True; _ -> False)

        it "rejects write, delete, and directory operations attempting traversal escapes" $ do
            withSystemTempDirectory "b4-contain-ops" $ \tmpDir -> do
                let actWrite = runEff $ runWorldLocal tmpDir (writeFileText "../evil.txt" "evil")
                let actDelete = runEff $ runWorldLocal tmpDir (deleteFile "../evil.txt")
                let actCreateDir = runEff $ runWorldLocal tmpDir (createDirectory "../evil_dir" True)
                let actListDir = runEff $ runWorldLocal tmpDir (listDirectory "../")
                actWrite `shouldThrow` (\case WorldPathOutsideWorkspace "../evil.txt" _ -> True; _ -> False)
                actDelete `shouldThrow` (\case WorldPathOutsideWorkspace "../evil.txt" _ -> True; _ -> False)
                actCreateDir `shouldThrow` (\case WorldPathOutsideWorkspace "../evil_dir" _ -> True; _ -> False)
                actListDir `shouldThrow` (\case WorldPathOutsideWorkspace "../" _ -> True; _ -> False)

        it "rejects searchFiles and findFiles targeting escaping paths" $ do
            withSystemTempDirectory "b4-contain-search" $ \tmpDir -> do
                let actSearch = runEff $ runWorldLocal tmpDir (searchFiles (SearchOptions "query" "../" False False Nothing [] []))
                let actFind = runEff $ runWorldLocal tmpDir (findFiles (FindOptions "/etc" Nothing Nothing FindAny []))
                actSearch `shouldThrow` (\case WorldPathOutsideWorkspace "../" _ -> True; _ -> False)
                actFind `shouldThrow` (\case WorldPathOutsideWorkspace "/etc" _ -> True; _ -> False)

        it "rejects probing operations (doesPathExist, doesFileExist, doesDirectoryExist) escaping workspace" $ do
            withSystemTempDirectory "b4-contain-probe" $ \tmpDir -> do
                let actPath = runEff $ runWorldLocal tmpDir (doesPathExist "/etc")
                let actFile = runEff $ runWorldLocal tmpDir (doesFileExist "../foo.txt")
                let actDir = runEff $ runWorldLocal tmpDir (doesDirectoryExist "/var")
                actPath `shouldThrow` (\case WorldPathOutsideWorkspace "/etc" _ -> True; _ -> False)
                actFile `shouldThrow` (\case WorldPathOutsideWorkspace "../foo.txt" _ -> True; _ -> False)
                actDir `shouldThrow` (\case WorldPathOutsideWorkspace "/var" _ -> True; _ -> False)

        it "rejects runCommand with escaping cwd" $ do
            withSystemTempDirectory "b4-contain-cmd" $ \tmpDir -> do
                let specEsc = CommandSpec "echo" ["hello"] (Just "../") Nothing (Just 2000) Nothing
                let action = runEff $ runWorldLocal tmpDir (runCommand specEsc)
                action `shouldThrow` (\case WorldPathOutsideWorkspace "../" _ -> True; _ -> False)

        it "permits valid internal paths with redundant dots and valid internal symlinks" $ do
            withSystemTempDirectory "b4-contain-valid" $ \tmpDir -> do
                runEff $ runWorldLocal tmpDir $ do
                    createDirectory "sub1/sub2" True
                    writeFileText "sub1/sub2/file.txt" "internal file content"

                -- Internal symlink
                _ <- readProcessWithExitCode "ln" ["-s", tmpDir </> "sub1" </> "sub2", tmpDir </> "sub1_link"] ""

                runEff $ runWorldLocal tmpDir $ do
                    -- Redundant dots within boundary
                    c1 <- readFileText "./sub1/../sub1/sub2/./file.txt"
                    c2 <- readFileText "sub1_link/file.txt"
                    liftIO $ do
                        c1 `shouldBe` "internal file content"
                        c2 `shouldBe` "internal file content"

    describe "2. Coding Tool Line Truncation & Byte Limit Stress Tests" $ do
        it "accurately aligns vfrStartLine and vfrEndLine with returned slice on large multi-line file" $ do
            -- 1000 lines of 100 chars = ~100KB > 46,080 byte limit
            let lineTemplate i = "line_" <> T.pack (show (i :: Int)) <> "_" <> T.replicate 90 "x"
            let content = T.unlines [lineTemplate i | i <- [1 .. 1000]]
            let st = initMemoryWorld [("stress_large.txt", content)]

            (res, _) <- runEff $ runWorldMemory st $ do
                runViewFile (ViewFileArgs "stress_large.txt" (Just 1) (Just 1000) Nothing)

            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right vfr -> do
                    vfrIsTruncated vfr `shouldBe` True
                    let linesList = vfrLines vfr
                    length linesList `shouldSatisfy` (> 0)
                    length linesList `shouldSatisfy` (< 1000)

                    case (linesList, reverse linesList) of
                        (firstLine : _, lastLine : _) -> do
                            vfrStartLine vfr `shouldBe` lineIndex firstLine
                            vfrEndLine vfr `shouldBe` lineIndex lastLine
                            vfrStartLine vfr `shouldBe` 1
                            vfrEndLine vfr `shouldBe` length linesList
                            map lineIndex linesList `shouldBe` [vfrStartLine vfr .. vfrEndLine vfr]
                        _ -> expectationFailure "Expected non-empty linesList"

        it "accurately aligns vfrStartLine and vfrEndLine with startLine > 1 and contentOffset" $ do
            let lineTemplate i = "line_" <> T.pack (show (i :: Int)) <> "_" <> T.replicate 100 "y"
            let content = T.unlines [lineTemplate i | i <- [1 .. 800]]
            let st = initMemoryWorld [("stress_offset.txt", content)]

            (res, _) <- runEff $ runWorldMemory st $ do
                runViewFile (ViewFileArgs "stress_offset.txt" (Just 50) (Just 500) (Just 20))

            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right vfr -> do
                    vfrIsTruncated vfr `shouldBe` True
                    let linesList = vfrLines vfr
                    length linesList `shouldSatisfy` (> 0)

                    case (linesList, reverse linesList) of
                        (firstLine : _, lastLine : _) -> do
                            -- startLine=50, contentOffset=20 -> actual start line = 70
                            lineIndex firstLine `shouldBe` 70
                            vfrStartLine vfr `shouldBe` 70
                            vfrEndLine vfr `shouldBe` lineIndex lastLine
                            map lineIndex linesList `shouldBe` [vfrStartLine vfr .. vfrEndLine vfr]
                        _ -> expectationFailure "Expected non-empty linesList"

        it "handles single huge line exceeding maxBytes" $ do
            let hugeLine = T.replicate 60000 "Z" -- 60,000 chars > 46,080 bytes
            let st = initMemoryWorld [("single_huge.txt", hugeLine)]

            (res, _) <- runEff $ runWorldMemory st $ do
                runViewFile (ViewFileArgs "single_huge.txt" (Just 1) (Just 1) Nothing)

            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right vfr -> do
                    -- First line is always included even if single line exceeds maxBytes
                    length (vfrLines vfr) `shouldBe` 1
                    vfrStartLine vfr `shouldBe` 1
                    vfrEndLine vfr `shouldBe` 1
                    vfrIsTruncated vfr `shouldBe` False

        it "handles offset beyond total lines gracefully" $ do
            let content = "line 1\nline 2\nline 3\n"
            let st = initMemoryWorld [("short.txt", content)]

            (res, _) <- runEff $ runWorldMemory st $ do
                runViewFile (ViewFileArgs "short.txt" (Just 1) (Just 3) (Just 10))

            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right vfr -> do
                    vfrLines vfr `shouldBe` []
                    vfrStartLine vfr `shouldBe` 1
                    vfrEndLine vfr `shouldBe` 1
                    vfrIsTruncated vfr `shouldBe` False

        it "executes editFile with line bounds and replacement validation" $ do
            let orig = "alpha\nbeta\nbeta\ngamma\n"
            let st = initMemoryWorld [("edit_test.txt", orig)]

            (res, finalSt) <- runEff $ runWorldMemory st $ do
                runEditFile (EditFileArgs "edit_test.txt" Nothing "beta" "delta" (Just 2) (Just 2) Nothing)

            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right efr -> do
                    efrReplacedCount efr `shouldBe` 1
                    efrLinesModified efr `shouldBe` [2]
                    Map.lookup "edit_test.txt" (mwsFiles finalSt) `shouldBe` Just "alpha\ndelta\nbeta\ngamma\n"

    describe "3. Process Supervision & Concurrency Stress Tests" $ do
        it "handles large bidirectional streams (200KB stdin / 200KB stdout) without deadlock" $ do
            withSystemTempDirectory "b4-supervision-bi" $ \tmpDir -> do
                let line = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\n" -- 65 bytes
                let payload = T.replicate 3500 line -- ~227 KB
                let specCat = CommandSpec "cat" [] Nothing Nothing (Just 10000) (Just payload)

                res <- runEff $ runWorldLocal tmpDir (runCommand specCat)
                prExitCode res `shouldBe` 0
                prTimedOut res `shouldBe` False
                prStdout res `shouldBe` payload

        it "terminates multi-process subprocess pipeline on timeout and reaps child" $ do
            withSystemTempDirectory "b4-supervision-pipe-kill" $ \tmpDir -> do
                -- Start a command with sleep in subshell
                let specSleep = CommandSpec "/bin/sh" ["-c", "sleep 30"] Nothing Nothing (Just 150) Nothing
                res <- runEff $ runWorldLocal tmpDir (runCommand specSleep)
                prTimedOut res `shouldBe` True
                prExitCode res `shouldBe` (-1)
                prStderr res `shouldBe` "Process timed out"

        it "applies default bounded timeout of 30,000ms in runRunCommand" $ do
            let st = initMemoryWorld []
            (res, finalSt) <- runEff $ runWorldMemory st $ do
                runRunCommand (RunCommandArgs "echo ok" Nothing Nothing Nothing Nothing)
            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right (CommandCompleted code stdout _ _) -> do
                    code `shouldBe` 0
                    stdout `shouldBe` "ok\n"
                    case mwsCommandHistory finalSt of
                        (cmd : _) -> cmdTimeoutMs cmd `shouldBe` Just 30000
                        [] -> expectationFailure "Expected command history"
                Right other -> expectationFailure ("Unexpected result: " <> show other)
