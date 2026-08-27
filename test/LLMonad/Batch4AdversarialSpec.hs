{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Adversarial verification suite for Batch 4:
Workspace Containment, Process Supervision Deadlocks & Coding Tool Limits.
-}
module LLMonad.Batch4AdversarialSpec (spec) where

import Data.Text qualified as T
import Effectful
import LLMonad.Tools.Coding
import LLMonad.World
import LLMonad.World.Local
import LLMonad.World.Memory
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Hspec

spec :: Spec
spec = describe "Batch 4 Adversarial & Security Suite" $ do
    describe "1. Path Traversal & Workspace Containment Security" $ do
        it "rejects relative path traversal escapes (..) in readFileText" $ do
            withSystemTempDirectory "llmonad-b4-traversal" $ \tmpDir -> do
                let action = runEff $ runWorldLocal tmpDir $ do
                        readFileText "../outside.txt"
                action `shouldThrow` (\case WorldPathOutsideWorkspace "../outside.txt" _ -> True; _ -> False)

        it "rejects deep relative traversal escapes (../../../../etc/passwd)" $ do
            withSystemTempDirectory "llmonad-b4-traversal" $ \tmpDir -> do
                let action = runEff $ runWorldLocal tmpDir $ do
                        readFileText "../../../../etc/passwd"
                action `shouldThrow` (\case WorldPathOutsideWorkspace "../../../../etc/passwd" _ -> True; _ -> False)

        it "rejects absolute path escapes outside workspace root in readFileText" $ do
            withSystemTempDirectory "llmonad-b4-traversal" $ \tmpDir -> do
                let action = runEff $ runWorldLocal tmpDir $ do
                        readFileText "/etc/passwd"
                action `shouldThrow` (\case WorldPathOutsideWorkspace "/etc/passwd" _ -> True; _ -> False)

        it "rejects writing files outside workspace root" $ do
            withSystemTempDirectory "llmonad-b4-traversal" $ \tmpDir -> do
                let action = runEff $ runWorldLocal tmpDir $ do
                        writeFileText "../escape.txt" "malicious payload"
                action `shouldThrow` (\case WorldPathOutsideWorkspace "../escape.txt" _ -> True; _ -> False)

        it "rejects deleting files outside workspace root" $ do
            withSystemTempDirectory "llmonad-b4-traversal" $ \tmpDir -> do
                let action = runEff $ runWorldLocal tmpDir $ do
                        deleteFile "../target.txt"
                action `shouldThrow` (\case WorldPathOutsideWorkspace "../target.txt" _ -> True; _ -> False)

        it "rejects creating directories outside workspace root" $ do
            withSystemTempDirectory "llmonad-b4-traversal" $ \tmpDir -> do
                let action = runEff $ runWorldLocal tmpDir $ do
                        createDirectory "../outside_dir" True
                action `shouldThrow` (\case WorldPathOutsideWorkspace "../outside_dir" _ -> True; _ -> False)

        it "rejects listing directories outside workspace root" $ do
            withSystemTempDirectory "llmonad-b4-traversal" $ \tmpDir -> do
                let action = runEff $ runWorldLocal tmpDir $ do
                        listDirectory "../"
                action `shouldThrow` (\case WorldPathOutsideWorkspace "../" _ -> True; _ -> False)

        it "rejects searchFiles on directories outside workspace root" $ do
            withSystemTempDirectory "llmonad-b4-traversal" $ \tmpDir -> do
                let action = runEff $ runWorldLocal tmpDir $ do
                        searchFiles (SearchOptions "query" "../" False False Nothing [] [])
                action `shouldThrow` (\case WorldPathOutsideWorkspace "../" _ -> True; _ -> False)

        it "rejects findFiles on directories outside workspace root" $ do
            withSystemTempDirectory "llmonad-b4-traversal" $ \tmpDir -> do
                let action = runEff $ runWorldLocal tmpDir $ do
                        findFiles (FindOptions "../" Nothing Nothing FindAny [])
                action `shouldThrow` (\case WorldPathOutsideWorkspace "../" _ -> True; _ -> False)

        it "rejects doesPathExist, doesFileExist, doesDirectoryExist probing outside workspace root" $ do
            withSystemTempDirectory "llmonad-b4-traversal" $ \tmpDir -> do
                let action1 = runEff $ runWorldLocal tmpDir (doesPathExist "/etc/passwd")
                let action2 = runEff $ runWorldLocal tmpDir (doesFileExist "../secret.txt")
                let action3 = runEff $ runWorldLocal tmpDir (doesDirectoryExist "../")
                action1 `shouldThrow` (\case WorldPathOutsideWorkspace "/etc/passwd" _ -> True; _ -> False)
                action2 `shouldThrow` (\case WorldPathOutsideWorkspace "../secret.txt" _ -> True; _ -> False)
                action3 `shouldThrow` (\case WorldPathOutsideWorkspace "../" _ -> True; _ -> False)

        it "rejects symlinks that resolve outside the workspace root" $ do
            withSystemTempDirectory "llmonad-b4-symlink-root" $ \tmpDir -> do
                withSystemTempDirectory "llmonad-b4-outside-target" $ \outsideDir -> do
                    writeFile (outsideDir </> "outside_secret.txt") "secret outside workspace"
                    -- Create a symlink inside tmpDir pointing to outsideDir
                    _ <- readProcessWithExitCode "ln" ["-s", outsideDir, tmpDir </> "evil_link"] ""

                    let action = runEff $ runWorldLocal tmpDir $ do
                            readFileText "evil_link/outside_secret.txt"
                    action `shouldThrow` (\case WorldPathOutsideWorkspace "evil_link/outside_secret.txt" _ -> True; _ -> False)

        it "allows symlinks that resolve within the workspace root" $ do
            withSystemTempDirectory "llmonad-b4-symlink-valid" $ \tmpDir -> do
                runEff $ runWorldLocal tmpDir $ do
                    createDirectory "real_dir" True
                    writeFileText "real_dir/data.txt" "internal data"

                -- Create a valid symlink inside workspace pointing to real_dir inside workspace
                _ <- readProcessWithExitCode "ln" ["-s", tmpDir </> "real_dir", tmpDir </> "link_dir"] ""

                res <- runEff $ runWorldLocal tmpDir $ do
                    readFileText "link_dir/data.txt"
                res `shouldBe` "internal data"

        it "rejects readFileSlice on paths outside workspace root" $ do
            withSystemTempDirectory "llmonad-b4-traversal" $ \tmpDir -> do
                let action = runEff $ runWorldLocal tmpDir $ do
                        readFileSlice "../outside.txt" (Just 1) (Just 5)
                action `shouldThrow` (\case WorldPathOutsideWorkspace "../outside.txt" _ -> True; _ -> False)

        it "safely skips symlinks resolving outside workspace during searchFiles and findFiles" $ do
            withSystemTempDirectory "llmonad-b4-search-symlink" $ \tmpDir -> do
                withSystemTempDirectory "llmonad-b4-outside" $ \outsideDir -> do
                    writeFile (outsideDir </> "secret_code.txt") "secret content to avoid"
                    writeFile (tmpDir </> "valid.txt") "target query match inside"
                    _ <- readProcessWithExitCode "ln" ["-s", outsideDir, tmpDir </> "bad_link"] ""

                    matches <- runEff $ runWorldLocal tmpDir $ do
                        searchFiles (SearchOptions "target" "." False False Nothing [] [])
                    map smFile matches `shouldBe` ["valid.txt"]

                    found <- runEff $ runWorldLocal tmpDir $ do
                        findFiles (FindOptions "." Nothing Nothing FindFilesOnly [])
                    found `shouldBe` ["valid.txt"]

        it "handles child process exiting immediately while stdin is written without broken pipe panic" $ do
            withSystemTempDirectory "llmonad-b4-broken-pipe" $ \tmpDir -> do
                let hugeData = T.replicate 20000 "data line for early exit\n"
                let specEarly = CommandSpec "true" [] Nothing Nothing (Just 3000) (Just hugeData)
                res <- runEff $ runWorldLocal tmpDir (runCommand specEarly)
                prExitCode res `shouldBe` 0
                prTimedOut res `shouldBe` False

        it "respects explicit timeoutMs override in runRunCommand" $ do
            let st = initMemoryWorld []
            (res, finalSt) <- runEff $ runWorldMemory st $ do
                runRunCommand (RunCommandArgs "echo fast" Nothing (Just 1500))
            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right (CommandCompleted code _ _ _) -> do
                    code `shouldBe` 0
                    case mwsCommandHistory finalSt of
                        (cmd : _) -> cmdTimeoutMs cmd `shouldBe` Just 1500
                        [] -> expectationFailure "Expected command history"
                Right other -> expectationFailure ("Unexpected result: " <> show other)

        it "calculates accurate vfrStartLine and vfrEndLine with contentOffset on truncated files" $ do
            let lineTextContent = T.replicate 100 "B"
            let linesList = T.unlines (replicate 600 lineTextContent)
            let st = initMemoryWorld [("offset_large.txt", linesList)]
            (res, _) <- runEff $ runWorldMemory st $ do
                runViewFile (ViewFileArgs "offset_large.txt" (Just 10) (Just 600) (Just 5))
            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right vfr -> do
                    vfrIsTruncated vfr `shouldBe` True
                    vfrStartLine vfr `shouldBe` 15 -- start (10) + offset (5)
                    let lastLine = last (vfrLines vfr)
                    vfrEndLine vfr `shouldBe` lineIndex lastLine

    describe "2. Process Supervision, Pipe Deadlock Prevention & Process Reaping" $ do
        it "handles large stdin payloads (>100KB) concurrently without OS pipe deadlock" $ do
            withSystemTempDirectory "llmonad-b4-pipe" $ \tmpDir -> do
                -- Generate 150KB of stdin data
                let largeChunk = T.replicate 1500 "0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789" -- 150,000 chars
                let specIn = CommandSpec "cat" [] Nothing Nothing (Just 5000) (Just largeChunk)
                res <- runEff $ runWorldLocal tmpDir (runCommand specIn)
                prExitCode res `shouldBe` 0
                prTimedOut res `shouldBe` False
                T.length (prStdout res) `shouldBe` T.length largeChunk

        it "handles concurrent large stdin and stdout without deadlocking" $ do
            withSystemTempDirectory "llmonad-b4-pipe-bi" $ \tmpDir -> do
                let payload = T.unlines ["line " <> T.pack (show i) | i <- [1 .. 5000 :: Int]]
                let specBi = CommandSpec "cat" [] Nothing Nothing (Just 5000) (Just payload)
                res <- runEff $ runWorldLocal tmpDir (runCommand specBi)
                prExitCode res `shouldBe` 0
                prTimedOut res `shouldBe` False
                prStdout res `shouldBe` payload

        it "terminates process group on timeout and reaps child process" $ do
            withSystemTempDirectory "llmonad-b4-timeout-reap" $ \tmpDir -> do
                let specSleep = CommandSpec "sleep" ["30"] Nothing Nothing (Just 100) Nothing
                res <- runEff $ runWorldLocal tmpDir (runCommand specSleep)
                prTimedOut res `shouldBe` True
                prExitCode res `shouldBe` (-1)
                prStderr res `shouldBe` "Process timed out"

    describe "3. Coding Tools Capability Limits & Bounded Execution" $ do
        it "applies default 30,000ms bounded timeout in runRunCommand when timeoutMs is Nothing" $ do
            let st = initMemoryWorld []
            (res, finalSt) <- runEff $ runWorldMemory st $ do
                runRunCommand (RunCommandArgs "echo bounded" Nothing Nothing)
            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right (CommandCompleted code stdout _ _) -> do
                    code `shouldBe` 0
                    stdout `shouldBe` "bounded\n"
                    -- Verify CommandSpec recorded in history had cmdTimeoutMs = Just 30000
                    case mwsCommandHistory finalSt of
                        (cmd : _) -> cmdTimeoutMs cmd `shouldBe` Just 30000
                        [] -> expectationFailure "Expected recorded command in history"
                Right other -> expectationFailure ("Expected CommandCompleted, got: " <> show other)

        it "accurately calculates vfrEndLine from truncated lines in runViewFile when byte limit is hit" $ do
            let lineTextContent = T.replicate 100 "A"
            let manyLines = T.unlines (replicate 600 lineTextContent) -- ~60KB > 46,080 byte limit
            let st = initMemoryWorld [("large.txt", manyLines)]
            (res, _) <- runEff $ runWorldMemory st $ do
                runViewFile (ViewFileArgs "large.txt" (Just 1) (Just 600) Nothing)
            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right vfr -> do
                    vfrIsTruncated vfr `shouldBe` True
                    let count = length (vfrLines vfr)
                    count `shouldSatisfy` (< 600)
                    count `shouldSatisfy` (> 0)
                    vfrStartLine vfr `shouldBe` 1
                    -- vfrEndLine MUST match the actual last line index returned in vfrLines, not 600
                    vfrEndLine vfr `shouldBe` count
                    vfrEndLine vfr `shouldBe` lineIndex (last (vfrLines vfr))

        it "lists directory recursively when recursive = Just True in runListDir" $ do
            let files =
                    [ ("project/README.md", "# Project")
                    , ("project/src/Main.hs", "main = pure ()")
                    , ("project/src/Internal/Core.hs", "core = 1")
                    , ("project/test/Spec.hs", "spec = pure ()")
                    ]
            let st = initMemoryWorld files
            (res, _) <- runEff $ runWorldMemory st $ do
                runListDir (ListDirArgs "project" (Just True) Nothing)
            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right ldr -> do
                    let names = map deiName (ldrEntries ldr)
                    names `shouldBe` ["README.md", "src", "src/Internal", "src/Internal/Core.hs", "src/Main.hs", "test", "test/Spec.hs"]

        it "respects maxDepth in runListDir when recursing" $ do
            let files =
                    [ ("root/a.txt", "a")
                    , ("root/d1/b.txt", "b")
                    , ("root/d1/d2/c.txt", "c")
                    , ("root/d1/d2/d3/d.txt", "d")
                    ]
            let st = initMemoryWorld files
            (res, _) <- runEff $ runWorldMemory st $ do
                runListDir (ListDirArgs "root" (Just True) (Just 2))
            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right ldr -> do
                    let names = map deiName (ldrEntries ldr)
                    names `shouldBe` ["a.txt", "d1", "d1/b.txt", "d1/d2"]
