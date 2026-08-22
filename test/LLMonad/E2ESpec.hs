{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Comprehensive End-to-End Test Suite across Tiers 1-5 for LLMonad Coding Agent Runtime & TUI.
module LLMonad.E2ESpec (spec) where

import Brick.Focus (focusGetCurrent)
import Brick.Widgets.Edit (getEditContents)
import Control.Concurrent.Async (forConcurrently_, wait)
import Control.Monad (forM_)
import Data.Aeson (object, toJSON, (.=))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Effectful
import qualified Effectful.Exception as EE
import qualified Graphics.Vty as Vty
import LLMonad
import qualified LLMonad.Types as CoreTypes
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Hspec

spec :: Spec
spec = describe "LLMonad E2E Master Test Suite (Milestone 5)" $ do

  ---------------------------------------------------------------------------
  -- Tier 1: Feature Coverage
  ---------------------------------------------------------------------------
  describe "Tier 1: Feature Coverage" $ do

    describe "1. World Effect Primitives & Interpreters" $ do
      it "Local Interpreter: writes, reads, slices, lists, and deletes files" $ do
        withSystemTempDirectory "e2e-world-local" $ \tmpDir -> do
          runEff $ runWorldLocal tmpDir $ do
            writeFileText "greeting.txt" "Hello LLMonad\nLine 2\nLine 3\n"
            content <- readFileText "greeting.txt"
            slice <- readFileSlice "greeting.txt" (Just 2) (Just 3)
            createDirectory "subdir" True
            writeFileText "subdir/nested.txt" "nested content"
            entries <- listDirectory "."
            fExists <- doesFileExist "greeting.txt"
            deleteFile "greeting.txt"
            fExistsAfter <- doesFileExist "greeting.txt"
            liftIO $ do
              content `shouldBe` "Hello LLMonad\nLine 2\nLine 3\n"
              slice `shouldBe` "Line 2\nLine 3\n"
              fExists `shouldBe` True
              fExistsAfter `shouldBe` False
              map deName entries `shouldBe` ["greeting.txt", "subdir"]

      it "Worktree Interpreter: executes sandboxed git changes and collects diff" $ do
        withSystemTempDirectory "e2e-world-worktree" $ \repoDir -> do
          _ <- readProcessWithExitCode "git" ["-C", repoDir, "init"] ""
          _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.email", "e2e@llmonad.org"] ""
          _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.name", "E2E Test"] ""
          writeFile (repoDir </> "code.hs") "val = 1\n"
          _ <- readProcessWithExitCode "git" ["-C", repoDir, "add", "code.hs"] ""
          _ <- readProcessWithExitCode "git" ["-C", repoDir, "commit", "-m", "Init"] ""

          let cfg = defaultWorktreeConfig repoDir
          (val, summary) <- runEff $ runWorldWorktree cfg $ do
            c0 <- readFileText "code.hs"
            writeFileText "code.hs" "val = 2\n"
            pure c0

          val `shouldBe` "val = 1\n"
          wsExitCode summary `shouldBe` 0
          wsDiff summary `shouldSatisfy` ("+val = 2" `T.isInfixOf`)

      it "Memory Interpreter: performs pure virtual filesystem and command simulation" $ do
        let initFiles = [("src/Lib.hs", "module Lib where\n"), ("doc.txt", "documentation\n")]
        let worldSt = initMemoryWorld initFiles
        (res, finalSt) <- runEff $ runWorldMemory worldSt $ do
          c <- readFileText "src/Lib.hs"
          writeFileText "src/New.hs" "module New where\n"
          p1 <- runCommand (CommandSpec "echo" ["pure", "world"] Nothing Nothing Nothing Nothing)
          d <- doesFileExist "doc.txt"
          pure (c, prStdout p1, d)

        let (c, out, d) = res
        c `shouldBe` "module Lib where\n"
        out `shouldBe` "pure world\n"
        d `shouldBe` True
        Map.member "src/New.hs" (mwsFiles finalSt) `shouldBe` True

      it "Process Execution: captures exit codes, stdout, and stderr" $ do
        withSystemTempDirectory "e2e-world-proc" $ \tmpDir -> do
          runEff $ runWorldLocal tmpDir $ do
            pOk <- execShell "echo standard output"
            pErr <- execShell "echo error output >&2; exit 7"
            liftIO $ do
              prExitCode pOk `shouldBe` 0
              prStdout pOk `shouldBe` "standard output\n"
              prExitCode pErr `shouldBe` 7
              prStderr pErr `shouldBe` "error output\n"

      it "World Smart Constructors: simplifies file and directory actions" $ do
        withSystemTempDirectory "e2e-world-smart" $ \tmpDir -> do
          runEff $ runWorldLocal tmpDir $ do
            writeFileWorld "test.txt" "smart text\n"
            r <- readFileWorld "test.txt"
            d <- getCurrentDirWorld
            f <- findFilesWorld "." "test"
            g <- grepFilesWorld "." "smart"
            c <- runCommandWorld "echo" ["pass"] Nothing Nothing
            deleteFileWorld "test.txt"
            liftIO $ do
              r `shouldBe` "smart text\n"
              d `shouldSatisfy` (not . null)
              f `shouldBe` ["test.txt"]
              length g `shouldBe` 1
              prStdout c `shouldBe` "pass\n"

    describe "2. Journal Effect & Persistence" $ do
      it "In-Memory Journal: records and retrieves structured lifecycle events" $ do
        let program = do
              recordTurnStart "turn-1"
              recordUserMsg "Inspect project"
              recordToolCall "list_dir" (object ["dir" .= ("." :: Text)])
              recordToolResult "list_dir" (Right (object ["files" .= (["a.txt"] :: [Text])]))
              recordModelTurn "I inspected the project."
              recordMetrics (ModelMetrics 100 20 120 45.0 "mock-model")
              recordTurnFinish "turn-1"
              getEvents

        (evs, _) <- runEff (runJournalMemory program)
        length evs `shouldBe` 7
        take 1 evs `shouldBe` [TurnStarted "turn-1"]
        drop 6 evs `shouldBe` [TurnFinished "turn-1"]

      it "File-Based JSONL Journal: persists and appends records on disk" $ do
        withSystemTempDirectory "e2e-journal-file" $ \tmpDir -> do
          let jPath = tmpDir </> "session.jsonl"
          runEff $ runJournalFile jPath $ do
            recordTurnStart "t1"
            recordUserMsg "First message"
            recordTurnFinish "t1"

          runEff $ runJournalFile jPath $ do
            recordTurnStart "t2"
            recordUserMsg "Second message"
            recordTurnFinish "t2"

          raw <- TIO.readFile jPath
          let ls = filter (not . T.null) (T.lines raw)
          length ls `shouldBe` 6

      it "Session Resume: reconstructs full ChatMessage history from persisted JSONL" $ do
        withSystemTempDirectory "e2e-journal-resume" $ \tmpDir -> do
          let jPath = tmpDir </> "resume.jsonl"
          runEff $ runJournalFile jPath $ do
            recordTurnStart "t1"
            recordUserMsg "What files are here?"
            recordToolCall "listDir" (object ["dir" .= ("." :: Text)])
            recordToolResult "listDir" (Right (object ["entries" .= (["src", "test"] :: [Text])]))
            recordModelTurn "There are src and test directories."
            recordTurnFinish "t1"

          resumed <- runEff (resumeSession jPath)
          let hist = reconstructChatHistory resumed
          length hist `shouldBe` 3
          hist `shouldBe`
            [ CoreTypes.UserMsg "What files are here?"
            , CoreTypes.ToolMsg "listDir" "{\"entries\":[\"src\",\"test\"]}"
            , CoreTypes.AssistantMsg "There are src and test directories." []
            ]

      it "Audit Replay: verifies turn integrity and token metrics aggregation" $ do
        let events =
              [ TurnStarted "turn-1"
              , JournalUserMsg "Query 1"
              , ToolInvoked "toolA" "toolA" (object [])
              , ToolCompleted "toolA" (Right (object ["status" .= ("ok" :: Text)]))
              , ModelTurn "Done 1" []
              , MetricsReported (ModelMetrics 150 50 200 120.0 "gpt-4o")
              , TurnFinished "turn-1"
              ]
        case replayAudit events of
          Left err -> expectationFailure ("Replay failed: " <> T.unpack err)
          Right summary -> do
            rsTotalTurns summary `shouldBe` 1
            rsUserMessages summary `shouldBe` 1
            rsModelTurns summary `shouldBe` 1
            rsToolInvocations summary `shouldBe` 1
            rsToolCompletions summary `shouldBe` 1
            rsTotalTokens summary `shouldBe` 200
            rsIsValidSequence summary `shouldBe` True

      it "JournalState & Memory: maintains and clears session state in memory" $ do
        let program = do
              recordUserMsg "temp message"
              clearEvents
              recordUserMsg "kept message"
              getEvents

        (evs, _) <- runEff (runJournalMemory program)
        evs `shouldBe` [JournalUserMsg "kept message"]

    describe "3. Standard Coding Tools" $ do
      it "viewFileTool: slices lines and reports line count metadata" $ do
        let sample = "line 1\nline 2\nline 3\nline 4\n"
        let st = initMemoryWorld [("file.txt", sample)]
        (res, _) <- runEff $ runWorldMemory st $ do
          runViewFile (ViewFileArgs "file.txt" (Just 2) (Just 3) Nothing)
        case res of
          Left err -> expectationFailure ("View failed: " <> T.unpack err)
          Right vfr -> do
            vfrTotalLines vfr `shouldBe` 4
            map lineText (vfrLines vfr) `shouldBe` ["line 2", "line 3"]

      it "editFileTool: replaces target content and generates unified diff snippet" $ do
        let sample = "var a = 1;\nvar b = 2;\n"
        let st = initMemoryWorld [("app.js", sample)]
        (res, finalSt) <- runEff $ runWorldMemory st $ do
          runEditFile (EditFileArgs "app.js" Nothing "var a = 1;" "var a = 100;" Nothing Nothing Nothing)
        case res of
          Left err -> expectationFailure ("Edit failed: " <> T.unpack err)
          Right efr -> do
            efrReplacedCount efr `shouldBe` 1
            efrLinesModified efr `shouldBe` [1]
            Map.lookup "app.js" (mwsFiles finalSt) `shouldBe` Just "var a = 100;\nvar b = 2;\n"

      it "grepSearchTool: matches patterns across virtual workspace files" $ do
        let files = [("src/A.hs", "target symbol\n"), ("src/B.hs", "no match\n"), ("test/C.hs", "target symbol\n")]
        let st = initMemoryWorld files
        (res, _) <- runEff $ runWorldMemory st $ do
          runGrepSearch (GrepSearchArgs "target" Nothing Nothing Nothing Nothing (Just True) Nothing)
        case res of
          Left err -> expectationFailure ("Grep failed: " <> T.unpack err)
          Right gsr -> do
            gsrTotalCount gsr `shouldBe` 2
            map gmFilePath (gsrMatches gsr) `shouldBe` ["src/A.hs", "test/C.hs"]

      it "findByNameTool: locates files and directories matching glob patterns" $ do
        let files = [("app/Main.hs", ""), ("src/Lib.hs", ""), ("README.md", "")]
        let st = initMemoryWorld files
        (res, _) <- runEff $ runWorldMemory st $ do
          runFindByName (FindByNameArgs (Just "Lib") Nothing Nothing Nothing Nothing Nothing)
        case res of
          Left err -> expectationFailure ("Find failed: " <> T.unpack err)
          Right fbr -> map feiPath (fbrEntries fbr) `shouldBe` ["src/Lib.hs"]

      it "listDirTool: lists directory entries and directory flags" $ do
        let files = [("root/a.txt", ""), ("root/sub/b.txt", "")]
        let st = initMemoryWorld files
        (res, _) <- runEff $ runWorldMemory st $ do
          runListDir (ListDirArgs "root" Nothing Nothing)
        case res of
          Left err -> expectationFailure ("List failed: " <> T.unpack err)
          Right ldr -> do
            map deiName (ldrEntries ldr) `shouldBe` ["a.txt", "sub"]
            map deiIsDir (ldrEntries ldr) `shouldBe` [False, True]

      it "runCommandTool: executes commands and returns structured result" $ do
        let st = initMemoryWorld []
        (res, _) <- runEff $ runWorldMemory st $ do
          runRunCommand (RunCommandArgs "echo success" Nothing Nothing Nothing Nothing)
        case res of
          Left err -> expectationFailure ("Command failed: " <> T.unpack err)
          Right (CommandCompleted code out _ _) -> do
            code `shouldBe` 0
            out `shouldBe` "success\n"
          Right other -> expectationFailure ("Unexpected command result: " <> show other)

    describe "4. Subagent Delegation" $ do
      it "executes child subagent and returns structured SubagentResult" $ do
        let script =
              [ Right (toolResp [ToolCall "c1" "view_file" (toJSON (ViewFileArgs "src/A.hs" Nothing Nothing Nothing))])
              , Right (textResp "Module A is valid.")
              ]
        let args = SubagentArgs "Check A.hs" Nothing (Just 5) Nothing Nothing
        let worldSt = initMemoryWorld [("src/A.hs", "module A where\n")]
        (((res, _), _), _) <- runEff $ runJournalMemory $ runWorldMemory worldSt $ runLLMMock script $ do
          runSubagent args standardCodingTools
        srStatus res `shouldBe` "completed"
        srOutput res `shouldBe` "Module A is valid."

      it "restricts explorer role to read-only toolset" $ do
        let tools = standardCodingTools @'[World]
        let filtered = filterSubagentTools (SubagentArgs "read" (Just "explorer") Nothing Nothing Nothing) tools
        map (toolSpecName . toolSpec) filtered `shouldBe` ["view_file", "grep_search", "find_by_name", "list_dir"]

      it "enforces explicit allowedTools whitelist" $ do
        let tools = standardCodingTools @'[World]
        let filtered = filterSubagentTools (SubagentArgs "custom" Nothing Nothing (Just ["view_file", "run_command"]) Nothing) tools
        map (toolSpecName . toolSpec) filtered `shouldBe` ["view_file", "run_command"]

      it "enforces step budget and reports exhausted status gracefully" $ do
        let infiniteScript = repeat (Right (toolResp [ToolCall "c1" "view_file" (toJSON (ViewFileArgs "f.txt" Nothing Nothing Nothing))]))
        let args = SubagentArgs "loop" Nothing (Just 2) Nothing Nothing
        let worldSt = initMemoryWorld [("f.txt", "data")]
        (((res, _), _), _) <- runEff $ runJournalMemory $ runWorldMemory worldSt $ runLLMMock infiniteScript $ do
          runSubagent args standardCodingTools
        srStatus res `shouldBe` "exhausted"
        srRoundsUsed res `shouldBe` 2

      it "spawns subagent asynchronously on green thread" $ do
        let script = [Right (textResp "Async subagent result")]
        let args = SubagentArgs "async job" Nothing (Just 3) Nothing Nothing
        let worldSt = initMemoryWorld []
        (((res, _), _), _) <- runEff $ runJournalMemory $ runWorldMemory worldSt $ runLLMMock script $ do
          h <- runSubagentAsync args []
          liftIO (wait h)
        srStatus res `shouldBe` "completed"
        srOutput res `shouldBe` "Async subagent result"

    describe "5. Brick + Vty TUI State Machine & Widgets" $ do
      it "initializes AppState with default values" $ do
        let st = initialAppState defaultTUIConfig Nothing
        appStatus st `shouldBe` StatusIdle
        appMessages st `shouldBe` []
        appStreamingText st `shouldBe` ""
        focusGetCurrent (appFocusRing st) `shouldBe` Just EditorPrompt

      it "handles prompt submission and transitions to StatusThinking" $ do
        let st0 = initialAppState defaultTUIConfig Nothing
        let st1 = submitPromptPure "Build feature X" st0
        appStatus st1 `shouldBe` StatusThinking
        appMessages st1 `shouldBe` [UserMsg "Build feature X"]
        getEditContents (appEditor st1) `shouldBe` [""]

      it "handles token streaming and commits on turn completion" $ do
        let st0 = initialAppState defaultTUIConfig Nothing
        let st1 = handleCustomAppEvent (TokenStreamed "Generating ") st0
        let st2 = handleCustomAppEvent (TokenStreamed "code...") st1
        appStreamingText st2 `shouldBe` "Generating code..."
        appStatus st2 `shouldBe` StatusStreaming
        let st3 = handleCustomAppEvent TurnCompleted st2
        appStreamingText st3 `shouldBe` ""
        appMessages st3 `shouldBe` [AssistantMsg "Generating code..." []]
        appStatus st3 `shouldBe` StatusIdle
        amTurnCount (appMetrics st3) `shouldBe` 1

      it "handles keyboard focus cycling and shortcuts" $ do
        let st0 = initialAppState defaultTUIConfig Nothing
        let (st1, a1) = handleVtyEventPure (Vty.EvKey (Vty.KChar '\t') []) st0
        a1 `shouldBe` Just ActionFocusNext
        focusGetCurrent (appFocusRing st1) `shouldBe` Just ViewportChat

        let (st2, a2) = handleVtyEventPure (Vty.EvKey (Vty.KChar 'd') [Vty.MCtrl]) st1
        a2 `shouldBe` Just (ActionFocusResource ViewportDiff)
        focusGetCurrent (appFocusRing st2) `shouldBe` Just ViewportDiff

      it "renders UI widgets without exceptions across diverse states" $ do
        let st0 = initialAppState defaultTUIConfig Nothing
        let st1 = submitPromptPure "Refactor code" st0
        let st2 = handleCustomAppEvent (TokenStreamed "Refactoring in progress") st1
        let st3 = handleCustomAppEvent (DiffUpdated "--- a/f.hs\n+++ b/f.hs\n@@ -1 +1 @@\n-1\n+2\n") st2
        let ws = drawUI st3
        length ws `shouldBe` 1

  ---------------------------------------------------------------------------
  -- Tier 2: Boundary & Corner Cases
  ---------------------------------------------------------------------------
  describe "Tier 2: Boundary & Corner Cases" $ do

    describe "1. World Boundary Conditions" $ do
      it "handles 0-byte empty files and out-of-bounds line slices" $ do
        let st = initMemoryWorld [("empty.txt", "")]
        (res, _) <- runEff $ runWorldMemory st $ do
          c <- readFileText "empty.txt"
          s1 <- readFileSlice "empty.txt" (Just 1) (Just 10)
          s2 <- readFileSlice "empty.txt" (Just 50) (Just 100)
          pure (c, s1, s2)
        let (c, s1, s2) = res
        c `shouldBe` ""
        s1 `shouldBe` ""
        s2 `shouldBe` ""

      it "clamps negative, zero, and inverted slice parameters safely" $ do
        let st = initMemoryWorld [("content.txt", "l1\nl2\nl3\n")]
        (res, _) <- runEff $ runWorldMemory st $ do
          s0 <- readFileSlice "content.txt" (Just 0) (Just 2)
          sNeg <- readFileSlice "content.txt" (Just (-5)) (Just 1)
          sInv <- readFileSlice "content.txt" (Just 3) (Just 1)
          pure (s0, sNeg, sInv)
        let (s0, sNeg, sInv) = res
        s0 `shouldBe` "l1\nl2\n"
        sNeg `shouldBe` "l1\n"
        sInv `shouldBe` "l3\n"

      it "handles deep hierarchy nesting (20 levels) in memory" $ do
        let deepPath = "d1/d2/d3/d4/d5/d6/d7/d8/d9/d10/d11/d12/d13/d14/d15/d16/d17/d18/d19/d20/file.txt"
        let st = initMemoryWorld []
        (res, finalSt) <- runEff $ runWorldMemory st $ do
          writeFileText deepPath "nested 20 levels"
          r <- readFileText deepPath
          pEx <- doesPathExist deepPath
          dEx <- doesDirectoryExist "d1/d2/d3"
          pure (r, pEx, dEx)
        let (r, pEx, dEx) = res
        r `shouldBe` "nested 20 levels"
        pEx `shouldBe` True
        dEx `shouldBe` True
        Map.lookup deepPath (mwsFiles finalSt) `shouldBe` Just "nested 20 levels"

      it "throws WorldIsADirectory when reading directory as file" $ do
        let st = initMemoryWorld [("folder/file.txt", "data")]
        let action = runEff $ runWorldMemory st (readFileText "folder")
        action `shouldThrow` (\case WorldIsADirectory "folder" -> True; _ -> False)

      it "terminates long-running process when timeout expires" $ do
        withSystemTempDirectory "e2e-adv-timeout" $ \tmpDir -> do
          runEff $ runWorldLocal tmpDir $ do
            let specSleep = CommandSpec "sleep" ["10"] Nothing Nothing (Just 50) Nothing
            res <- runCommand specSleep
            liftIO $ do
              prTimedOut res `shouldBe` True
              prExitCode res `shouldBe` (-1)

    describe "2. Journal Boundary Conditions" $ do
      it "handles corrupted JSONL lines gracefully returning Left error" $ do
        withSystemTempDirectory "e2e-journal-bad" $ \tmpDir -> do
          let badPath = tmpDir </> "bad.jsonl"
          TIO.writeFile badPath "{\"type\":\"TurnStarted\",\"turnId\":\"t1\"}\nCORRUPT_NOT_JSON\n"
          res <- runEff (loadJournalFile badPath)
          res `shouldSatisfy` (\case Left _ -> True; Right _ -> False)

      it "detects unclosed TurnStarted in replay audit validation" $ do
        let events = [TurnStarted "t1", JournalUserMsg "hello"]
        case replayAudit events of
          Left err -> err `shouldSatisfy` ("never finished" `T.isInfixOf`)
          Right _  -> expectationFailure "Expected unclosed turn audit failure"

      it "detects mismatched TurnFinished in replay audit validation" $ do
        let events = [TurnStarted "t1", TurnFinished "t2"]
        case replayAudit events of
          Left err -> err `shouldSatisfy` ("does not match active turn" `T.isInfixOf`)
          Right _  -> expectationFailure "Expected mismatched turn audit failure"

      it "detects ToolCompleted without matching ToolInvoked" $ do
        let events = [TurnStarted "t1", ToolCompleted "toolX" (Right (object [])), TurnFinished "t1"]
        case replayAudit events of
          Left err -> err `shouldSatisfy` ("without matching ToolInvoked" `T.isInfixOf`)
          Right _  -> expectationFailure "Expected uninvoked tool completion failure"

      it "handles empty journal session as valid zero-counter audit" $ do
        case replayAudit [] of
          Left err -> expectationFailure ("Empty session failed: " <> T.unpack err)
          Right summary -> do
            rsTotalTurns summary `shouldBe` 0
            rsIsValidSequence summary `shouldBe` True

    describe "3. Coding Tools Boundary Conditions" $ do
      it "truncates large files exceeding 46080 bytes limit in viewFile" $ do
        let bigLine = T.replicate 100 "A"
        let bigText = T.unlines (replicate 600 bigLine)
        let st = initMemoryWorld [("big.txt", bigText)]
        (res, _) <- runEff $ runWorldMemory st $ do
          runViewFile (ViewFileArgs "big.txt" (Just 1) (Just 600) Nothing)
        case res of
          Left err -> expectationFailure ("View failed: " <> T.unpack err)
          Right vfr -> vfrIsTruncated vfr `shouldBe` True

      it "rejects ambiguous multiple occurrences in editFile when allowMultiple is False" $ do
        let textVal = "x = 1\nx = 1\nx = 1\n"
        let st = initMemoryWorld [("vars.txt", textVal)]
        (res, _) <- runEff $ runWorldMemory st $ do
          runEditFile (EditFileArgs "vars.txt" Nothing "x = 1" "x = 2" Nothing Nothing Nothing)
        case res of
          Left err -> err `shouldSatisfy` ("matched 3 times" `T.isInfixOf`)
          Right _  -> expectationFailure "Expected multiple match rejection"

      it "allows replacement within line bounds when multiple exist in file" $ do
        let textVal = "x = 1\nx = 1\nx = 1\n"
        let st = initMemoryWorld [("vars.txt", textVal)]
        (res, finalSt) <- runEff $ runWorldMemory st $ do
          runEditFile (EditFileArgs "vars.txt" Nothing "x = 1" "x = 99" (Just 2) (Just 2) Nothing)
        case res of
          Left err -> expectationFailure ("Edit bounded failed: " <> T.unpack err)
          Right efr -> do
            efrReplacedCount efr `shouldBe` 1
            Map.lookup "vars.txt" (mwsFiles finalSt) `shouldBe` Just "x = 1\nx = 99\nx = 1\n"

      it "creates a new file when targetContent is empty and file does not exist" $ do
        let st = initMemoryWorld []
        (res, finalSt) <- runEff $ runWorldMemory st $ do
          runEditFile (EditFileArgs "new.txt" Nothing "" "initial text\n" Nothing Nothing Nothing)
        case res of
          Left err -> expectationFailure ("Create failed: " <> T.unpack err)
          Right efr -> do
            efrReplacedCount efr `shouldBe` 1
            Map.lookup "new.txt" (mwsFiles finalSt) `shouldBe` Just "initial text\n"

      it "searches special characters literally when regex is False" $ do
        let files = [("calc.txt", "x + y * z = [1, 2, 3] ($USD)")]
        let st = initMemoryWorld files
        (res, _) <- runEff $ runWorldMemory st $ do
          runGrepSearch (GrepSearchArgs "[1, 2, 3]" Nothing (Just False) Nothing Nothing (Just True) Nothing)
        case res of
          Left err -> expectationFailure ("Grep special failed: " <> T.unpack err)
          Right gsr -> gsrTotalCount gsr `shouldBe` 1

    describe "4. Subagent & TUI Boundary Conditions" $ do
      it "strips subagentTool from child tools to prevent recursive fork-bombs" $ do
        let tools = standardCodingTools @'[World, Journal, LLM, IOE] ++ [subagentTool standardCodingTools]
        let filtered = filterSubagentTools (SubagentArgs "child" Nothing Nothing Nothing Nothing) tools
        let names = map (toolSpecName . toolSpec) filtered
        names `shouldNotContain` ["subagent"]

      it "cleans up Git worktree even when child execution throws exception" $ do
        withSystemTempDirectory "e2e-subagent-exc" $ \repoDir -> do
          _ <- readProcessWithExitCode "git" ["-C", repoDir, "init"] ""
          _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.email", "e2e@llmonad.org"] ""
          _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.name", "E2E Test"] ""
          writeFile (repoDir </> "code.txt") "base\n"
          _ <- readProcessWithExitCode "git" ["-C", repoDir, "add", "code.txt"] ""
          _ <- readProcessWithExitCode "git" ["-C", repoDir, "commit", "-m", "Init"] ""

          let cfg = defaultWorktreeConfig repoDir
          let failingAct = runEff $ runWorldWorktree cfg $ do
                writeFileText "code.txt" "corrupted"
                EE.throwIO (WorldIOError "deliberate crash")

          failingAct `shouldThrow` (\case WorldIOError "deliberate crash" -> True; _ -> False)

          (_, wtList, _) <- readProcessWithExitCode "git" ["-C", repoDir, "worktree", "list", "--porcelain"] ""
          let count = length (filter ("worktree " `T.isPrefixOf`) (T.lines (T.pack wtList)))
          count `shouldBe` 1

      it "TUI ignores empty prompts on Enter submission" $ do
        let st0 = initialAppState defaultTUIConfig Nothing
        let (st1, act) = handleVtyEventPure (Vty.EvKey Vty.KEnter []) st0
        act `shouldBe` Just ActionNone
        appMessages st1 `shouldBe` []
        appStatus st1 `shouldBe` StatusIdle

      it "TUI handles unicode and emoji in token streams" $ do
        let st0 = initialAppState defaultTUIConfig Nothing
        let st1 = handleCustomAppEvent (TokenStreamed "🚀 Haskell effectful 中文 🤖") st0
        appStreamingText st1 `shouldBe` "🚀 Haskell effectful 中文 🤖"

  ---------------------------------------------------------------------------
  -- Tier 3: Cross-Feature Interactions
  ---------------------------------------------------------------------------
  describe "Tier 3: Cross-Feature Interactions" $ do

    it "Interaction 1: Subagent running in Git worktree modifying files while Journal records events" $ do
      withSystemTempDirectory "e2e-interact-subagent" $ \repoDir -> do
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "init"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.email", "e2e@llmonad.org"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.name", "E2E Test"] ""
        writeFile (repoDir </> "module.py") "def run():\n    return 1\n"
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "add", "module.py"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "commit", "-m", "Initial commit"] ""

        let script =
              [ Right (toolResp [ToolCall "c1" "edit_file" (toJSON (EditFileArgs "module.py" Nothing "return 1" "return 42" Nothing Nothing Nothing))])
              , Right (textResp "Subagent updated module.py return value.")
              ]

        let args = SubagentArgs "Upgrade return value" Nothing (Just 5) Nothing (Just True)
        let journalPath = repoDir </> "session.jsonl"

        ((subRes, _), journalEvs) <- runEff $ runJournalFileWithEvents journalPath $ runWorldLocal repoDir $ runLLMMock script $ do
          runSubagent args standardCodingTools

        srStatus subRes `shouldBe` "completed"
        srOutput subRes `shouldBe` "Subagent updated module.py return value."
        srGitDiff subRes `shouldSatisfy` (\case Just d -> "+    return 42" `T.isInfixOf` d; Nothing -> False)

        -- Base repo must remain untouched
        basePy <- readFile (repoDir </> "module.py")
        basePy `shouldBe` "def run():\n    return 1\n"

        -- Journal events recorded properly
        length journalEvs `shouldBe` 2
        case journalEvs of
          [ToolInvoked "subagent" _ _, ToolCompleted "subagent" _] -> pure ()
          other -> expectationFailure ("Expected subagent tool start/finish events, got: " <> show other)

    it "Interaction 2: World Local operations logged to Journal and verified by Audit Replay" $ do
      withSystemTempDirectory "e2e-interact-world-journal" $ \tmpDir -> do
        let jPath = tmpDir </> "audit.jsonl"
        runEff $ runJournalFile jPath $ runWorldLocal tmpDir $ do
          recordTurnStart "turn-1"
          recordUserMsg "Create and test file"
          writeFileText "calc.txt" "10 + 20 = 30\n"
          recordToolCall "write_file" (object ["path" .= ("calc.txt" :: Text)])
          recordToolResult "write_file" (Right (object ["status" .= ("created" :: Text)]))
          res <- execShell "cat calc.txt"
          recordToolCall "run_command" (object ["cmd" .= ("cat calc.txt" :: Text)])
          recordToolResult "run_command" (Right (object ["stdout" .= prStdout res]))
          recordModelTurn "Verified calculation in calc.txt"
          recordMetrics (ModelMetrics 80 30 110 55.0 "mock-llm")
          recordTurnFinish "turn-1"

        resumed <- runEff (resumeSession jPath)
        length resumed `shouldBe` 9
        case replayAudit resumed of
          Left err -> expectationFailure ("Audit failed: " <> T.unpack err)
          Right summary -> do
            rsTotalTurns summary `shouldBe` 1
            rsToolInvocations summary `shouldBe` 2
            rsToolCompletions summary `shouldBe` 2
            rsIsValidSequence summary `shouldBe` True

    it "Interaction 3: TUI state machine processes editFile tool execution and renders visual diff" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
      let st1 = submitPromptPure "Refactor API router" st0
      let editArgs = toJSON (EditFileArgs "src/Router.hs" Nothing "oldRoute" "newRoute" Nothing Nothing Nothing)
      let st2 = handleCustomAppEvent (ToolStarted "edit_file" editArgs) st1
      appStatus st2 `shouldBe` StatusRunningTool "edit_file"

      let diffSnippet = "--- a/src/Router.hs\n+++ b/src/Router.hs\n@@ -10,1 +10,1 @@\n-oldRoute\n+newRoute\n"
      let toolRes = Right (object ["diffSnippet" .= diffSnippet, "replaced" .= (1 :: Int)])
      let st3 = handleCustomAppEvent (ToolFinished "edit_file" toolRes) st2

      case appDiffState st3 of
        Nothing -> expectationFailure "Expected visual diff state to be updated"
        Just ds -> vdsContent ds `shouldBe` diffSnippet

      let st4 = handleCustomAppEvent (TokenStreamed "Router refactored.") st3
      let st5 = handleCustomAppEvent TurnCompleted st4
      appStatus st5 `shouldBe` StatusIdle
      appMessages st5 `shouldBe`
        [ UserMsg "Refactor API router"
        , AssistantMsg "Router refactored." []
        ]
      let ws = drawUI st5
      length ws `shouldBe` 1

    it "Interaction 4: Error recovery lifecycle with tool failure and self-correction" $ do
      let script =
            [ Right (toolResp [ToolCall "c1" "view_file" (toJSON (ViewFileArgs "missing.hs" Nothing Nothing Nothing))])
            , Right (toolResp [ToolCall "c2" "view_file" (toJSON (ViewFileArgs "existing.hs" Nothing Nothing Nothing))])
            , Right (textResp "Found existing.hs after resolving missing.hs error.")
            ]
      let worldSt = initMemoryWorld [("existing.hs", "module Existing where\n")]
      let jProgram = do
            recordTurnStart "t-err"
            recordUserMsg "Find module"
            ans <- runAgent standardCodingTools "Find existing module"
            recordModelTurn ans
            recordTurnFinish "t-err"
            getEvents

      (((events, _), _), _) <- runEff $ runJournalMemory $ runWorldMemory worldSt $ runLLMMock script jProgram
      length events `shouldSatisfy` (>= 4)
      case replayAudit events of
        Left err -> expectationFailure ("Audit replay on recovery failed: " <> T.unpack err)
        Right s  -> rsIsValidSequence s `shouldBe` True

    it "Interaction 5: Full coding pipeline chain across six standard tools" $ do
      let initialFiles =
            [ ("project/README.md", "# Project Documentation\n")
            , ("project/src/Calc.hs", "module Calc where\nmultiply a b = a + b\n")
            , ("project/test/Spec.hs", "main = pure ()\n")
            ]
      let worldSt = initMemoryWorld initialFiles

      (pipelineRes, finalWorld) <- runEff $ runWorldMemory worldSt $ do
        -- 1. listDir
        rList <- runListDir (ListDirArgs "project" Nothing Nothing)
        -- 2. findByName
        rFind <- runFindByName (FindByNameArgs (Just "Calc") (Just "project") Nothing Nothing Nothing Nothing)
        -- 3. grepSearch
        rGrep <- runGrepSearch (GrepSearchArgs "multiply" (Just "project/src") Nothing Nothing Nothing (Just True) Nothing)
        -- 4. viewFile
        rView <- runViewFile (ViewFileArgs "project/src/Calc.hs" (Just 1) (Just 2) Nothing)
        -- 5. editFile
        rEdit <- runEditFile (EditFileArgs "project/src/Calc.hs" Nothing "a + b" "a * b" Nothing Nothing Nothing)
        -- 6. runCommand
        rCmd <- runRunCommand (RunCommandArgs "echo pipeline_complete" Nothing Nothing Nothing Nothing)
        pure (rList, rFind, rGrep, rView, rEdit, rCmd)

      let (rList, rFind, rGrep, rView, rEdit, rCmd) = pipelineRes
      case (rList, rFind, rGrep, rView, rEdit, rCmd) of
        (Right ldr, Right fbr, Right gsr, Right vfr, Right efr, Right (CommandCompleted _ out _ _)) -> do
          length (ldrEntries ldr) `shouldBe` 3
          fbrTotalCount fbr `shouldBe` 1
          gsrTotalCount gsr `shouldBe` 1
          vfrTotalLines vfr `shouldBe` 2
          efrReplacedCount efr `shouldBe` 1
          out `shouldBe` "pipeline_complete\n"
          Map.lookup "project/src/Calc.hs" (mwsFiles finalWorld) `shouldBe` Just "module Calc where\nmultiply a b = a * b\n"
        other -> expectationFailure ("Pipeline failed: " <> show other)

  ---------------------------------------------------------------------------
  -- Tier 4: Real-World Application Scenarios
  ---------------------------------------------------------------------------
  describe "Tier 4: Real-World Application Scenarios" $ do

    it "Scenario 1: Codebase Inspection & Search" $ do
      let codebase =
            [ ("src/Server.hs", "module Server where\nimport Router\nstartServer port = runRouter port\n")
            , ("src/Router.hs", "module Router where\nrunRouter p = print p\n")
            , ("src/Config.hs", "module Config where\ndefaultPort = 8080\n")
            , ("README.md", "# Web Application\nRun with defaultPort.\n")
            ]
      let worldSt = initMemoryWorld codebase
      let script =
            [ Right (toolResp [ToolCall "c1" "list_dir" (toJSON (ListDirArgs "src" Nothing Nothing))])
            , Right (toolResp [ToolCall "c2" "find_by_name" (toJSON (FindByNameArgs (Just "Router") (Just "src") Nothing Nothing Nothing Nothing))])
            , Right (toolResp [ToolCall "c3" "grep_search" (toJSON (GrepSearchArgs "defaultPort" (Just "src") Nothing Nothing Nothing (Just True) Nothing))])
            , Right (toolResp [ToolCall "c4" "view_file" (toJSON (ViewFileArgs "src/Config.hs" (Just 1) (Just 2) Nothing))])
            , Right (textResp "Inspection complete: Server uses Router and Config defines defaultPort = 8080.")
            ]

      (((ans, _), _), _) <- runEff $ runJournalMemory $ runWorldMemory worldSt $ runLLMMock script $ do
        runAgent standardCodingTools "Inspect the codebase architecture and locate defaultPort definition"

      ans `shouldBe` "Inspection complete: Server uses Router and Config defines defaultPort = 8080."

    it "Scenario 2: Multi-File Refactoring with Diffs" $ do
      let codebase =
            [ ("src/Math.hs", "module Math where\nadd x y = x + y\n")
            , ("src/Util.hs", "module Util where\nimport Math\ncalc = add 10 20\n")
            ]
      let worldSt = initMemoryWorld codebase
      let script =
            [ Right (toolResp [ToolCall "c1" "view_file" (toJSON (ViewFileArgs "src/Math.hs" Nothing Nothing Nothing))])
            , Right (toolResp [ToolCall "c2" "edit_file" (toJSON (EditFileArgs "src/Math.hs" Nothing "add x y = x + y" "addNumbers x y = x + y" Nothing Nothing Nothing))])
            , Right (toolResp [ToolCall "c3" "edit_file" (toJSON (EditFileArgs "src/Util.hs" Nothing "add 10 20" "addNumbers 10 20" Nothing Nothing Nothing))])
            , Right (toolResp [ToolCall "c4" "run_command" (toJSON (RunCommandArgs "echo 'build ok'" Nothing Nothing Nothing Nothing))])
            , Right (textResp "Multi-file refactoring completed: renamed add to addNumbers across Math and Util.")
            ]

      (((ans, _), finalWorld), _) <- runEff $ runJournalMemory $ runWorldMemory worldSt $ runLLMMock script $ do
        runAgent standardCodingTools "Rename add function to addNumbers in Math.hs and update callers"

      ans `shouldBe` "Multi-file refactoring completed: renamed add to addNumbers across Math and Util."
      Map.lookup "src/Math.hs" (mwsFiles finalWorld) `shouldBe` Just "module Math where\naddNumbers x y = x + y\n"
      Map.lookup "src/Util.hs" (mwsFiles finalWorld) `shouldBe` Just "module Util where\nimport Math\ncalc = addNumbers 10 20\n"

    it "Scenario 3: Isolated Subagent Feature Implementation in Git worktree" $ do
      withSystemTempDirectory "e2e-scenario-subagent-git" $ \repoDir -> do
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "init"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.email", "agent@llmonad.org"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.name", "Agent Subagent"] ""
        writeFile (repoDir </> "feature.py") "def feature():\n    return 'v1'\n"
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "add", "feature.py"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "commit", "-m", "Initial commit"] ""

        let script =
              [ Right (toolResp [ToolCall "c1" "edit_file" (toJSON (EditFileArgs "feature.py" Nothing "'v1'" "'v2_optimized'" Nothing Nothing Nothing))])
              , Right (textResp "Feature upgraded to v2_optimized")
              ]

        let args = SubagentArgs "Optimize feature" Nothing (Just 5) Nothing (Just True)
        let journalPath = repoDir </> "subagent_audit.jsonl"

        ((subRes, _), _) <- runEff $ runJournalFileWithEvents journalPath $ runWorldLocal repoDir $ runLLMMock script $ do
          runSubagent args standardCodingTools

        srStatus subRes `shouldBe` "completed"
        srGitDiff subRes `shouldSatisfy` (\case Just d -> "+    return 'v2_optimized'" `T.isInfixOf` d; Nothing -> False)

        -- Check main repo isolation
        baseCode <- readFile (repoDir </> "feature.py")
        baseCode `shouldBe` "def feature():\n    return 'v1'\n"

        -- Validate persisted audit log
        resumed <- runEff (resumeSession journalPath)
        length resumed `shouldBe` 2

    it "Scenario 4: Session Crash & Resume with Audit Replay" $ do
      withSystemTempDirectory "e2e-scenario-crash-resume" $ \tmpDir -> do
        let journalPath = tmpDir </> "crash_session.jsonl"

        -- Turn 1 before crash
        runEff $ runJournalFile journalPath $ do
          recordTurnStart "turn-1"
          recordUserMsg "Search for database config"
          recordToolCall "grep_search" (object ["query" .= ("db_host" :: Text)])
          recordToolResult "grep_search" (Right (object ["matches" .= (["config.env: db_host=localhost"] :: [Text])]))
          recordModelTurn "Database host is configured as localhost."
          recordMetrics (ModelMetrics 100 25 125 75.0 "gpt-4o")
          recordTurnFinish "turn-1"

        -- Simulate crash and resume
        resumedEvents <- runEff (resumeSession journalPath)
        let chatHist = reconstructChatHistory resumedEvents
        length chatHist `shouldBe` 3

        -- Validate audit replay
        case replayAudit resumedEvents of
          Left err -> expectationFailure ("Crash replay failed: " <> T.unpack err)
          Right summary -> do
            rsTotalTurns summary `shouldBe` 1
            rsIsValidSequence summary `shouldBe` True

        -- Continue Turn 2 after recovery
        runEff $ runJournalFile journalPath $ do
          recordTurnStart "turn-2"
          recordUserMsg "Now change db_host to postgres"
          recordToolCall "edit_file" (object ["path" .= ("config.env" :: Text)])
          recordToolResult "edit_file" (Right (object ["status" .= ("updated" :: Text)]))
          recordModelTurn "Updated db_host to postgres."
          recordMetrics (ModelMetrics 120 20 140 60.0 "gpt-4o")
          recordTurnFinish "turn-2"

        finalEvents <- runEff (resumeSession journalPath)
        length finalEvents `shouldBe` 14
        case replayAudit finalEvents of
          Left err -> expectationFailure ("Final audit failed: " <> T.unpack err)
          Right s  -> rsTotalTurns s `shouldBe` 2

    it "Scenario 5: Interactive TUI Prompt & Streaming Workflow" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
      -- 1. User submits prompt
      let st1 = submitPromptPure "Refactor user authentication handler" st0
      appStatus st1 `shouldBe` StatusThinking
      appMessages st1 `shouldBe` [UserMsg "Refactor user authentication handler"]

      -- 2. Agent invokes editFile tool
      let editArgs = toJSON (EditFileArgs "src/Auth.hs" Nothing "checkPassword" "verifyPasswordHash" Nothing Nothing Nothing)
      let st2 = handleCustomAppEvent (ToolStarted "editFile" editArgs) st1
      appStatus st2 `shouldBe` StatusRunningTool "editFile"

      -- 3. Tool produces unified diff
      let diff = "--- a/src/Auth.hs\n+++ b/src/Auth.hs\n@@ -25,1 +25,1 @@\n-checkPassword\n+verifyPasswordHash\n"
      let st3 = handleCustomAppEvent (ToolFinished "editFile" (Right (object ["diff" .= diff]))) st2
      case appDiffState st3 of
        Nothing -> expectationFailure "Expected visual diff in TUI"
        Just ds -> vdsContent ds `shouldBe` diff

      -- 4. Agent streams explanation tokens
      let st4 = handleCustomAppEvent (TokenStreamed "I updated ") st3
      let st5 = handleCustomAppEvent (TokenStreamed "checkPassword to verifyPasswordHash.") st4
      appStreamingText st5 `shouldBe` "I updated checkPassword to verifyPasswordHash."

      -- 5. Turn completes
      let st6 = handleCustomAppEvent TurnCompleted st5
      appStatus st6 `shouldBe` StatusIdle
      appMessages st6 `shouldBe`
        [ UserMsg "Refactor user authentication handler"
        , AssistantMsg "I updated checkPassword to verifyPasswordHash." []
        ]

      -- 6. User navigates diff pane and clears it
      let (st7, _) = handleVtyEventPure (Vty.EvKey (Vty.KChar 'd') [Vty.MCtrl]) st6
      focusGetCurrent (appFocusRing st7) `shouldBe` Just ViewportDiff
      let (st8, aClear) = handleVtyEventPure (Vty.EvKey (Vty.KChar 'x') [Vty.MCtrl]) st7
      aClear `shouldBe` Just ActionClearDiff
      appDiffState st8 `shouldBe` Nothing

  ---------------------------------------------------------------------------
  -- Tier 5: Adversarial Hardening
  ---------------------------------------------------------------------------
  describe "Tier 5: Adversarial Hardening" $ do

    it "Stress 1: High-Volume 10,000 Micro-Token Streaming into TUI" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
      let chunks = [T.singleton c | c <- take 10000 (cycle ['a'..'z'])]
      let finalSt = foldl (\s chunk -> handleCustomAppEvent (TokenStreamed chunk) s) st0 chunks
      appStatus finalSt `shouldBe` StatusStreaming
      T.length (appStreamingText finalSt) `shouldBe` 10000
      let completedSt = handleCustomAppEvent TurnCompleted finalSt
      appStatus completedSt `shouldBe` StatusIdle
      length (appMessages completedSt) `shouldBe` 1

    it "Stress 2: Massive Multi-Turn Session (100 Turns / 500 Events)" $ do
      let n = 100
      let program = do
            forM_ [1 .. n] $ \(i :: Int) -> do
              let tid = "t-" <> T.pack (show i)
              recordTurnStart tid
              recordUserMsg ("User message " <> T.pack (show i))
              recordToolCall "tool_ping" (object ["i" .= i])
              recordToolResult "tool_ping" (Right (object ["status" .= ("pong" :: Text)]))
              recordModelTurn ("Model response " <> T.pack (show i))
              recordTurnFinish tid
            getEvents

      (events, _) <- runEff (runJournalMemory program)
      length events `shouldBe` (n * 6)
      case replayAudit events of
        Left err -> expectationFailure ("100 turns audit failed: " <> T.unpack err)
        Right s  -> do
          rsTotalTurns s `shouldBe` n
          rsUserMessages s `shouldBe` n
          rsModelTurns s `shouldBe` n
          rsIsValidSequence s `shouldBe` True

    it "Stress 3: Concurrent Multi-Worker Local File Operations" $ do
      withSystemTempDirectory "e2e-adv-concurrent-local" $ \tmpDir -> do
        let numThreads = 10
        runEff $ runWorldLocal tmpDir $ do
          forM_ [1 .. numThreads] $ \i ->
            writeFileText (T.unpack ("worker_" <> T.pack (show (i :: Int)) <> ".txt")) ("init_" <> T.pack (show (i :: Int)))

        forConcurrently_ [1 .. numThreads] $ \i -> do
          runEff $ runWorldLocal tmpDir $ do
            let fn = T.unpack ("worker_" <> T.pack (show (i :: Int)) <> ".txt")
            val <- readFileText fn
            writeFileText fn (val <> "_done")

        runEff $ runWorldLocal tmpDir $ do
          forM_ [1 .. numThreads] $ \i -> do
            let fn = T.unpack ("worker_" <> T.pack (show (i :: Int)) <> ".txt")
            c <- readFileText fn
            liftIO $ c `shouldBe` ("init_" <> T.pack (show (i :: Int)) <> "_done")

    it "Stress 4: Concurrent Git Worktree Sandboxes Without Leaks" $ do
      withSystemTempDirectory "e2e-adv-concurrent-wt" $ \repoDir -> do
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "init"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.email", "e2e@llmonad.org"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.name", "E2E Test"] ""
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

    it "Stress 5: Extreme TUI Layout with 500 Messages, 500 Logs, and Large Diff" $ do
      let msgs = [if even i then UserMsg ("Q" <> T.pack (show i)) else AssistantMsg ("A" <> T.pack (show i)) [] | i <- [1..500 :: Int]]
      let logs = [ToolLogEntry ("tool_" <> T.pack (show i)) (object ["idx" .= i]) (Just (Right (object ["ok" .= True]))) "done" | i <- [1..500 :: Int]]
      let diff = T.unlines ["+ diff line " <> T.pack (show i) | i <- [1..1000 :: Int]]
      let st = (initialAppState defaultTUIConfig Nothing)
            { appMessages = msgs
            , appToolLogs = logs
            , appDiffState = Just (VisualDiffState diff (Just "extreme.hs"))
            , appStreamingText = "Streaming active data..."
            , appStatus = StatusStreaming
            , appMetrics = AppMetrics 10000 50000 60000 120.0 500
            }
      let ws = drawUI st
      length ws `shouldBe` 1
