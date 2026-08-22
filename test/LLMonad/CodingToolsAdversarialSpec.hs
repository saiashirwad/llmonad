{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Adversarial Stress & Edge Case Test Suite for Coding Tools & Subagents (Milestone 3 / R3).
module LLMonad.CodingToolsAdversarialSpec (spec) where

import Data.Aeson (toJSON)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Effectful
import LLMonad
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Hspec

spec :: Spec
spec = describe "Coding Tools & Subagents Adversarial Suite (Milestone 3)" $ do

  describe "1. viewFile Tool Edge Cases" $ do
    it "handles negative line bounds by clamping safely" $ do
      let sampleText = "line 1\nline 2\nline 3\nline 4\nline 5\n"
      let st = initMemoryWorld [("test.txt", sampleText)]
      (res, _) <- runEff $ runWorldMemory st $ do
        runViewFile (ViewFileArgs "test.txt" (Just (-10)) (Just (-5)) (Just (-20)))

      case res of
        Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
        Right vfr -> do
          vfrPath vfr `shouldBe` "test.txt"
          vfrStartLine vfr `shouldBe` 1
          vfrContentOffset vfr `shouldBe` 0
          -- startLine clamped to 1, endLine clamped to max start (-5) = 1
          vfrLines vfr `shouldBe` [ViewFileLine 1 "line 1"]

    it "handles inverted line bounds (startLine > endLine) without crashing" $ do
      let sampleText = "line 1\nline 2\nline 3\n"
      let st = initMemoryWorld [("test.txt", sampleText)]
      (res, _) <- runEff $ runWorldMemory st $ do
        runViewFile (ViewFileArgs "test.txt" (Just 3) (Just 1) Nothing)

      case res of
        Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
        Right vfr -> do
          vfrLines vfr `shouldBe` [ViewFileLine 3 "line 3"]
          vfrStartLine vfr `shouldBe` 3
          vfrEndLine vfr `shouldBe` 3

    it "handles startLine far beyond total file lines" $ do
      let sampleText = "line 1\nline 2\n"
      let st = initMemoryWorld [("test.txt", sampleText)]
      (res, _) <- runEff $ runWorldMemory st $ do
        runViewFile (ViewFileArgs "test.txt" (Just 1000) (Just 2000) Nothing)

      case res of
        Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
        Right vfr -> do
          vfrTotalLines vfr `shouldBe` 2
          vfrLines vfr `shouldBe` []

    it "truncates large files exceeding 46080 bytes limit" $ do
      let largeLine = T.replicate 100 "A"
      let largeContent = T.unlines (replicate 600 largeLine) -- > 60KB
      let st = initMemoryWorld [("large.txt", largeContent)]
      (res, _) <- runEff $ runWorldMemory st $ do
        runViewFile (ViewFileArgs "large.txt" (Just 1) (Just 600) Nothing)

      case res of
        Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
        Right vfr -> do
          vfrIsTruncated vfr `shouldBe` True
          length (vfrLines vfr) `shouldSatisfy` (< 600)

    it "rejects viewing a directory path" $ do
      let st = initMemoryWorld [("dir/a.txt", "content")]
      (res, _) <- runEff $ runWorldMemory st $ do
        runViewFile (ViewFileArgs "dir" Nothing Nothing Nothing)

      case res of
        Left err -> err `shouldSatisfy` ("directory" `T.isInfixOf`)
        Right _  -> expectationFailure "Expected directory error"

    it "rejects viewing non-existent file" $ do
      let st = initMemoryWorld []
      (res, _) <- runEff $ runWorldMemory st $ do
        runViewFile (ViewFileArgs "missing.txt" Nothing Nothing Nothing)

      case res of
        Left err -> err `shouldSatisfy` ("File not found" `T.isInfixOf`)
        Right _  -> expectationFailure "Expected missing file error"

    it "handles empty 0-byte file cleanly" $ do
      let st = initMemoryWorld [("empty.txt", "")]
      (res, _) <- runEff $ runWorldMemory st $ do
        runViewFile (ViewFileArgs "empty.txt" Nothing Nothing Nothing)

      case res of
        Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
        Right vfr -> do
          vfrTotalLines vfr `shouldBe` 0
          vfrLines vfr `shouldBe` []

  describe "2. editFile Tool Edge Cases" $ do
    it "rejects ambiguous multiple occurrences when allowMultiple is False or omitted" $ do
      let content = "dup_key = 1\ndup_key = 2\ndup_key = 3\n"
      let st = initMemoryWorld [("config.ini", content)]
      (res, _) <- runEff $ runWorldMemory st $ do
        runEditFile (EditFileArgs "config.ini" Nothing "dup_key" "new_key" Nothing Nothing Nothing)

      case res of
        Left err -> err `shouldSatisfy` ("matched 3 times" `T.isInfixOf`)
        Right _  -> expectationFailure "Expected ambiguous edit rejection"

    it "allows replacement within narrow line bounds when multiple exist in file" $ do
      let content = "dup_key = 1\ndup_key = 2\ndup_key = 3\n"
      let st = initMemoryWorld [("config.ini", content)]
      (res, finalSt) <- runEff $ runWorldMemory st $ do
        runEditFile (EditFileArgs "config.ini" Nothing "dup_key" "new_key" (Just 2) (Just 2) Nothing)

      case res of
        Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
        Right efr -> do
          efrReplacedCount efr `shouldBe` 1
          efrLinesModified efr `shouldBe` [2]
          Map.lookup "config.ini" (mwsFiles finalSt) `shouldBe` Just "dup_key = 1\nnew_key = 2\ndup_key = 3\n"

    it "rejects ambiguous occurrences within line bounds if still multiple in range" $ do
      let content = "dup_key = 1\ndup_key = 2\ndup_key = 3\n"
      let st = initMemoryWorld [("config.ini", content)]
      (res, _) <- runEff $ runWorldMemory st $ do
        runEditFile (EditFileArgs "config.ini" Nothing "dup_key" "new_key" (Just 1) (Just 2) (Just False))

      case res of
        Left err -> err `shouldSatisfy` ("matched multiple times in specified range" `T.isInfixOf`)
        Right _  -> expectationFailure "Expected multiple match in range rejection"

    it "replaces all occurrences globally when allowMultiple is True" $ do
      let content = "foo = 1\nfoo = 2\nfoo = 3\n"
      let st = initMemoryWorld [("test.txt", content)]
      (res, finalSt) <- runEff $ runWorldMemory st $ do
        runEditFile (EditFileArgs "test.txt" Nothing "foo" "bar" Nothing Nothing (Just True))

      case res of
        Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
        Right efr -> do
          efrReplacedCount efr `shouldBe` 3
          Map.lookup "test.txt" (mwsFiles finalSt) `shouldBe` Just "bar = 1\nbar = 2\nbar = 3\n"

    it "rejects invalid line bounds (startLine > endLine or startLine > totalLines)" $ do
      let content = "line 1\nline 2\nline 3\n"
      let st = initMemoryWorld [("test.txt", content)]
      (res1, _) <- runEff $ runWorldMemory st $ do
        runEditFile (EditFileArgs "test.txt" Nothing "line 2" "mod" (Just 5) (Just 2) Nothing)

      case res1 of
        Left err -> err `shouldSatisfy` ("invalid or out of bounds" `T.isInfixOf`)
        Right _  -> expectationFailure "Expected invalid bounds error"

      (res2, _) <- runEff $ runWorldMemory st $ do
        runEditFile (EditFileArgs "test.txt" Nothing "line 2" "mod" (Just 50) (Just 60) Nothing)

      case res2 of
        Left err -> err `shouldSatisfy` ("invalid or out of bounds" `T.isInfixOf`)
        Right _  -> expectationFailure "Expected out of bounds error"

    it "rejects editing a directory path" $ do
      let st = initMemoryWorld [("dir/sub.txt", "data")]
      (res, _) <- runEff $ runWorldMemory st $ do
        runEditFile (EditFileArgs "dir" Nothing "data" "new" Nothing Nothing Nothing)

      case res of
        Left err -> err `shouldSatisfy` ("directory" `T.isInfixOf`)
        Right _  -> expectationFailure "Expected directory error"

    it "rejects editing a non-existent file when targetContent is non-empty" $ do
      let st = initMemoryWorld []
      (res, _) <- runEff $ runWorldMemory st $ do
        runEditFile (EditFileArgs "nonexistent.txt" Nothing "foo" "bar" Nothing Nothing Nothing)

      case res of
        Left err -> err `shouldSatisfy` ("File not found" `T.isInfixOf`)
        Right _  -> expectationFailure "Expected file not found error"

    it "creates a new file when targetContent is empty and file does not exist" $ do
      let st = initMemoryWorld []
      (res, finalSt) <- runEff $ runWorldMemory st $ do
        runEditFile (EditFileArgs "brand_new.txt" (Just "create") "" "init content\n" Nothing Nothing Nothing)

      case res of
        Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
        Right efr -> do
          efrReplacedCount efr `shouldBe` 1
          Map.lookup "brand_new.txt" (mwsFiles finalSt) `shouldBe` Just "init content\n"

    it "rejects empty targetContent when file already exists" $ do
      let st = initMemoryWorld [("existing.txt", "some content\n")]
      (res, _) <- runEff $ runWorldMemory st $ do
        runEditFile (EditFileArgs "existing.txt" Nothing "" "prepended\n" Nothing Nothing Nothing)

      case res of
        Left err -> err `shouldSatisfy` ("Target content not found" `T.isInfixOf`)
        Right _  -> expectationFailure "Expected target content not found error"

    it "returns 0 replacements when targetContent == replacementContent" $ do
      let content = "unchanged content\n"
      let st = initMemoryWorld [("same.txt", content)]
      (res, _) <- runEff $ runWorldMemory st $ do
        runEditFile (EditFileArgs "same.txt" Nothing "unchanged content\n" "unchanged content\n" Nothing Nothing Nothing)

      case res of
        Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
        Right efr -> do
          efrReplacedCount efr `shouldBe` 0
          efrLinesModified efr `shouldBe` []

  describe "3. listDir Tool Edge Cases" $ do
    it "rejects non-existent directory" $ do
      let st = initMemoryWorld []
      (res, _) <- runEff $ runWorldMemory st $ do
        runListDir (ListDirArgs "missing_dir" Nothing Nothing)

      case res of
        Left err -> err `shouldSatisfy` ("Directory not found" `T.isInfixOf`)
        Right _  -> expectationFailure "Expected directory not found error"

    it "rejects listing a file path as a directory" $ do
      let st = initMemoryWorld [("file.txt", "text")]
      (res, _) <- runEff $ runWorldMemory st $ do
        runListDir (ListDirArgs "file.txt" Nothing Nothing)

      case res of
        Left err -> err `shouldSatisfy` ("Directory not found" `T.isInfixOf`)
        Right _  -> expectationFailure "Expected directory not found error"

  describe "4. Subagent Budget Exhaustion & Recursion Restriction" $ do
    it "handles subagent round budget exhaustion without throwing unhandled exception" $ do
      let loopScript = repeat (Right (toolResp [ToolCall "c1" "view_file" (toJSON (ViewFileArgs "f.txt" Nothing Nothing Nothing))]))
      let args = SubagentArgs "Run infinite loop" Nothing (Just 2) Nothing Nothing
      let worldState = initMemoryWorld [("f.txt", "content")]

      (((res, _reqs), _finalWorld), journalEvents) <- runEff $ runJournalMemory $ runWorldMemory worldState $ runLLMMock loopScript $ do
        runSubagent args standardCodingTools

      srStatus res `shouldBe` "exhausted"
      srRoundsUsed res `shouldBe` 2
      srOutput res `shouldSatisfy` ("budget exhausted" `T.isInfixOf`)
      length journalEvents `shouldBe` 2

    it "strips subagent and subagentTool from child tools to prevent fork-bomb recursion" $ do
      let tools = standardCodingTools @'[World, Journal, LLM, IOE] ++ [subagentTool standardCodingTools, subagentToolWith defaultAgentOpts standardCodingTools]
      let filtered = filterSubagentTools (SubagentArgs "child" Nothing Nothing Nothing Nothing) tools
      let names = map (toolSpecName . toolSpec) filtered
      names `shouldNotContain` ["subagent"]
      names `shouldNotContain` ["subagentTool"]

    it "strictly enforces read-only tool filtering for explorer and readonly roles" $ do
      let tools = standardCodingTools @'[World]
      let explorerFiltered = filterSubagentTools (SubagentArgs "explore" (Just "explorer") Nothing Nothing Nothing) tools
      let readonlyFiltered = filterSubagentTools (SubagentArgs "read" (Just "readonly") Nothing Nothing Nothing) tools
      let expNames = map (toolSpecName . toolSpec) explorerFiltered
      let roNames = map (toolSpecName . toolSpec) readonlyFiltered

      expNames `shouldBe` ["view_file", "grep_search", "find_by_name", "list_dir"]
      roNames `shouldBe` ["view_file", "grep_search", "find_by_name", "list_dir"]
      expNames `shouldNotContain` ["edit_file", "run_command"]
      roNames `shouldNotContain` ["edit_file", "run_command"]

    it "enforces explicit allowedTools whitelist" $ do
      let tools = standardCodingTools @'[World]
      let filtered = filterSubagentTools (SubagentArgs "whitelist" Nothing Nothing (Just ["run_command", "view_file"]) Nothing) tools
      let names = map (toolSpecName . toolSpec) filtered
      names `shouldBe` ["view_file", "run_command"]

  describe "5. Subagent Git Worktree Sandboxing & Isolation" $ do
    it "ensures child modifications in worktree do not affect parent workspace until merged" $ do
      withSystemTempDirectory "llmonad-adv-worktree" $ \repoDir -> do
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "init"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.email", "adv@llmonad.org"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.name", "Adv Test"] ""
        writeFile (repoDir </> "main.py") "value = 100\n"
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "add", "main.py"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "commit", "-m", "Initial commit"] ""

        let script =
              [ Right (toolResp [ToolCall "c1" "edit_file" (toJSON (EditFileArgs "main.py" Nothing "100" "999" Nothing Nothing Nothing))])
              , Right (textResp "Child completed edit")
              ]

        let args = SubagentArgs "Update value to 999" Nothing (Just 5) Nothing (Just True)

        ((res, _reqs), _journal) <- runEff $ runJournalMemory $ runWorldLocal repoDir $ runLLMMock script $ do
          runSubagent args standardCodingTools

        srStatus res `shouldBe` "completed"
        srOutput res `shouldBe` "Child completed edit"
        srGitDiff res `shouldSatisfy` (\case Just d -> "+value = 999" `T.isInfixOf` d; Nothing -> False)
        srModifiedFiles res `shouldBe` ["main.py"]

        -- Parent repository must be untouched
        parentContent <- readFile (repoDir </> "main.py")
        parentContent `shouldBe` "value = 100\n"

        -- Git worktree list should only have main worktree (ephemeral worktree cleaned up)
        (_, wtList, _) <- readProcessWithExitCode "git" ["-C", repoDir, "worktree", "list"] ""
        length (lines wtList) `shouldBe` 1
