{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Comprehensive test suite for Subagent Delegation (Milestone 3 / R3).
module LLMonad.SubagentSpec (spec) where

import Control.Concurrent.Async (wait)
import Data.Aeson (toJSON)
import qualified Data.Text as T
import Effectful
import LLMonad
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Hspec

spec :: Spec
spec = describe "LLMonad.Subagent (Milestone 3)" $ do

  describe "1. Tool Filtering and Role Restrictions" $ do
    it "prevents recursive subagent calls by stripping subagent tool from child" $ do
      let tools = standardCodingTools @'[World, Journal, LLM, IOE] ++ [subagentTool standardCodingTools]
      let filtered = filterSubagentTools (SubagentArgs "task" Nothing Nothing Nothing Nothing) tools
      map (toolSpecName . toolSpec) filtered `shouldBe`
        [ "view_file"
        , "edit_file"
        , "grep_search"
        , "find_by_name"
        , "list_dir"
        , "run_command"
        ]

    it "restricts explorer role to read-only tools" $ do
      let tools = standardCodingTools @'[World]
      let filtered = filterSubagentTools (SubagentArgs "explore" (Just "explorer") Nothing Nothing Nothing) tools
      map (toolSpecName . toolSpec) filtered `shouldBe`
        [ "view_file"
        , "grep_search"
        , "find_by_name"
        , "list_dir"
        ]

    it "enforces explicit allowedTools whitelist" $ do
      let tools = standardCodingTools @'[World]
      let filtered = filterSubagentTools (SubagentArgs "search" Nothing Nothing (Just ["grep_search", "view_file"]) Nothing) tools
      map (toolSpecName . toolSpec) filtered `shouldBe` ["view_file", "grep_search"]

  describe "2. Step Budget Enforcement" $ do
    it "returns exhausted status when child agent exceeds step budget without crashing parent" $ do
      let infiniteLoopScript = repeat (Right (toolResp [ToolCall "loop" "view_file" (toJSON (ViewFileArgs "f.txt" Nothing Nothing Nothing))]))
      let args = SubagentArgs "infinite task" Nothing (Just 3) Nothing Nothing
      let worldState = initMemoryWorld [("f.txt", "data")]

      (((res, _reqs), _finalWorld), _journalEvents) <- runEff $ runJournalMemory $ runWorldMemory worldState $ runLLMMock infiniteLoopScript $ do
        runSubagent args standardCodingTools

      srStatus res `shouldBe` "exhausted"
      srRoundsUsed res `shouldBe` 3
      srOutput res `shouldSatisfy` ("budget" `T.isInfixOf`)

  describe "3. Direct Subagent Execution in Pure Environment" $ do
    it "executes child agent successfully and returns structured SubagentResult" $ do
      let script =
            [ Right (toolResp [ToolCall "c1" "view_file" (toJSON (ViewFileArgs "src/A.hs" Nothing Nothing Nothing))])
            , Right (textResp "Analysis complete: Module has no syntax errors.")
            ]
      let args = SubagentArgs "Analyze src/A.hs" (Just "explorer") (Just 5) Nothing Nothing
      let worldState = initMemoryWorld [("src/A.hs", "module A where\nx = 1\n")]

      (((res, _reqs), _finalWorld), journalEvents) <- runEff $ runJournalMemory $ runWorldMemory worldState $ runLLMMock script $ do
        runSubagent args standardCodingTools

      srStatus res `shouldBe` "completed"
      srOutput res `shouldBe` "Analysis complete: Module has no syntax errors."
      length journalEvents `shouldBe` 2
      case journalEvents of
        [ToolInvoked "subagent" _ _, ToolCompleted "subagent" _] -> pure ()
        other -> expectationFailure ("Expected subagent start/finish journal events, got: " <> show other)

  describe "4. Asynchronous Subagent Delegation (Green Threads)" $ do
    it "spawns subagent on background thread and awaits completion" $ do
      let script = [Right (textResp "Subagent async finished")]
      let args = SubagentArgs "Async job" Nothing (Just 4) Nothing Nothing
      let worldState = initMemoryWorld []

      (((res, _reqs), _finalWorld), _journalEvents) <- runEff $ runJournalMemory $ runWorldMemory worldState $ runLLMMock script $ do
        asyncHandle <- runSubagentAsync args []
        liftIO (wait asyncHandle)

      srStatus res `shouldBe` "completed"
      srOutput res `shouldBe` "Subagent async finished"

  describe "5. Git Worktree Isolation Sandboxing" $ do
    it "runs subagent in isolated Git worktree, collects diff, and cleans up worktree" $ do
      withSystemTempDirectory "llmonad-subagent-git" $ \repoDir -> do
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "init"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.email", "test@llmonad.org"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "config", "user.name", "LLMonad Test"] ""
        writeFile (repoDir </> "code.py") "def hello():\n    return 'old'\n"
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "add", "code.py"] ""
        _ <- readProcessWithExitCode "git" ["-C", repoDir, "commit", "-m", "Initial commit"] ""

        let script =
              [ Right (toolResp [ToolCall "c1" "edit_file" (toJSON (EditFileArgs "code.py" Nothing "'old'" "'new'" Nothing Nothing Nothing))])
              , Right (textResp "Updated greeting in code.py")
              ]

        let args = SubagentArgs "Refactor greeting" Nothing (Just 6) Nothing (Just True)

        ((res, _reqs), journalEvents) <- runEff $ runJournalMemory $ runWorldLocal repoDir $ runLLMMock script $ do
          runSubagent args standardCodingTools

        srStatus res `shouldBe` "completed"
        srOutput res `shouldBe` "Updated greeting in code.py"
        srGitDiff res `shouldSatisfy` (\case Just d -> "+    return 'new'" `T.isInfixOf` d; Nothing -> False)
        srModifiedFiles res `shouldSatisfy` (\f -> "code.py" `elem` f)

        -- Base repo should remain untouched until merged
        baseCode <- readFile (repoDir </> "code.py")
        baseCode `shouldBe` "def hello():\n    return 'old'\n"
        length journalEvents `shouldBe` 2

  describe "6. subagentTool Invocation via LLM Loop" $ do
    it "allows parent agent to invoke subagentTool as a first-class tool" $ do
      let script =
            [ Right (toolResp [ToolCall "call-sub" "subagent" (toJSON (SubagentArgs "List files" (Just "explorer") (Just 4) Nothing Nothing))])
            , Right (textResp "Child result: found 3 files")
            , Right (textResp "Parent summary based on child findings.")
            ]
      let worldState = initMemoryWorld [("a.txt", "a"), ("b.txt", "b"), ("c.txt", "c")]

      -- Run parent agent with subagentTool in toolset
      let tools = [subagentTool standardCodingTools]
      (((answer, _reqs), _finalWorld), _journalEvents) <- runEff $ runJournalMemory $ runWorldMemory worldState $ runLLMMock script $ do
        runAgent tools "Delegate file listing to subagent"

      answer `shouldBe` "Parent summary based on child findings."
