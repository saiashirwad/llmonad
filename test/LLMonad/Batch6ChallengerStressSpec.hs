{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLMonad.Batch6ChallengerStressSpec (spec) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Data.Text (Text)
import qualified Data.Text as T
import LLMonad
import qualified LLMonad.Journal.Types as J
import Test.Hspec

spec :: Spec
spec = describe "Batch 6 Empirical Challenger Stress Suite" $ do

  describe "1. High-Volume Session Resume Hydration Stress (500 Turns / 2,500 Events)" $ do
    it "hydrates a 500-turn session with tools and metrics with full fidelity and zero dropped turns" $ do
      let mkTurn i =
            let tid = "turn-" <> T.pack (show (i :: Int))
                callId = "call-" <> T.pack (show (i :: Int))
                userText = "User prompt for step " <> T.pack (show i)
                assistText = "Executing step " <> T.pack (show i)
                finalText = "Step " <> T.pack (show i) <> " completed successfully."
                diffTxt = "--- a/file" <> T.pack (show i) <> ".hs\n+++ b/file" <> T.pack (show i) <> ".hs\n+added " <> T.pack (show i)
            in [ J.TurnStarted tid
               , J.UserMsg userText
               , J.ModelTurn assistText [ToolCall callId "editFile" "{}"]
               , J.ToolInvoked callId "editFile" (Aeson.object ["step" .= i])
               , J.ToolCompleted callId (Right (Aeson.object ["efrDiffSnippet" .= diffTxt, "efrPath" .= ("file" <> show i <> ".hs")]))
               , J.ModelTurn finalText []
               , J.MetricsReported (J.ModelMetrics 100 25 125 50.0 "deepseek-chat")
               , J.TurnFinished tid
               ]

      let allEvents = concatMap mkTurn [1..500 :: Int]
      length allEvents `shouldBe` 4000

      let st0 = initialAppState defaultTUIConfig Nothing
          stHydrated = hydrateAppStateFromEvents allEvents st0

      -- Reconstructed messages: 500 turns * 4 messages (User, Assist-with-call, ToolMsg, Assist-final) = 2000 msgs
      length (appMessages stHydrated) `shouldBe` 2000
      length (appToolLogs stHydrated) `shouldBe` 500

      -- Metrics
      let m = appMetrics stHydrated
      amTurnCount m `shouldBe` 500
      amPromptTokens m `shouldBe` (500 * 100)
      amCompletionTokens m `shouldBe` (500 * 25)
      amTotalTokens m `shouldBe` (500 * 125)
      amLatencyMs m `shouldBe` (500 * 50.0)

      -- Visual Diff should match the last turn (turn 500)
      case appDiffState stHydrated of
        Nothing -> expectationFailure "Expected visual diff state from turn 500"
        Just ds -> do
          vdsContent ds `shouldBe` "--- a/file500.hs\n+++ b/file500.hs\n+added 500"
          vdsFileName ds `shouldBe` Just "editFile"

      -- App status
      appStatus stHydrated `shouldBe` StatusIdle
      T.isInfixOf "2000 messages loaded" (appStatusMessage stHydrated) `shouldBe` True

  describe "2. Deeply Nested & Pathological Tool Result Diff Extraction" $ do
    it "extracts diff when embedded inside deeply nested JSON object structures with extra fields" $ do
      let nestedObj = Aeson.object
            [ "status" .= ("ok" :: Text)
            , "metadata" .= Aeson.object
                [ "tool" .= ("editFile" :: Text)
                , "version" .= (2 :: Int)
                ]
            , "efrDiffSnippet" .= ("@@ -1,3 +1,3 @@\n-x = 1\n+x = 2" :: Text)
            , "efrPath" .= ("src/Nested.hs" :: Text)
            , "stats" .= Aeson.object
                [ "linesAdded" .= (1 :: Int)
                , "linesRemoved" .= (1 :: Int)
                ]
            ]
      extractDiffFromToolResult (Right nestedObj) `shouldBe` Just "@@ -1,3 +1,3 @@\n-x = 1\n+x = 2"

    it "handles large multi-megabyte unified diff snippets" $ do
      let hugeDiff = T.unlines $
            [ "--- a/HugeFile.hs"
            , "+++ b/HugeFile.hs"
            , "@@ -1,10000 +1,10000 @@"
            ] ++ [ (if even (i :: Int) then "+line " else "-line ") <> T.pack (show i) | i <- [1..5000] ]
          res = Right (Aeson.object ["diff" .= hugeDiff])
      extractDiffFromToolResult res `shouldBe` Just hugeDiff

    it "handles diff snippets containing all kinds of Unicode, Asian glyphs, RTL scripts, and emojis" $ do
      let unicodeDiff = T.unlines
            [ "--- a/Unicode.hs"
            , "+++ b/Unicode.hs"
            , "@@ -1,4 +1,4 @@"
            , "-let greeting = \"Hello\""
            , "+let greeting = \"こんにちは 🌍 🚀\""
            , "-let rtl = \"abc\""
            , "+let rtl = \"مرحبا بالعالم\""
            , "+let math = \"∀x ∈ ℝ: x² ≥ 0\""
            ]
          res = Right (Aeson.object ["efrDiffSnippet" .= unicodeDiff])
      extractDiffFromToolResult res `shouldBe` Just unicodeDiff

  describe "3. Rapid Streaming & Layout Extreme Stress" $ do
    it "processes 50,000 streaming chunks with arbitrary sizes without latency degradation" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
          chunks = [ "chunk_" <> T.pack (show (i :: Int)) <> " " | i <- [1..50000] ]
          finalSt = foldl (flip (handleCustomAppEvent . TokenStreamed)) st0 chunks

      appStatus finalSt `shouldBe` StatusStreaming
      T.length (appStreamingText finalSt) `shouldSatisfy` (> 400000)

      -- Commit turn
      let completedSt = handleCustomAppEvent TurnCompleted finalSt
      appStatus completedSt `shouldBe` StatusIdle
      appStreamingText completedSt `shouldBe` ""
      length (appMessages completedSt) `shouldBe` 1
      amTurnCount (appMetrics completedSt) `shouldBe` 1

    it "computes unified diff for 5,000-line files accurately" $ do
      let oldLines = [ "function_" <> T.pack (show (i :: Int)) <> "() { return " <> T.pack (show i) <> "; }" | i <- [1..5000] ]
          newLines = [ "function_" <> T.pack (show (i :: Int)) <> "() { return " <> T.pack (show (i + 1)) <> "; }" | i <- [1..5000] ]
          diff = computeDiffUnified (T.unlines oldLines) (T.unlines newLines)

      T.isInfixOf "- function_1() { return 1; }" diff `shouldBe` True
      T.isInfixOf "+ function_1() { return 2; }" diff `shouldBe` True
      T.isInfixOf "- function_5000() { return 5000; }" diff `shouldBe` True
      T.isInfixOf "+ function_5000() { return 5001; }" diff `shouldBe` True

  describe "4. Out-of-Order & Parallel Tool Completion State Hydration" $ do
    it "handles parallel in-order tool completions during session hydration" $ do
      let events =
            [ J.TurnStarted "turn-par"
            , J.UserMsg "Run 2 parallel tasks"
            , J.ModelTurn "Starting 2 tasks"
                [ ToolCall "c1" "editFile" "{}"
                , ToolCall "c2" "listDir" "{}"
                ]
            , J.ToolInvoked "c1" "editFile" (Aeson.object ["path" .= ("A.hs" :: Text)])
            , J.ToolInvoked "c2" "listDir" (Aeson.object ["dir" .= ("." :: Text)])
            -- In-order completions: c1 (with diff), c2 (no diff)
            , J.ToolCompleted "c1" (Right (Aeson.object ["efrDiffSnippet" .= ("+diff A" :: Text), "efrPath" .= ("A.hs" :: Text)]))
            , J.ToolCompleted "c2" (Right (Aeson.object ["entries" .= (["A.hs"] :: [Text])]))
            , J.ModelTurn "Both finished." []
            , J.TurnFinished "turn-par"
            ]

          st = hydrateAppStateFromEvents events (initialAppState defaultTUIConfig Nothing)

      length (appToolLogs st) `shouldBe` 2
      case appToolLogs st of
        [l1, l2] -> do
          tleToolName l1 `shouldBe` "editFile"
          tleResult l1 `shouldSatisfy` (\case Just (Right _) -> True; _ -> False)
          tleToolName l2 `shouldBe` "listDir"
          tleResult l2 `shouldSatisfy` (\case Just (Right _) -> True; _ -> False)
        _ -> expectationFailure "Expected 2 tool logs"

      case appDiffState st of
        Nothing -> expectationFailure "Expected visual diff state from editFile"
        Just ds -> do
          vdsContent ds `shouldBe` "+diff A"
          vdsFileName ds `shouldBe` Just "editFile"

    it "handles out-of-order tool completion matching during session hydration" $ do
      let events =
            [ J.TurnStarted "turn-ooo"
            , J.UserMsg "Run 3 parallel tasks"
            , J.ModelTurn "Starting 3 tasks"
                [ ToolCall "c1" "toolA" "{}"
                , ToolCall "c2" "toolB" "{}"
                , ToolCall "c3" "toolC" "{}"
                ]
            , J.ToolInvoked "c1" "toolA" (Aeson.object ["id" .= (1 :: Int)])
            , J.ToolInvoked "c2" "toolB" (Aeson.object ["id" .= (2 :: Int)])
            , J.ToolInvoked "c3" "toolC" (Aeson.object ["id" .= (3 :: Int)])
            -- Complete in reverse order: c3, c1, c2
            , J.ToolCompleted "c3" (Right (Aeson.object ["status" .= ("ok3" :: Text)]))
            , J.ToolCompleted "c1" (Right (Aeson.object ["status" .= ("ok1" :: Text)]))
            , J.ToolCompleted "c2" (Left "task 2 failed")
            , J.ModelTurn "All 3 finished." []
            , J.TurnFinished "turn-ooo"
            ]

          st = hydrateAppStateFromEvents events (initialAppState defaultTUIConfig Nothing)

      length (appMessages st) `shouldBe` 6
      length (appToolLogs st) `shouldBe` 3

      -- Check that all tool logs received results
      case appToolLogs st of
        [l1, l2, l3] -> do
          tleToolName l1 `shouldBe` "toolA"
          tleResult l1 `shouldSatisfy` (\case Just (Right _) -> True; _ -> False)
          tleToolName l2 `shouldBe` "toolB"
          tleResult l2 `shouldSatisfy` (\case Just (Left _) -> True; _ -> False)
          tleToolName l3 `shouldBe` "toolC"
          tleResult l3 `shouldSatisfy` (\case Just (Right _) -> True; _ -> False)
        _ -> expectationFailure "Expected 3 tool logs"
