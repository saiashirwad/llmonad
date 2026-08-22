{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLMonad.Batch6ChallengerSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Data.Text (Text)
import qualified Data.Text as T
import Effectful (runEff)
import qualified Graphics.Vty as Vty
import LLMonad
import LLMonad.Journal.File (runJournalFile)
import LLMonad.Journal.Replay (loadJournalFile)
import qualified LLMonad.Journal.Types as J
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "Batch 6 Empirical Challenger Suite" $ do

  describe "1. TUI Diff Extraction & Precedence Stress Tests" $ do
    it "extracts diff from EditFileResult representation with efrDiffSnippet and efrPath" $ do
      let diffTxt = "--- a/src/Core.hs\n+++ b/src/Core.hs\n@@ -10,3 +10,4 @@\n-oldLine\n+newLine\n+extraLine\n"
          res = Right (Aeson.object
            [ "efrPath" .= ("src/Core.hs" :: Text)
            , "efrReplacedChunks" .= (2 :: Int)
            , "efrTotalChunks" .= (2 :: Int)
            , "efrDiffSnippet" .= diffTxt
            ])
      extractDiffFromToolResult res `shouldBe` Just diffTxt

    it "respects candidate key precedence: efrDiffSnippet > diffSnippet > diff_snippet > diff" $ do
      let diffEfr = "diff-efr" :: Text
          diffSnippet = "diff-snippet" :: Text
          diffUnderscore = "diff-underscore" :: Text
          diffPlain = "diff-plain" :: Text

      -- All 4 present -> efrDiffSnippet wins
      let resAll = Right (Aeson.object
            [ "efrDiffSnippet" .= diffEfr
            , "diffSnippet" .= diffSnippet
            , "diff_snippet" .= diffUnderscore
            , "diff" .= diffPlain
            ])
      extractDiffFromToolResult resAll `shouldBe` Just diffEfr

      -- 3 present -> diffSnippet wins
      let res3 = Right (Aeson.object
            [ "diffSnippet" .= diffSnippet
            , "diff_snippet" .= diffUnderscore
            , "diff" .= diffPlain
            ])
      extractDiffFromToolResult res3 `shouldBe` Just diffSnippet

      -- 2 present -> diff_snippet wins
      let res2 = Right (Aeson.object
            [ "diff_snippet" .= diffUnderscore
            , "diff" .= diffPlain
            ])
      extractDiffFromToolResult res2 `shouldBe` Just diffUnderscore

      -- 1 present -> diff wins
      let res1 = Right (Aeson.object
            [ "diff" .= diffPlain
            ])
      extractDiffFromToolResult res1 `shouldBe` Just diffPlain

    it "skips empty or whitespace-only diff strings in higher-priority keys and falls back to populated keys" $ do
      let resFallback = Right (Aeson.object
            [ "efrDiffSnippet" .= ("" :: Text) -- empty string should be skipped
            , "diffSnippet" .= ("" :: Text)    -- empty string should be skipped
            , "diff" .= ("+valid diff line" :: Text)
            ])
      extractDiffFromToolResult resFallback `shouldBe` Just "+valid diff line"

    it "returns Nothing safely for non-string, null, or malformed diff fields" $ do
      let nonStringResults =
            [ Right (Aeson.object ["efrDiffSnippet" .= (12345 :: Int)])
            , Right (Aeson.object ["diffSnippet" .= True])
            , Right (Aeson.object ["diff_snippet" .= Aeson.Null])
            , Right (Aeson.object ["diff" .= Aeson.object ["nested" .= ("not-a-string" :: Text)]])
            , Right (Aeson.object ["diff" .= ([1, 2, 3] :: [Int])])
            , Right (Aeson.Array mempty)
            , Right (Aeson.Bool False)
            , Right (Aeson.String "raw diff without object wrapper")
            , Right (Aeson.Number 42)
            , Left "Tool execution error"
            , Left ""
            ]
      forM_ nonStringResults $ \res -> do
        extractDiffFromToolResult res `shouldBe` Nothing

    it "preserves active visual diff when a subsequent non-diff tool finishes" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
          diffText = "--- a/test.hs\n+++ b/test.hs\n+new"
          st1 = handleCustomAppEvent (ToolStarted "editFile" (Aeson.object [])) st0
          st2 = handleCustomAppEvent (ToolFinished "editFile" (Right (Aeson.object ["efrDiffSnippet" .= diffText]))) st1
      appDiffState st2 `shouldBe` Just (VisualDiffState diffText (Just "editFile"))

      -- Subsequent tool execution without diff
      let st3 = handleCustomAppEvent (ToolStarted "listDir" (Aeson.object [])) st2
          st4 = handleCustomAppEvent (ToolFinished "listDir" (Right (Aeson.object ["entries" .= (["a", "b"] :: [Text])]))) st3
      appDiffState st4 `shouldBe` Just (VisualDiffState diffText (Just "editFile"))

    it "overwrites active visual diff when a new tool with a diff finishes" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
          diff1 = "+diff 1" :: Text
          diff2 = "+diff 2" :: Text
          st1 = handleCustomAppEvent (ToolFinished "edit1" (Right (Aeson.object ["diff" .= diff1]))) st0
          st2 = handleCustomAppEvent (ToolFinished "edit2" (Right (Aeson.object ["efrDiffSnippet" .= diff2]))) st1
      appDiffState st2 `shouldBe` Just (VisualDiffState diff2 (Just "edit2"))

  describe "2. Multi-Turn Session Resume & State Hydration Fidelity" $ do
    it "rehydrates multi-turn session with alternating prose, tools, errors, and diffs" $ do
      let diffA = "--- a/A.hs\n+++ b/A.hs\n+module A\n" :: Text
          diffB = "--- a/B.hs\n+++ b/B.hs\n+module B\n" :: Text
          events =
            [ -- Turn 1: Inspect
              J.TurnStarted "turn-1"
            , J.UserMsg "Check files"
            , J.ModelTurn "Listing directory..." [ToolCall "c1" "listDir" "{\"directoryPath\":\".\"}"]
            , J.ToolInvoked "c1" "listDir" (Aeson.object ["directoryPath" .= ("." :: Text)])
            , J.ToolCompleted "c1" (Right (Aeson.object ["entries" .= (["A.hs", "B.hs"] :: [Text])]))
            , J.ModelTurn "Found A.hs and B.hs." []
            , J.MetricsReported (J.ModelMetrics 100 50 150 120.0 "deepseek-chat")
            , J.TurnFinished "turn-1"

            -- Turn 2: Edit A.hs (success with diff)
            , J.TurnStarted "turn-2"
            , J.UserMsg "Update A.hs"
            , J.ModelTurn "Editing A.hs..." [ToolCall "c2" "editFile" "{\"targetFile\":\"A.hs\"}"]
            , J.ToolInvoked "c2" "editFile" (Aeson.object ["targetFile" .= ("A.hs" :: Text)])
            , J.ToolCompleted "c2" (Right (Aeson.object ["efrDiffSnippet" .= diffA, "efrPath" .= ("A.hs" :: Text)]))
            , J.ModelTurn "Updated A.hs." []
            , J.MetricsReported (J.ModelMetrics 200 80 280 180.0 "deepseek-chat")
            , J.TurnFinished "turn-2"

            -- Turn 3: Attempt command (error) and retry with edit B.hs
            , J.TurnStarted "turn-3"
            , J.UserMsg "Build and update B.hs"
            , J.ModelTurn "Running build..." [ToolCall "c3" "runCommand" "{\"cmd\":\"cabal build\"}"]
            , J.ToolInvoked "c3" "runCommand" (Aeson.object ["cmd" .= ("cabal build" :: Text)])
            , J.ToolCompleted "c3" (Left "Build failed: syntax error in B.hs")
            , J.ModelTurn "Fixing B.hs..." [ToolCall "c4" "editFile" "{\"targetFile\":\"B.hs\"}"]
            , J.ToolInvoked "c4" "editFile" (Aeson.object ["targetFile" .= ("B.hs" :: Text)])
            , J.ToolCompleted "c4" (Right (Aeson.object ["diffSnippet" .= diffB]))
            , J.ModelTurn "Fixed B.hs and verified build." []
            , J.MetricsReported (J.ModelMetrics 350 120 470 300.0 "deepseek-chat")
            , J.TurnFinished "turn-3"
            ]

          cfg = defaultTUIConfig { cfgModelName = "deepseek-chat" }
          st = initialAppStateWithEvents cfg Nothing events

      -- Check hydrated message count
      -- Turn 1: UserMsg, AssistantMsg (tool c1), ToolMsg c1, AssistantMsg = 4 msgs
      -- Turn 2: UserMsg, AssistantMsg (tool c2), ToolMsg c2, AssistantMsg = 4 msgs
      -- Turn 3: UserMsg, AssistantMsg (tool c3), ToolMsg c3 (Error), AssistantMsg (tool c4), ToolMsg c4, AssistantMsg = 6 msgs
      -- Total = 14 messages
      length (appMessages st) `shouldBe` 14

      -- Check tool logs
      length (appToolLogs st) `shouldBe` 4
      map tleToolName (appToolLogs st) `shouldBe` ["listDir", "editFile", "runCommand", "editFile"]

      -- Check visual diff (latest diff is diffB from c4)
      case appDiffState st of
        Nothing -> expectationFailure "Expected hydrated visual diff state"
        Just ds -> do
          vdsContent ds `shouldBe` diffB
          vdsFileName ds `shouldBe` Just "editFile"

      -- Check aggregated metrics
      let m = appMetrics st
      amPromptTokens m `shouldBe` (100 + 200 + 350)
      amCompletionTokens m `shouldBe` (50 + 80 + 120)
      amTotalTokens m `shouldBe` (150 + 280 + 470)
      amTurnCount m `shouldBe` 3
      amLatencyMs m `shouldBe` (120.0 + 180.0 + 300.0)

      -- Check status message
      T.isInfixOf "14 messages loaded" (appStatusMessage st) `shouldBe` True

    it "persists and restores state accurately through disk JSONL journal file" $ do
      withSystemTempDirectory "batch6_tui_resume" $ \tmpDir -> do
        let journalPath = tmpDir ++ "/session.jsonl"
            diffSnippet = "+line added\n-line removed" :: Text
            events =
              [ J.TurnStarted "t1"
              , J.UserMsg "Hello agent"
              , J.ModelTurn "Running tool" [ToolCall "call-1" "edit" "{}"]
              , J.ToolInvoked "call-1" "edit" (Aeson.object [])
              , J.ToolCompleted "call-1" (Right (Aeson.object ["efrDiffSnippet" .= diffSnippet]))
              , J.ModelTurn "Finished" []
              , J.MetricsReported (J.ModelMetrics 50 25 75 80.0 "deepseek-chat")
              , J.TurnFinished "t1"
              ]

        -- Write to disk journal
        runEff $ runJournalFile journalPath $ do
          forM_ events recordEvent

        -- Load from disk
        loadRes <- runEff (loadJournalFile journalPath)
        case loadRes of
          Left err -> expectationFailure ("Failed to load journal file: " ++ T.unpack err)
          Right loadedEvents -> do
            loadedEvents `shouldBe` events
            let cfg = defaultTUIConfig { cfgSessionFilePath = Just journalPath }
                st = initialAppStateWithEvents cfg Nothing loadedEvents
            length (appMessages st) `shouldBe` 4
            appDiffState st `shouldBe` Just (VisualDiffState diffSnippet (Just "edit"))
            amTotalTokens (appMetrics st) `shouldBe` 75
            amTurnCount (appMetrics st) `shouldBe` 1

    it "seamlessly continues conversational interaction after session resume" $ do
      let initialEvents =
            [ J.TurnStarted "t1"
            , J.UserMsg "Original prompt"
            , J.ModelTurn "Original reply" []
            , J.TurnFinished "t1"
            ]
          st0 = initialAppStateWithEvents defaultTUIConfig Nothing initialEvents

      -- Submit next prompt
      let st1 = submitPromptPure "Follow-up question" st0
      appStatus st1 `shouldBe` StatusThinking
      length (appMessages st1) `shouldBe` 3

      -- Stream response
      let st2 = handleCustomAppEvent (TokenStreamed "Streaming follow-up...") st1
      appStatus st2 `shouldBe` StatusStreaming

      -- Complete turn
      let st3 = handleCustomAppEvent TurnCompleted st2
      appStatus st3 `shouldBe` StatusIdle
      length (appMessages st3) `shouldBe` 4
      amTurnCount (appMetrics st3) `shouldBe` 2
      appMessages st3 `shouldBe`
        [ UserMsg "Original prompt"
        , AssistantMsg "Original reply" []
        , UserMsg "Follow-up question"
        , AssistantMsg "Streaming follow-up..." []
        ]

  describe "3. Pure Navigation & Layout Invariants" $ do
    it "preserves state invariant: clear diff with Ctrl+X leaves message history and tool logs intact" $ do
      let st0 = (initialAppState defaultTUIConfig Nothing)
            { appMessages  = [UserMsg "Task", AssistantMsg "Done" []]
            , appToolLogs  = [ToolLogEntry "tool1" (Aeson.object []) (Just (Right (Aeson.object []))) "done"]
            , appDiffState = Just (VisualDiffState "+diff" (Just "file.hs"))
            }
          (st1, action) = handleVtyEventPure (Vty.EvKey (Vty.KChar 'x') [Vty.MCtrl]) st0
      action `shouldBe` Just ActionClearDiff
      appDiffState st1 `shouldBe` Nothing
      length (appMessages st1) `shouldBe` 2
      length (appToolLogs st1) `shouldBe` 1

    it "drawUI executes cleanly across all Viewport combinations and Diff states" $ do
      let baseSt = initialAppState defaultTUIConfig Nothing
          statesToRender =
            [ baseSt
            , baseSt { appDiffState = Just (VisualDiffState "+added\n-removed\n@@ -1 +1 @@\n context" (Just "src/Main.hs")) }
            , baseSt { appStatus = StatusRunningTool "editFile", appStreamingText = "Streaming text chunk" }
            , baseSt { appStatus = StatusError "Fatal socket exception" }
            ]
      forM_ statesToRender $ \st -> do
        let widgets = drawUI st
        length widgets `shouldBe` 1
