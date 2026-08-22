{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLMonad.TUISpec (spec) where

import qualified Brick
import Brick.Focus (focusGetCurrent)
import Brick.Widgets.Edit (editorText, getEditContents)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import LLMonad
import Test.Hspec

spec :: Spec
spec = describe "Brick + Vty Interactive TUI (Milestone 4)" $ do

  describe "1. Initial State & Configuration" $ do
    it "initializes empty AppState with valid defaults" $ do
      let cfg = defaultTUIConfig
          st = initialAppState cfg Nothing
      appStatus st `shouldBe` StatusIdle
      appMessages st `shouldBe` []
      appStreamingText st `shouldBe` ""
      appToolLogs st `shouldBe` []
      appDiffState st `shouldBe` Nothing
      appStatusMessage st `shouldBe` "Ready"
      appWorkspacePath st `shouldBe` "."
      appModelName st `shouldBe` "deepseek-chat"
      appSystemPrompt st `shouldBe` Nothing
      appSessionFilePath st `shouldBe` Nothing
      amTurnCount (appMetrics st) `shouldBe` 0
      amTotalTokens (appMetrics st) `shouldBe` 0
      focusGetCurrent (appFocusRing st) `shouldBe` Just EditorPrompt

    it "respects custom TUIConfig parameters" $ do
      let cfg = TUIConfig
            { cfgWorkspacePath   = "/custom/workspace"
            , cfgModelName       = "custom-model"
            , cfgSystemPrompt    = Just "System instructions"
            , cfgSessionFilePath = Just "/tmp/session.jsonl"
            }
          st = initialAppState cfg Nothing
      appWorkspacePath st `shouldBe` "/custom/workspace"
      appModelName st `shouldBe` "custom-model"
      appSystemPrompt st `shouldBe` Just "System instructions"
      appSessionFilePath st `shouldBe` Just "/tmp/session.jsonl"

  describe "2. Pure Prompt Submission (submitPromptPure)" $ do
    it "appends user message, resets editor, and updates status to StatusThinking" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
          st1 = submitPromptPure "Fix the bug in src/Main.hs" st0
      appMessages st1 `shouldBe` [UserMsg "Fix the bug in src/Main.hs"]
      appStatus st1 `shouldBe` StatusThinking
      appStatusMessage st1 `shouldBe` "Processing user prompt..."
      appStreamingText st1 `shouldBe` ""
      getEditContents (appEditor st1) `shouldBe` [""]

    it "handles consecutive prompt submissions across multiple turns" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
          st1 = submitPromptPure "First instruction" st0
          st2 = handleCustomAppEvent TurnCompleted st1
          st3 = submitPromptPure "Second instruction" st2
      length (appMessages st3) `shouldBe` 2
      appMessages st3 `shouldBe` [UserMsg "First instruction", UserMsg "Second instruction"]
      appStatus st3 `shouldBe` StatusThinking

  describe "3. Custom Event Handlers (handleCustomAppEvent)" $ do
    it "TokenStreamed accumulates tokens and sets StatusStreaming" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
          st1 = handleCustomAppEvent (TokenStreamed "Hello ") st0
          st2 = handleCustomAppEvent (TokenStreamed "world!") st1
      appStreamingText st2 `shouldBe` "Hello world!"
      appStatus st2 `shouldBe` StatusStreaming
      appStatusMessage st2 `shouldBe` "Streaming response..."

    it "ToolStarted appends a tool log entry with StatusRunningTool" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
          args = Aeson.object ["filePath" Aeson..= ("src/Main.hs" :: Text)]
          st1 = handleCustomAppEvent (ToolStarted "viewFile" args) st0
      case appToolLogs st1 of
        [logEntry] -> do
          tleToolName logEntry `shouldBe` "viewFile"
          tleArguments logEntry `shouldBe` args
          tleResult logEntry `shouldBe` Nothing
          appStatus st1 `shouldBe` StatusRunningTool "viewFile"
          appStatusMessage st1 `shouldBe` "Executing tool: viewFile"
        _ -> expectationFailure "Expected exactly one tool log entry"

    it "ToolFinished updates the matching log entry and extracts diff if present" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
          args = Aeson.object ["filePath" Aeson..= ("src/Main.hs" :: Text)]
          st1 = handleCustomAppEvent (ToolStarted "editFile" args) st0
          diffSnippet = "--- a/src/Main.hs\n+++ b/src/Main.hs\n@@ -1 +1 @@\n-old\n+new\n"
          res = Right (Aeson.object ["diff" Aeson..= diffSnippet, "replaced" Aeson..= (1 :: Int)])
          st2 = handleCustomAppEvent (ToolFinished "editFile" res) st1
      case appToolLogs st2 of
        [logEntry] -> do
          tleResult logEntry `shouldBe` Just res
          appStatus st2 `shouldBe` StatusThinking
          case appDiffState st2 of
            Nothing -> expectationFailure "Expected VisualDiffState to be present"
            Just ds -> do
              vdsContent ds `shouldBe` diffSnippet
              vdsFileName ds `shouldBe` Just "editFile"
        _ -> expectationFailure "Expected exactly one tool log entry"

    it "ToolFinished handles error tool results gracefully" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
          st1 = handleCustomAppEvent (ToolStarted "runCommand" (Aeson.object [])) st0
          res = Left "Command timed out after 5000ms"
          st2 = handleCustomAppEvent (ToolFinished "runCommand" res) st1
      case appToolLogs st2 of
        [logEntry] -> do
          tleResult logEntry `shouldBe` Just res
          appStatus st2 `shouldBe` StatusThinking
        _ -> expectationFailure "Expected exactly one tool log entry"

    it "DiffUpdated updates visual diff state explicitly" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
          diffText = "@@ -10,3 +10,4 @@\n-lineA\n+lineB\n+lineC"
          st1 = handleCustomAppEvent (DiffUpdated diffText) st0
      case appDiffState st1 of
        Nothing -> expectationFailure "Expected diff state to be updated"
        Just ds -> do
          vdsContent ds `shouldBe` diffText
          vdsFileName ds `shouldBe` Nothing

    it "ErrorOccurred records error message and sets StatusError" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
          st1 = handleCustomAppEvent (ErrorOccurred "Rate limit exceeded") st0
      appStatus st1 `shouldBe` StatusError "Rate limit exceeded"
      appStatusMessage st1 `shouldBe` "Error: Rate limit exceeded"
      appMessages st1 `shouldBe` [AssistantMsg "[Error] Rate limit exceeded" []]

    it "AgentStateChanged updates status message without modifying status enum" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
          st1 = handleCustomAppEvent (AgentStateChanged "Indexing repository...") st0
      appStatus st1 `shouldBe` StatusIdle
      appStatusMessage st1 `shouldBe` "Indexing repository..."

    it "TurnCompleted commits streaming buffer to message history and resets buffer" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
          st1 = handleCustomAppEvent (TokenStreamed "I have completed your request.") st0
          st2 = handleCustomAppEvent TurnCompleted st1
      appStreamingText st2 `shouldBe` ""
      appMessages st2 `shouldBe` [AssistantMsg "I have completed your request." []]
      appStatus st2 `shouldBe` StatusIdle
      appStatusMessage st2 `shouldBe` "Turn completed. Ready for input."
      amTurnCount (appMetrics st2) `shouldBe` 1

    it "simulates a complete multi-event agent lifecycle in pure state" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
          st1 = submitPromptPure "Add a new endpoint to src/API.hs" st0
          st2 = handleCustomAppEvent (AgentStateChanged "Inspecting workspace...") st1
          st3 = handleCustomAppEvent (ToolStarted "viewFile" (Aeson.object ["filePath" Aeson..= ("src/API.hs" :: Text)])) st2
          st4 = handleCustomAppEvent (ToolFinished "viewFile" (Right (Aeson.object ["lines" Aeson..= (50 :: Int)]))) st3
          st5 = handleCustomAppEvent (ToolStarted "editFile" (Aeson.object ["filePath" Aeson..= ("src/API.hs" :: Text)])) st4
          diffText = "--- a/src/API.hs\n+++ b/src/API.hs\n+endpoint :: Handler\n"
          st6 = handleCustomAppEvent (ToolFinished "editFile" (Right (Aeson.object ["diff" Aeson..= diffText]))) st5
          st7 = handleCustomAppEvent (TokenStreamed "I added the endpoint successfully.") st6
          st8 = handleCustomAppEvent TurnCompleted st7

      appMessages st8 `shouldBe`
        [ UserMsg "Add a new endpoint to src/API.hs"
        , AssistantMsg "I added the endpoint successfully." []
        ]
      length (appToolLogs st8) `shouldBe` 2
      appStatus st8 `shouldBe` StatusIdle
      amTurnCount (appMetrics st8) `shouldBe` 1
      appDiffState st8 `shouldBe` Just (VisualDiffState diffText (Just "editFile"))

  describe "4. Pure Keyboard & Vty Event Handling (handleVtyEvent / handleVtyEventPure)" $ do
    it "handles Ctrl+C and Esc as ActionQuit" $ do
      let st = initialAppState defaultTUIConfig Nothing
          (_, act1) = handleVtyEvent (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl]) st
          (_, act2) = handleVtyEvent (Vty.EvKey Vty.KEsc []) st
      act1 `shouldBe` Just ActionQuit
      act2 `shouldBe` Just ActionQuit

    it "cycles focus with Tab and BackTab" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
          (st1, act1) = handleVtyEvent (Vty.EvKey (Vty.KChar '\t') []) st0
          (st2, act2) = handleVtyEvent (Vty.EvKey (Vty.KChar '\t') []) st1
          (st3, act3) = handleVtyEvent (Vty.EvKey Vty.KBackTab []) st2
      act1 `shouldBe` Just ActionFocusNext
      focusGetCurrent (appFocusRing st1) `shouldBe` Just ViewportChat
      act2 `shouldBe` Just ActionFocusNext
      focusGetCurrent (appFocusRing st2) `shouldBe` Just ViewportDiff
      act3 `shouldBe` Just ActionFocusPrev
      focusGetCurrent (appFocusRing st3) `shouldBe` Just ViewportChat

    it "direct focus navigation with Ctrl+P, Ctrl+H, Ctrl+D, Ctrl+L" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
          (st1, act1) = handleVtyEvent (Vty.EvKey (Vty.KChar 'h') [Vty.MCtrl]) st0
          (st2, act2) = handleVtyEvent (Vty.EvKey (Vty.KChar 'd') [Vty.MCtrl]) st1
          (st3, act3) = handleVtyEvent (Vty.EvKey (Vty.KChar 'l') [Vty.MCtrl]) st2
          (st4, act4) = handleVtyEvent (Vty.EvKey (Vty.KChar 'p') [Vty.MCtrl]) st3
      act1 `shouldBe` Just (ActionFocusResource ViewportChat)
      focusGetCurrent (appFocusRing st1) `shouldBe` Just ViewportChat
      act2 `shouldBe` Just (ActionFocusResource ViewportDiff)
      focusGetCurrent (appFocusRing st2) `shouldBe` Just ViewportDiff
      act3 `shouldBe` Just (ActionFocusResource ViewportToolLog)
      focusGetCurrent (appFocusRing st3) `shouldBe` Just ViewportToolLog
      act4 `shouldBe` Just (ActionFocusResource EditorPrompt)
      focusGetCurrent (appFocusRing st4) `shouldBe` Just EditorPrompt

    it "clears visual diff with Ctrl+X" $ do
      let st0 = (initialAppState defaultTUIConfig Nothing)
                  { appDiffState = Just (VisualDiffState "diff content" Nothing) }
          (st1, act) = handleVtyEvent (Vty.EvKey (Vty.KChar 'x') [Vty.MCtrl]) st0
      act `shouldBe` Just ActionClearDiff
      appDiffState st1 `shouldBe` Nothing

    it "submits prompt on Enter when focused on EditorPrompt" $ do
      let st0 = (initialAppState defaultTUIConfig Nothing)
                  { appEditor = editorText EditorPrompt (Just 3) "Run tests" }
          (st1, act) = handleVtyEvent (Vty.EvKey Vty.KEnter []) st0
      act `shouldBe` Just (ActionSubmitPrompt "Run tests")
      appMessages st1 `shouldBe` [UserMsg "Run tests"]
      appStatus st1 `shouldBe` StatusThinking

    it "ignores empty prompt on Enter" $ do
      let st0 = (initialAppState defaultTUIConfig Nothing)
                  { appEditor = editorText EditorPrompt (Just 3) "   " }
          (st1, act) = handleVtyEvent (Vty.EvKey Vty.KEnter []) st0
      act `shouldBe` Just ActionNone
      appMessages st1 `shouldBe` []

    it "dispatches scrolling actions when viewport is focused" $ do
      let (st0, _) = handleVtyEvent (Vty.EvKey (Vty.KChar 'h') [Vty.MCtrl]) (initialAppState defaultTUIConfig Nothing)
          (_, actUp) = handleVtyEvent (Vty.EvKey Vty.KUp []) st0
          (_, actDown) = handleVtyEvent (Vty.EvKey Vty.KDown []) st0
          (_, actPgUp) = handleVtyEvent (Vty.EvKey Vty.KPageUp []) st0
          (_, actPgDown) = handleVtyEvent (Vty.EvKey Vty.KPageDown []) st0
      actUp `shouldBe` Just (ActionScrollUp ViewportChat 1)
      actDown `shouldBe` Just (ActionScrollDown ViewportChat 1)
      actPgUp `shouldBe` Just (ActionScrollUp ViewportChat 10)
      actPgDown `shouldBe` Just (ActionScrollDown ViewportChat 10)

  describe "5. Visual Diff Computation & Extraction" $ do
    it "computes unified diff accurately" $ do
      let oldCode = "line1\nline2\nline3\n"
          newCode = "line1\nmodified2\nline3\nline4\n"
          diff = computeDiffUnified oldCode newCode
      T.isInfixOf "  line1" diff `shouldBe` True
      T.isInfixOf "- line2" diff `shouldBe` True
      T.isInfixOf "+ modified2" diff `shouldBe` True
      T.isInfixOf "+ line4" diff `shouldBe` True

    it "extracts diff from 'diff' field in JSON tool result" $ do
      let res = Right (Aeson.object ["diff" Aeson..= ("+added line" :: Text)])
      extractDiffFromToolResult res `shouldBe` Just "+added line"

    it "extracts diff from 'diffSnippet' field in JSON tool result" $ do
      let res = Right (Aeson.object ["diffSnippet" Aeson..= ("-removed line" :: Text)])
      extractDiffFromToolResult res `shouldBe` Just "-removed line"

    it "extracts diff from 'efrDiffSnippet' field in EditFileResult JSON tool result" $ do
      let res = Right (Aeson.object ["efrDiffSnippet" Aeson..= ("@@ -1 +1 @@\n-old\n+new" :: Text), "efrPath" Aeson..= ("src/Main.hs" :: Text)])
      extractDiffFromToolResult res `shouldBe` Just "@@ -1 +1 @@\n-old\n+new"

    it "extracts diff from 'diff_snippet' field in JSON tool result" $ do
      let res = Right (Aeson.object ["diff_snippet" Aeson..= ("+patched line" :: Text)])
      extractDiffFromToolResult res `shouldBe` Just "+patched line"

    it "returns Nothing when tool result has no diff" $ do
      let res1 = Right (Aeson.object ["status" Aeson..= ("ok" :: Text)])
          res2 = Left "Error running tool"
      extractDiffFromToolResult res1 `shouldBe` Nothing
      extractDiffFromToolResult res2 `shouldBe` Nothing

  describe "6. UI Rendering & Theme Attributes" $ do
    it "renders UI widgets without exceptions across various states" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
          st1 = submitPromptPure "Create a server" st0
          st2 = handleCustomAppEvent (TokenStreamed "Streaming tokens...") st1
          st3 = handleCustomAppEvent (ToolStarted "runCommand" (Aeson.object ["cmd" Aeson..= ("cabal build" :: Text)])) st2
          st4 = handleCustomAppEvent (DiffUpdated "--- old\n+++ new\n@@ -1 +1 @@\n-a\n+b\n") st3
          widgets = drawUI st4
      length widgets `shouldBe` 1

    it "tuiAttrMap contains all theme attributes" $ do
      let st = initialAppState defaultTUIConfig Nothing
          _attrMap = tuiAttrMap st
      -- Verification that attributes are defined and do not crash
      diffAddedAttr `shouldBe` Brick.attrName "diffAdded"
      diffRemovedAttr `shouldBe` Brick.attrName "diffRemoved"
      diffHeaderAttr `shouldBe` Brick.attrName "diffHeader"
      diffContextAttr `shouldBe` Brick.attrName "diffContext"
      statusIdleAttr `shouldBe` Brick.attrName "statusIdle"
      statusThinkingAttr `shouldBe` Brick.attrName "statusThinking"
      statusStreamingAttr `shouldBe` Brick.attrName "statusStreaming"
      statusErrorAttr `shouldBe` Brick.attrName "statusError"
      toolLogAttr `shouldBe` Brick.attrName "toolLog"
      headerAttr `shouldBe` Brick.attrName "header"
      borderAttr `shouldBe` Brick.attrName "border"

  describe "7. Session Resume & State Hydration (TUI-004)" $ do
    it "hydrates empty event list without altering initial AppState" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
          stHydrated = hydrateAppStateFromEvents [] st0
      appMessages stHydrated `shouldBe` []
      appToolLogs stHydrated `shouldBe` []
      appDiffState stHydrated `shouldBe` Nothing
      appStatusMessage stHydrated `shouldBe` "Ready"

    it "hydrates multi-turn conversation with UserMsg, AssistantMsg, and ToolMsg into appMessages" $ do
      let diffText = "--- a/src/App.hs\n+++ b/src/App.hs\n@@ -1 +1 @@\n-old\n+new"
          events =
            [ TurnStarted "turn-1"
            , JournalUserMsg "Inspect project structure"
            , ModelTurn "I will list the directory." [ToolCall "call-1" "listDir" "{\"directoryPath\":\".\"}"]
            , ToolInvoked "call-1" "listDir" (Aeson.object ["directoryPath" Aeson..= ("." :: Text)])
            , ToolCompleted "call-1" (Right (Aeson.object ["entries" Aeson..= (["src", "app"] :: [Text])]))
            , ModelTurn "Directory scan complete." []
            , MetricsReported (ModelMetrics 120 45 165 150.0 "deepseek-chat")
            , TurnFinished "turn-1"
            , TurnStarted "turn-2"
            , JournalUserMsg "Edit src/App.hs"
            , ModelTurn "Applying edit..." [ToolCall "call-2" "editFile" "{\"targetFile\":\"src/App.hs\"}"]
            , ToolInvoked "call-2" "editFile" (Aeson.object ["targetFile" Aeson..= ("src/App.hs" :: Text)])
            , ToolCompleted "call-2" (Right (Aeson.object ["efrDiffSnippet" Aeson..= diffText, "efrPath" Aeson..= ("src/App.hs" :: Text)]))
            , ModelTurn "Edit applied successfully." []
            , MetricsReported (ModelMetrics 200 80 280 210.0 "deepseek-chat")
            , TurnFinished "turn-2"
            ]
          st0 = initialAppState defaultTUIConfig Nothing
          stHydrated = hydrateAppStateFromEvents events st0

      length (appMessages stHydrated) `shouldBe` 8
      appMessages stHydrated `shouldBe`
        [ UserMsg "Inspect project structure"
        , AssistantMsg "I will list the directory." [ToolCall "call-1" "listDir" "{\"directoryPath\":\".\"}"]
        , ToolMsg "call-1" "{\"entries\":[\"src\",\"app\"]}"
        , AssistantMsg "Directory scan complete." []
        , UserMsg "Edit src/App.hs"
        , AssistantMsg "Applying edit..." [ToolCall "call-2" "editFile" "{\"targetFile\":\"src/App.hs\"}"]
        , ToolMsg "call-2" "{\"efrDiffSnippet\":\"--- a/src/App.hs\\n+++ b/src/App.hs\\n@@ -1 +1 @@\\n-old\\n+new\",\"efrPath\":\"src/App.hs\"}"
        , AssistantMsg "Edit applied successfully." []
        ]

      -- Check tool logs
      case appToolLogs stHydrated of
        [l1, l2] -> do
          tleToolName l1 `shouldBe` "listDir"
          tleToolName l2 `shouldBe` "editFile"
        _ -> expectationFailure "Expected exactly 2 tool logs"

      -- Check diff state
      case appDiffState stHydrated of
        Nothing -> expectationFailure "Expected visual diff state to be hydrated"
        Just ds -> do
          vdsContent ds `shouldBe` diffText
          vdsFileName ds `shouldBe` Just "editFile"

      -- Check metrics
      let m = appMetrics stHydrated
      amPromptTokens m `shouldBe` 320
      amCompletionTokens m `shouldBe` 125
      amTotalTokens m `shouldBe` 445
      amTurnCount m `shouldBe` 2
      amLatencyMs m `shouldBe` 360.0

      -- Check status message
      T.isInfixOf "Session resumed" (appStatusMessage stHydrated) `shouldBe` True

    it "initialAppStateWithEvents initializes hydrated state directly" $ do
      let events =
            [ TurnStarted "turn-1"
            , JournalUserMsg "Ping"
            , ModelTurn "Pong" []
            , TurnFinished "turn-1"
            ]
          cfg = defaultTUIConfig
          st = initialAppStateWithEvents cfg Nothing events
      length (appMessages st) `shouldBe` 2
      appMessages st `shouldBe` [UserMsg "Ping", AssistantMsg "Pong" []]
      amTurnCount (appMetrics st) `shouldBe` 1

    it "subsequent prompt submission appends cleanly without duplicating historical turns" $ do
      let events =
            [ TurnStarted "turn-1"
            , JournalUserMsg "Initial prompt"
            , ModelTurn "Initial answer" []
            , TurnFinished "turn-1"
            ]
          st0 = initialAppStateWithEvents defaultTUIConfig Nothing events
          st1 = submitPromptPure "Follow-up question" st0
      length (appMessages st1) `shouldBe` 3
      appMessages st1 `shouldBe`
        [ UserMsg "Initial prompt"
        , AssistantMsg "Initial answer" []
        , UserMsg "Follow-up question"
        ]
      appStatus st1 `shouldBe` StatusThinking
