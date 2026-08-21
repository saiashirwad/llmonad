{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Adversarial Stress & Edge Case Test Suite for Brick + Vty TUI (Milestone 4 / R4).
module LLMonad.TUIAdversarialSpec (spec) where

import Brick.BChan (newBChan, readBChan)
import Brick.Focus (focusGetCurrent, focusSetCurrent)
import Brick.Widgets.Edit (getEditContents)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import LLMonad
import Test.Hspec

spec :: Spec
spec = describe "Brick + Vty Interactive TUI Adversarial Suite (Milestone 4)" $ do

  describe "1. Rapid Token Streaming & Multiline Prompt Submissions" $ do
    it "handles 10,000 rapid micro-token streaming chunks without loss or corruption" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
      let chunks = [T.singleton c | c <- take 10000 (cycle ['a'..'z'])]
      let finalSt = foldl (\s chunk -> handleCustomAppEvent (TokenStreamed chunk) s) st0 chunks
      appStatus finalSt `shouldBe` StatusStreaming
      appStatusMessage finalSt `shouldBe` "Streaming response..."
      T.length (appStreamingText finalSt) `shouldBe` 10000
      T.take 26 (appStreamingText finalSt) `shouldBe` "abcdefghijklmnopqrstuvwxyz"

    it "handles unicode, emoji, and ANSI escape sequences in token streaming" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
      let unicodeChunks =
            [ "🚀 Initializing..."
            , "\ESC[31mRed Text\ESC[0m"
            , "日本語テキスト"
            , "مرحبا بالعالم"
            , "\n\t\r\0 Special chars"
            ]
      let finalSt = foldl (\s chunk -> handleCustomAppEvent (TokenStreamed chunk) s) st0 unicodeChunks
      appStreamingText finalSt `shouldBe` T.concat unicodeChunks
      appStatus finalSt `shouldBe` StatusStreaming

    it "handles empty token chunks gracefully without altering state incorrectly" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
      let st1 = handleCustomAppEvent (TokenStreamed "") st0
      appStreamingText st1 `shouldBe` ""
      appStatus st1 `shouldBe` StatusStreaming
      let st2 = handleCustomAppEvent TurnCompleted st1
      appStreamingText st2 `shouldBe` ""
      appMessages st2 `shouldBe` [] -- No empty assistant message committed
      appStatus st2 `shouldBe` StatusIdle

    it "submits multiline prompts with complex whitespace and preserves structure" $ do
      let multilinePrompt = "def solve():\n    # First step\n    x = 10\n    return x * 2\n"
      let st0 = initialAppState defaultTUIConfig Nothing
      let st1 = submitPromptPure multilinePrompt st0
      appMessages st1 `shouldBe` [UserMsg multilinePrompt]
      appStatus st1 `shouldBe` StatusThinking
      getEditContents (appEditor st1) `shouldBe` [""]

    it "simulates 50 rapid sequential turns and verifies turn count and message history" $ do
      let runTurn (s, turnIdx) =
            let p = "Turn " <> T.pack (show (turnIdx :: Int))
                s1 = submitPromptPure p s
                s2 = handleCustomAppEvent (TokenStreamed ("Response " <> T.pack (show (turnIdx :: Int)))) s1
                s3 = handleCustomAppEvent TurnCompleted s2
            in (s3, turnIdx + 1)

      let st0 = initialAppState defaultTUIConfig Nothing
      let (finalSt, finalIdx) = iterate runTurn (st0, 1 :: Int) !! 50
      finalIdx `shouldBe` (51 :: Int)
      amTurnCount (appMetrics finalSt) `shouldBe` 50
      length (appMessages finalSt) `shouldBe` 100
      appStatus finalSt `shouldBe` StatusIdle

    it "asynchronously drains events from runAgentWorker channel into AppState" $ do
      chan <- newBChan 50
      let cfg = defaultTUIConfig
      runAgentWorker chan cfg "Refactor module X"

      -- Drain all 7 events produced by worker
      ev1 <- readBChan chan
      ev2 <- readBChan chan
      ev3 <- readBChan chan
      ev4 <- readBChan chan
      ev5 <- readBChan chan
      ev6 <- readBChan chan
      ev7 <- readBChan chan

      let st0 = initialAppState cfg (Just chan)
      let events = [ev1, ev2, ev3, ev4, ev5, ev6, ev7]
      let finalSt = foldl (flip handleCustomAppEvent) st0 events

      appStatus finalSt `shouldBe` StatusIdle
      amTurnCount (appMetrics finalSt) `shouldBe` 1
      length (appToolLogs finalSt) `shouldBe` 1
      length (appMessages finalSt) `shouldBe` 1

  describe "2. Large Unified Diff Parsing & Edge Cases" $ do
    it "computes unified diff for large 2,000-line text inputs" $ do
      let oldLines = ["line " <> T.pack (show i) | i <- [1..2000 :: Int]]
      let newLines = ["line " <> T.pack (show i) <> (if i `mod` 10 == 0 then " modified" else "") | i <- [1..2000 :: Int]]
      let oldTxt = T.unlines oldLines
      let newTxt = T.unlines newLines
      let diff = computeDiffUnified oldTxt newTxt
      T.length diff `shouldSatisfy` (> 0)
      T.isInfixOf "+ line 10 modified" diff `shouldBe` True
      T.isInfixOf "- line 10" diff `shouldBe` True
      T.isInfixOf "  line 1" diff `shouldBe` True

    it "handles empty, identical, and completely disjoint diff inputs" $ do
      -- Empty
      computeDiffUnified "" "" `shouldBe` ""

      -- Identical
      let same = "single line\nsecond line\n"
      let diffSame = computeDiffUnified same same
      diffSame `shouldBe` "  single line\n  second line\n"

      -- Disjoint
      let diffDisjoint = computeDiffUnified "alpha\n" "beta\n"
      diffDisjoint `shouldBe` "- alpha\n+ beta\n"

    it "computes unified diff with unicode and missing trailing newlines" $ do
      let oldTxt = "こんにちは\n世界"
      let newTxt = "こんにちは\n Haskell\n世界"
      let diff = computeDiffUnified oldTxt newTxt
      T.isInfixOf "+  Haskell" diff `shouldBe` True
      T.isInfixOf "  こんにちは" diff `shouldBe` True

    it "extracts diff safely from non-string and malformed JSON tool results" $ do
      -- Numeric diff
      let resNum = Right (Aeson.object ["diff" Aeson..= (12345 :: Int)])
      extractDiffFromToolResult resNum `shouldBe` Nothing

      -- Boolean diffSnippet
      let resBool = Right (Aeson.object ["diffSnippet" Aeson..= True])
      extractDiffFromToolResult resBool `shouldBe` Nothing

      -- Null diff
      let resNull = Right (Aeson.object ["diff" Aeson..= Aeson.Null])
      extractDiffFromToolResult resNull `shouldBe` Nothing

      -- Array tool result
      let resArr = Right (Aeson.Array mempty)
      extractDiffFromToolResult resArr `shouldBe` Nothing

      -- String tool result (not an object)
      let resStr = Right (Aeson.String "diff content directly")
      extractDiffFromToolResult resStr `shouldBe` Nothing

    it "renders diff with pathological and malformed lines without throwing" $ do
      let pathologicalDiff = T.unlines
            [ "--- a/file.txt"
            , "+++ b/file.txt"
            , "@@ -1,5 +1,6 @@"
            , "+added line"
            , "-removed line"
            , "  context line"
            , "+++"
            , "---"
            , "@@ invalid header @@"
            , "plain text without prefix"
            , ""
            , "   indented context"
            , "+ + nested plus"
            , "- - nested minus"
            ]
      let st = (initialAppState defaultTUIConfig Nothing)
            { appDiffState = Just (VisualDiffState pathologicalDiff (Just "file.txt")) }
      let widgets = drawUI st
      length widgets `shouldBe` 1

  describe "3. Tool Failure Events, Error Transitions & Interleaved Logging" $ do
    it "handles sequential execution of multiple tools with the same name" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
          st1 = handleCustomAppEvent (ToolStarted "viewFile" (Aeson.object ["path" Aeson..= ("a.hs" :: Text)])) st0
          st2 = handleCustomAppEvent (ToolFinished "viewFile" (Right (Aeson.object ["content" Aeson..= ("module A" :: Text)]))) st1
          st3 = handleCustomAppEvent (ToolStarted "viewFile" (Aeson.object ["path" Aeson..= ("b.hs" :: Text)])) st2
          st4 = handleCustomAppEvent (ToolFinished "viewFile" (Left "File not found")) st3

      length (appToolLogs st4) `shouldBe` 2
      case appToolLogs st4 of
        [e1, e2] -> do
          tleToolName e1 `shouldBe` "viewFile"
          tleResult e1 `shouldSatisfy` (\case Just (Right _) -> True; _ -> False)
          tleToolName e2 `shouldBe` "viewFile"
          tleResult e2 `shouldSatisfy` (\case Just (Left _) -> True; _ -> False)
        _ -> expectationFailure "Expected exactly 2 tool logs"

    it "handles multiple consecutive ErrorOccurred events during active tool execution" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
          st1 = handleCustomAppEvent (ToolStarted "runCommand" (Aeson.object ["cmd" Aeson..= ("test" :: Text)])) st0
          st2 = handleCustomAppEvent (ErrorOccurred "Network socket timeout") st1
          st3 = handleCustomAppEvent (ErrorOccurred "Process killed by OOM killer") st2

      appStatus st3 `shouldBe` StatusError "Process killed by OOM killer"
      appStatusMessage st3 `shouldBe` "Error: Process killed by OOM killer"
      length (appMessages st3) `shouldBe` 2
      appMessages st3 `shouldBe`
        [ AssistantMsg "[Error] Network socket timeout" []
        , AssistantMsg "[Error] Process killed by OOM killer" []
        ]

    it "recovers cleanly from StatusError on next user prompt submission" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
          st1 = handleCustomAppEvent (ErrorOccurred "Fatal failure") st0
          st2 = submitPromptPure "Try again with different parameters" st1

      appStatus st2 `shouldBe` StatusThinking
      appStatusMessage st2 `shouldBe` "Processing user prompt..."
      length (appMessages st2) `shouldBe` 2

  describe "4. Scrolling, Focus Cycling & Vty Navigation Resilience" $ do
    it "cycles focus ring in full round-trips forward and backward" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
      focusGetCurrent (appFocusRing st0) `shouldBe` Just EditorPrompt

      -- Forward cycle
      let (st1, a1) = handleVtyEventPure (Vty.EvKey (Vty.KChar '\t') []) st0
      a1 `shouldBe` Just ActionFocusNext
      focusGetCurrent (appFocusRing st1) `shouldBe` Just ViewportChat

      let (st2, a2) = handleVtyEventPure (Vty.EvKey (Vty.KChar '\t') []) st1
      a2 `shouldBe` Just ActionFocusNext
      focusGetCurrent (appFocusRing st2) `shouldBe` Just ViewportDiff

      let (st3, a3) = handleVtyEventPure (Vty.EvKey (Vty.KChar '\t') []) st2
      a3 `shouldBe` Just ActionFocusNext
      focusGetCurrent (appFocusRing st3) `shouldBe` Just ViewportToolLog

      let (st4, a4) = handleVtyEventPure (Vty.EvKey (Vty.KChar '\t') []) st3
      a4 `shouldBe` Just ActionFocusNext
      focusGetCurrent (appFocusRing st4) `shouldBe` Just EditorPrompt

      -- Backward cycle
      let (st5, a5) = handleVtyEventPure (Vty.EvKey Vty.KBackTab []) st4
      a5 `shouldBe` Just ActionFocusPrev
      focusGetCurrent (appFocusRing st5) `shouldBe` Just ViewportToolLog

    it "does not trigger scrolling actions when EditorPrompt is focused" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
      focusGetCurrent (appFocusRing st0) `shouldBe` Just EditorPrompt
      let (_, aUp) = handleVtyEventPure (Vty.EvKey Vty.KUp []) st0
      let (_, aDown) = handleVtyEventPure (Vty.EvKey Vty.KDown []) st0
      let (_, aPgUp) = handleVtyEventPure (Vty.EvKey Vty.KPageUp []) st0
      let (_, aPgDown) = handleVtyEventPure (Vty.EvKey Vty.KPageDown []) st0
      aUp `shouldBe` Just ActionNone
      aDown `shouldBe` Just ActionNone
      aPgUp `shouldBe` Just ActionNone
      aPgDown `shouldBe` Just ActionNone

    it "triggers scrolling actions for all non-editor viewports" $ do
      let testViewport vp key expectedAct = do
            let st = (initialAppState defaultTUIConfig Nothing)
                  { appFocusRing = focusSetCurrent vp (appFocusRing (initialAppState defaultTUIConfig Nothing)) }
            let (_, act) = handleVtyEventPure (Vty.EvKey key []) st
            act `shouldBe` Just expectedAct

      testViewport ViewportChat Vty.KUp (ActionScrollUp ViewportChat 1)
      testViewport ViewportChat Vty.KDown (ActionScrollDown ViewportChat 1)
      testViewport ViewportChat Vty.KPageUp (ActionScrollUp ViewportChat 10)
      testViewport ViewportChat Vty.KPageDown (ActionScrollDown ViewportChat 10)

      testViewport ViewportDiff Vty.KUp (ActionScrollUp ViewportDiff 1)
      testViewport ViewportDiff Vty.KDown (ActionScrollDown ViewportDiff 1)
      testViewport ViewportDiff Vty.KPageUp (ActionScrollUp ViewportDiff 10)
      testViewport ViewportDiff Vty.KPageDown (ActionScrollDown ViewportDiff 10)

      testViewport ViewportToolLog Vty.KUp (ActionScrollUp ViewportToolLog 1)
      testViewport ViewportToolLog Vty.KDown (ActionScrollDown ViewportToolLog 1)
      testViewport ViewportToolLog Vty.KPageUp (ActionScrollUp ViewportToolLog 10)
      testViewport ViewportToolLog Vty.KPageDown (ActionScrollDown ViewportToolLog 10)

    it "handles arbitrary unhandled Vty keys safely" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
      let unhandledEvents =
            [ Vty.EvKey (Vty.KChar 'z') []
            , Vty.EvKey (Vty.KChar 'F') [Vty.MMeta]
            , Vty.EvKey Vty.KHome []
            , Vty.EvKey Vty.KEnd []
            , Vty.EvResize 100 50
            ]
      mapM_ (\ev -> do
        let (_, act) = handleVtyEventPure ev st0
        act `shouldBe` Just ActionNone) unhandledEvents

    it "navigates directly to any viewport using Ctrl shortcuts from any initial state" $ do
      let st0 = initialAppState defaultTUIConfig Nothing
      let (stChat, actChat) = handleVtyEventPure (Vty.EvKey (Vty.KChar 'h') [Vty.MCtrl]) st0
      actChat `shouldBe` Just (ActionFocusResource ViewportChat)
      focusGetCurrent (appFocusRing stChat) `shouldBe` Just ViewportChat

      let (stDiff, actDiff) = handleVtyEventPure (Vty.EvKey (Vty.KChar 'd') [Vty.MCtrl]) stChat
      actDiff `shouldBe` Just (ActionFocusResource ViewportDiff)
      focusGetCurrent (appFocusRing stDiff) `shouldBe` Just ViewportDiff

      let (stLog, actLog) = handleVtyEventPure (Vty.EvKey (Vty.KChar 'l') [Vty.MCtrl]) stDiff
      actLog `shouldBe` Just (ActionFocusResource ViewportToolLog)
      focusGetCurrent (appFocusRing stLog) `shouldBe` Just ViewportToolLog

      let (stPrompt, actPrompt) = handleVtyEventPure (Vty.EvKey (Vty.KChar 'p') [Vty.MCtrl]) stLog
      actPrompt `shouldBe` Just (ActionFocusResource EditorPrompt)
      focusGetCurrent (appFocusRing stPrompt) `shouldBe` Just EditorPrompt

  describe "5. Extreme UI Rendering & Layout Stress" $ do
    it "renders UI across all agent status variants cleanly" $ do
      let baseSt = initialAppState defaultTUIConfig Nothing
      let statuses =
            [ StatusIdle
            , StatusThinking
            , StatusStreaming
            , StatusRunningTool "editFile"
            , StatusError "Connection reset by peer"
            , StatusCompleted
            ]
      mapM_ (\stat -> do
        let st = baseSt { appStatus = stat }
        let widgets = drawUI st
        length widgets `shouldBe` 1) statuses

    it "renders UI with 500 chat messages, 500 tool logs, and large diff" $ do
      let msgs = [if even i then UserMsg ("Question " <> T.pack (show i)) else AssistantMsg ("Answer " <> T.pack (show i)) [] | i <- [1..500 :: Int]]
      let logs = [ToolLogEntry ("tool_" <> T.pack (show i)) (Aeson.object ["arg" Aeson..= i]) (Just (Right (Aeson.object ["res" Aeson..= i]))) "done" | i <- [1..500 :: Int]]
      let diff = T.unlines ["+ line " <> T.pack (show i) | i <- [1..1000 :: Int]]
      let st = (initialAppState defaultTUIConfig Nothing)
            { appMessages = msgs
            , appToolLogs = logs
            , appDiffState = Just (VisualDiffState diff (Just "massive.hs"))
            , appStreamingText = "Current active streaming output..."
            , appStatus = StatusStreaming
            , appMetrics = AppMetrics 50000 120000 170000 450.5 500
            }
      let widgets = drawUI st
      length widgets `shouldBe` 1
