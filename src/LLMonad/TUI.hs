{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Interactive Brick + Vty Terminal User Interface for LLMonad Coding Agent.
module LLMonad.TUI
  ( -- * Types
    ResourceName (..)
  , AgentStatus (..)
  , CustomAppEvent (..)
  , ToolLogEntry (..)
  , VisualDiffState (..)
  , AppMetrics (..)
  , TUIAction (..)
  , AppState (..)
  , TUIConfig (..)
  , defaultTUIConfig
  , initialAppState

    -- * Pure Event Handlers & State Transitions
  , handleCustomAppEvent
  , handleVtyEvent
  , handleVtyEventPure
  , submitPromptPure
  , computeDiffUnified
  , extractDiffFromToolResult

    -- * UI Rendering & Attributes
  , drawUI
  , tuiAttrMap
  , diffAddedAttr
  , diffRemovedAttr
  , diffHeaderAttr
  , diffContextAttr
  , statusIdleAttr
  , statusThinkingAttr
  , statusStreamingAttr
  , statusErrorAttr
  , toolLogAttr
  , headerAttr
  , borderAttr

    -- * Brick Application & Runners
  , tuiApp
  , runTUIApp
  , runTUIAppWithConfig
  , runAgentWorker
  ) where

import Brick
  ( App (..)
  , AttrMap
  , AttrName
  , BrickEvent (..)
  , EventM
  , Padding (..)
  , ViewportType (..)
  , Widget
  , attrMap
  , attrName
  , customMain
  , emptyWidget
  , get
  , halt
  , modify
  , nestEventM
  , padBottom
  , padLeft
  , padRight
  , put
  , showFirstCursor
  , txt
  , txtWrap
  , vBox
  , vScrollBy
  , viewport
  , viewportScroll
  , withAttr
  , (<+>)
  , (<=>)
  )
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Focus
  ( FocusRing
  , focusGetCurrent
  , focusNext
  , focusPrev
  , focusRing
  , focusSetCurrent
  )
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Edit
  ( Editor
  , editorText
  , getEditContents
  , handleEditorEvent
  , renderEditor
  )
import Control.Concurrent.Async (Async, async)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Text as AesonText
import qualified Data.Algorithm.Diff as Diff
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Generics (Generic)
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.CrossPlatform as VtyCross
import LLMonad.Journal.Types (ToolResult)
import LLMonad.Types (ChatMessage (..), ToolCall (..))

-- | Brick viewport and widget resource identifiers.
data ResourceName
  = ViewportChat
  | ViewportDiff
  | ViewportToolLog
  | EditorPrompt
  deriving (Show, Eq, Ord)

-- | Current execution status of the agent.
data AgentStatus
  = StatusIdle
  | StatusThinking
  | StatusStreaming
  | StatusRunningTool !Text
  | StatusError !Text
  | StatusCompleted
  deriving (Show, Eq, Generic)

-- | Custom asynchronous events sent to the Brick event loop.
data CustomAppEvent
  = TokenStreamed !Text
  | ToolStarted !Text !Value
  | ToolFinished !Text !ToolResult
  | DiffUpdated !Text
  | ErrorOccurred !Text
  | AgentStateChanged !Text
  | TurnCompleted
  deriving (Show, Eq, Generic)

-- | Log entry for an executed or running tool.
data ToolLogEntry = ToolLogEntry
  { tleToolName   :: !Text
  , tleArguments  :: !Value
  , tleResult     :: !(Maybe ToolResult)
  , tleTimestamp  :: !Text
  } deriving (Show, Eq, Generic)

-- | Visual diff representation state.
data VisualDiffState = VisualDiffState
  { vdsContent  :: !Text
  , vdsFileName :: !(Maybe FilePath)
  } deriving (Show, Eq, Generic)

-- | Execution metrics tracked by the TUI.
data AppMetrics = AppMetrics
  { amPromptTokens     :: !Int
  , amCompletionTokens :: !Int
  , amTotalTokens      :: !Int
  , amLatencyMs        :: !Double
  , amTurnCount        :: !Int
  } deriving (Show, Eq, Generic)

-- | Pure action triggered by user input.
data TUIAction
  = ActionNone
  | ActionSubmitPrompt !Text
  | ActionQuit
  | ActionFocusNext
  | ActionFocusPrev
  | ActionFocusResource !ResourceName
  | ActionScrollUp !ResourceName !Int
  | ActionScrollDown !ResourceName !Int
  | ActionClearDiff
  deriving (Show, Eq, Generic)

-- | Complete TUI application state.
data AppState = AppState
  { appEditor           :: !(Editor Text ResourceName)
  , appMessages         :: ![ChatMessage]
  , appStreamingText    :: !Text
  , appToolLogs         :: ![ToolLogEntry]
  , appDiffState        :: !(Maybe VisualDiffState)
  , appMetrics          :: !AppMetrics
  , appFocusRing        :: !(FocusRing ResourceName)
  , appStatus           :: !AgentStatus
  , appStatusMessage    :: !Text
  , appWorkspacePath    :: !FilePath
  , appModelName        :: !Text
  , appSystemPrompt     :: !(Maybe Text)
  , appSessionFilePath  :: !(Maybe FilePath)
  , appEventChan        :: !(Maybe (BChan CustomAppEvent))
  , appAgentThread      :: !(Maybe (Async ()))
  }

-- | Configuration options for launching the TUI application.
data TUIConfig = TUIConfig
  { cfgWorkspacePath   :: !FilePath
  , cfgModelName       :: !Text
  , cfgSystemPrompt    :: !(Maybe Text)
  , cfgSessionFilePath :: !(Maybe FilePath)
  } deriving (Show, Eq, Generic)

-- | Default TUI configuration.
defaultTUIConfig :: TUIConfig
defaultTUIConfig = TUIConfig
  { cfgWorkspacePath   = "."
  , cfgModelName       = "deepseek-chat"
  , cfgSystemPrompt    = Nothing
  , cfgSessionFilePath = Nothing
  }

-- | Initialize empty 'AppState' with default widgets and focus ring.
initialAppState :: TUIConfig -> Maybe (BChan CustomAppEvent) -> AppState
initialAppState cfg mChan = AppState
  { appEditor          = editorText EditorPrompt (Just 3) ""
  , appMessages        = []
  , appStreamingText   = ""
  , appToolLogs        = []
  , appDiffState       = Nothing
  , appMetrics         = AppMetrics 0 0 0 0.0 0
  , appFocusRing       = focusRing [EditorPrompt, ViewportChat, ViewportDiff, ViewportToolLog]
  , appStatus          = StatusIdle
  , appStatusMessage   = "Ready"
  , appWorkspacePath   = cfgWorkspacePath cfg
  , appModelName       = cfgModelName cfg
  , appSystemPrompt    = cfgSystemPrompt cfg
  , appSessionFilePath = cfgSessionFilePath cfg
  , appEventChan       = mChan
  , appAgentThread     = Nothing
  }

-- ============================================================================
-- Pure Event Handlers
-- ============================================================================

-- | Pure state update from a 'CustomAppEvent'.
handleCustomAppEvent :: CustomAppEvent -> AppState -> AppState
handleCustomAppEvent event st = case event of
  TokenStreamed chunk ->
    st { appStreamingText = appStreamingText st <> chunk
       , appStatus        = StatusStreaming
       , appStatusMessage = "Streaming response..."
       }

  ToolStarted toolName args ->
    let entry = ToolLogEntry
          { tleToolName  = toolName
          , tleArguments = args
          , tleResult    = Nothing
          , tleTimestamp = "active"
          }
    in st { appToolLogs      = appToolLogs st ++ [entry]
          , appStatus        = StatusRunningTool toolName
          , appStatusMessage = "Executing tool: " <> toolName
          }

  ToolFinished toolName res ->
    let updatedLogs = map (updateMatchingLog toolName res) (appToolLogs st)
        newDiff = extractDiffFromToolResult res
        mergedDiff = case newDiff of
          Just d  -> Just (VisualDiffState d (Just (T.unpack toolName)))
          Nothing -> appDiffState st
    in st { appToolLogs      = updatedLogs
          , appStatus        = StatusThinking
          , appStatusMessage = "Tool " <> toolName <> " finished"
          , appDiffState     = mergedDiff
          }

  DiffUpdated diffTxt ->
    st { appDiffState = Just (VisualDiffState diffTxt Nothing) }

  ErrorOccurred errMsg ->
    st { appStatus        = StatusError errMsg
       , appStatusMessage = "Error: " <> errMsg
       , appMessages      = appMessages st ++ [AssistantMsg ("[Error] " <> errMsg) []]
       }

  AgentStateChanged msg ->
    st { appStatusMessage = msg }

  TurnCompleted ->
    let finalStream = appStreamingText st
        curMetrics = appMetrics st
        newMetrics = curMetrics { amTurnCount = amTurnCount curMetrics + 1 }
        newMsgs = if T.null finalStream
                    then appMessages st
                    else appMessages st ++ [AssistantMsg finalStream []]
    in st { appStreamingText = ""
          , appMessages      = newMsgs
          , appStatus        = StatusIdle
          , appStatusMessage = "Turn completed. Ready for input."
          , appMetrics       = newMetrics
          }

-- | Helper to update the last unfinished log entry matching toolName.
updateMatchingLog :: Text -> ToolResult -> ToolLogEntry -> ToolLogEntry
updateMatchingLog name res entry
  | tleToolName entry == name && tleResult entry == Nothing =
      entry { tleResult = Just res }
  | otherwise = entry

-- | Extract a diff snippet if present in tool result.
extractDiffFromToolResult :: ToolResult -> Maybe Text
extractDiffFromToolResult (Right val) = case val of
  Aeson.Object obj -> case KeyMap.lookup "diff" obj of
    Just (Aeson.String d) -> Just d
    _ -> case KeyMap.lookup "diffSnippet" obj of
      Just (Aeson.String d) -> Just d
      _ -> Nothing
  _ -> Nothing
extractDiffFromToolResult (Left _) = Nothing

-- | Pure handler for Vty key events without terminal I/O.
handleVtyEvent :: Vty.Event -> AppState -> (AppState, Maybe TUIAction)
handleVtyEvent = handleVtyEventPure

-- | Pure handler for Vty key events without terminal I/O.
handleVtyEventPure :: Vty.Event -> AppState -> (AppState, Maybe TUIAction)
handleVtyEventPure vtyEv st = case vtyEv of
  Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] ->
    (st, Just ActionQuit)

  Vty.EvKey Vty.KEsc [] ->
    (st, Just ActionQuit)

  Vty.EvKey (Vty.KChar '\t') [] ->
    let nextRing = focusNext (appFocusRing st)
    in (st { appFocusRing = nextRing }, Just ActionFocusNext)

  Vty.EvKey Vty.KBackTab [] ->
    let prevRing = focusPrev (appFocusRing st)
    in (st { appFocusRing = prevRing }, Just ActionFocusPrev)

  Vty.EvKey (Vty.KChar 'p') [Vty.MCtrl] ->
    (st { appFocusRing = focusSetCurrent EditorPrompt (appFocusRing st) }, Just (ActionFocusResource EditorPrompt))

  Vty.EvKey (Vty.KChar 'h') [Vty.MCtrl] ->
    (st { appFocusRing = focusSetCurrent ViewportChat (appFocusRing st) }, Just (ActionFocusResource ViewportChat))

  Vty.EvKey (Vty.KChar 'd') [Vty.MCtrl] ->
    (st { appFocusRing = focusSetCurrent ViewportDiff (appFocusRing st) }, Just (ActionFocusResource ViewportDiff))

  Vty.EvKey (Vty.KChar 'l') [Vty.MCtrl] ->
    (st { appFocusRing = focusSetCurrent ViewportToolLog (appFocusRing st) }, Just (ActionFocusResource ViewportToolLog))

  Vty.EvKey (Vty.KChar 'x') [Vty.MCtrl] ->
    (st { appDiffState = Nothing }, Just ActionClearDiff)

  Vty.EvKey Vty.KEnter [] | focusGetCurrent (appFocusRing st) == Just EditorPrompt ->
    let content = T.strip . T.intercalate "\n" . getEditContents $ appEditor st
    in if T.null content
         then (st, Just ActionNone)
         else
           let updatedSt = submitPromptPure content st
           in (updatedSt, Just (ActionSubmitPrompt content))

  Vty.EvKey Vty.KUp [] | Just vp <- focusGetCurrent (appFocusRing st), vp /= EditorPrompt ->
    (st, Just (ActionScrollUp vp 1))

  Vty.EvKey Vty.KDown [] | Just vp <- focusGetCurrent (appFocusRing st), vp /= EditorPrompt ->
    (st, Just (ActionScrollDown vp 1))

  Vty.EvKey Vty.KPageUp [] | Just vp <- focusGetCurrent (appFocusRing st), vp /= EditorPrompt ->
    (st, Just (ActionScrollUp vp 10))

  Vty.EvKey Vty.KPageDown [] | Just vp <- focusGetCurrent (appFocusRing st), vp /= EditorPrompt ->
    (st, Just (ActionScrollDown vp 10))

  _ ->
    (st, Just ActionNone)

-- | Pure helper to submit a user prompt and update state.
submitPromptPure :: Text -> AppState -> AppState
submitPromptPure promptText st =
  st { appEditor        = editorText EditorPrompt (Just 3) ""
     , appMessages      = appMessages st ++ [UserMsg promptText]
     , appStatus        = StatusThinking
     , appStatusMessage = "Processing user prompt..."
     , appStreamingText = ""
     }

-- | Pure unified diff computation between old and new text lines.
computeDiffUnified :: Text -> Text -> Text
computeDiffUnified oldTxt newTxt =
  let oldLines = lines (T.unpack oldTxt)
      newLines = lines (T.unpack newTxt)
      grouped = Diff.getGroupedDiff oldLines newLines
      renderGroup (Diff.Both ls _)   = map ("  " <>) ls
      renderGroup (Diff.First ls)    = map ("- " <>) ls
      renderGroup (Diff.Second ls)   = map ("+ " <>) ls
      diffLines = concatMap renderGroup grouped
  in T.pack (unlines diffLines)

-- ============================================================================
-- UI Rendering
-- ============================================================================

-- | Attribute names for theme styling.
diffAddedAttr, diffRemovedAttr, diffHeaderAttr, diffContextAttr :: AttrName
diffAddedAttr   = attrName "diffAdded"
diffRemovedAttr = attrName "diffRemoved"
diffHeaderAttr  = attrName "diffHeader"
diffContextAttr = attrName "diffContext"

statusIdleAttr, statusThinkingAttr, statusStreamingAttr, statusErrorAttr :: AttrName
statusIdleAttr      = attrName "statusIdle"
statusThinkingAttr  = attrName "statusThinking"
statusStreamingAttr = attrName "statusStreaming"
statusErrorAttr     = attrName "statusError"

toolLogAttr, headerAttr, borderAttr :: AttrName
toolLogAttr = attrName "toolLog"
headerAttr  = attrName "header"
borderAttr  = attrName "border"

-- | Complete Attribute map for the application.
tuiAttrMap :: AppState -> AttrMap
tuiAttrMap _ = attrMap Vty.defAttr
  [ (diffAddedAttr,       Vty.defAttr `Vty.withForeColor` Vty.green)
  , (diffRemovedAttr,     Vty.defAttr `Vty.withForeColor` Vty.red)
  , (diffHeaderAttr,      Vty.defAttr `Vty.withForeColor` Vty.cyan `Vty.withStyle` Vty.bold)
  , (diffContextAttr,     Vty.defAttr `Vty.withForeColor` Vty.white)
  , (statusIdleAttr,      Vty.defAttr `Vty.withForeColor` Vty.green `Vty.withStyle` Vty.bold)
  , (statusThinkingAttr,  Vty.defAttr `Vty.withForeColor` Vty.yellow `Vty.withStyle` Vty.bold)
  , (statusStreamingAttr, Vty.defAttr `Vty.withForeColor` Vty.cyan `Vty.withStyle` Vty.bold)
  , (statusErrorAttr,     Vty.defAttr `Vty.withForeColor` Vty.brightRed `Vty.withStyle` Vty.bold)
  , (toolLogAttr,         Vty.defAttr `Vty.withForeColor` Vty.brightBlack)
  , (headerAttr,          Vty.defAttr `Vty.withForeColor` Vty.white `Vty.withStyle` Vty.bold)
  , (borderAttr,          Vty.defAttr `Vty.withForeColor` Vty.cyan)
  ]

-- | Top-level UI drawing function.
drawUI :: AppState -> [Widget ResourceName]
drawUI st =
  [ drawHeader st
      <=> hBorder
      <=> drawMainBody st
      <=> hBorder
      <=> drawStatusBar st
      <=> drawPromptEditor st
      <=> drawHelpBar
  ]

-- | Draw top header bar.
drawHeader :: AppState -> Widget ResourceName
drawHeader st =
  withAttr headerAttr $
    padRight Max (txt " [LLMonad Coding Agent] ")
      <+> padLeft Max (txt ("Workspace: " <> T.pack (appWorkspacePath st) <> " | Model: " <> appModelName st <> " "))

-- | Draw central main body with chat viewport on left, diff & tools on right.
drawMainBody :: AppState -> Widget ResourceName
drawMainBody st =
  drawChatPane st
    <+> vBorder
    <+> drawSidePane st

-- | Draw chat history and active streaming response.
drawChatPane :: AppState -> Widget ResourceName
drawChatPane st =
  let isFocused = focusGetCurrent (appFocusRing st) == Just ViewportChat
      title = if isFocused then " Chat History [FOCUSED] " else " Chat History "
      renderedMsgs = map renderChatMessage (appMessages st)
      streamingWidget = if T.null (appStreamingText st)
        then emptyWidget
        else vBox
               [ hBorder
               , withAttr statusStreamingAttr (txt "Assistant (streaming):")
               , txtWrap (appStreamingText st)
               ]
      allContent = vBox (renderedMsgs ++ [streamingWidget])
  in borderWithLabel (txt title) (viewport ViewportChat Both allContent)

-- | Render single chat message.
renderChatMessage :: ChatMessage -> Widget ResourceName
renderChatMessage msg = case msg of
  UserMsg t ->
    padBottom (Pad 1) $
      vBox
        [ withAttr (attrName "userLabel") (txt "User:")
        , txtWrap t
        ]
  AssistantMsg t calls ->
    let callsWidget = if null calls
          then emptyWidget
          else vBox (map (\c -> withAttr toolLogAttr (txt ("[Tool Call: " <> toolCallName c <> "]"))) calls)
    in padBottom (Pad 1) $
         vBox
           [ withAttr (attrName "assistantLabel") (txt "Assistant:")
           , txtWrap t
           , callsWidget
           ]
  SystemMsg t ->
    padBottom (Pad 1) $
      withAttr toolLogAttr (txt ("[System] " <> t))
  ToolMsg tid t ->
    padBottom (Pad 1) $
      withAttr toolLogAttr (txt ("[Tool Result " <> tid <> "] " <> t))

-- | Draw side pane: visual diff on top, tool log drawer on bottom.
drawSidePane :: AppState -> Widget ResourceName
drawSidePane st =
  drawDiffPane st
    <=> hBorder
    <=> drawToolLogsPane st

-- | Draw visual diff renderer.
drawDiffPane :: AppState -> Widget ResourceName
drawDiffPane st =
  let isFocused = focusGetCurrent (appFocusRing st) == Just ViewportDiff
      title = if isFocused then " Visual Diff [FOCUSED] " else " Visual Diff "
      content = case appDiffState st of
        Nothing -> withAttr toolLogAttr (txt " (No active diff) ")
        Just ds -> renderDiffLines (vdsContent ds)
  in borderWithLabel (txt title) (viewport ViewportDiff Both content)

-- | Colorize lines of diff.
renderDiffLines :: Text -> Widget ResourceName
renderDiffLines diffTxt =
  let ls = T.lines diffTxt
      colorizeLine l
        | "+" `T.isPrefixOf` l && not ("+++" `T.isPrefixOf` l) =
            withAttr diffAddedAttr (txt l)
        | "-" `T.isPrefixOf` l && not ("---" `T.isPrefixOf` l) =
            withAttr diffRemovedAttr (txt l)
        | "@@" `T.isPrefixOf` l =
            withAttr diffHeaderAttr (txt l)
        | "---" `T.isPrefixOf` l || "+++" `T.isPrefixOf` l =
            withAttr diffHeaderAttr (txt l)
        | otherwise =
            withAttr diffContextAttr (txt l)
  in vBox (map colorizeLine ls)

-- | Draw tool execution logs panel.
drawToolLogsPane :: AppState -> Widget ResourceName
drawToolLogsPane st =
  let isFocused = focusGetCurrent (appFocusRing st) == Just ViewportToolLog
      title = if isFocused then " Tool Execution Logs [FOCUSED] " else " Tool Execution Logs "
      logs = appToolLogs st
      renderedLogs = if null logs
        then withAttr toolLogAttr (txt " (No tool invocations recorded) ")
        else vBox (map renderToolLog logs)
  in borderWithLabel (txt title) (viewport ViewportToolLog Both renderedLogs)

-- | Render a single tool execution log entry.
renderToolLog :: ToolLogEntry -> Widget ResourceName
renderToolLog entry =
  let statusTxt = case tleResult entry of
        Nothing        -> "[RUNNING]"
        Just (Right _) -> "[SUCCESS]"
        Just (Left _)  -> "[ERROR]"
      argsSummary = TL.toStrict (AesonText.encodeToLazyText (tleArguments entry))
  in padBottom (Pad 1) $
       vBox
         [ padRight Max (txt ("• " <> tleToolName entry <> " "))
             <+> withAttr toolLogAttr (txt statusTxt)
         , withAttr toolLogAttr (txtWrap ("Args: " <> argsSummary))
         ]

-- | Draw status and metrics bar.
drawStatusBar :: AppState -> Widget ResourceName
drawStatusBar st =
  let (statusAttrVal, statusLabel) = case appStatus st of
        StatusIdle           -> (statusIdleAttr, "IDLE")
        StatusThinking       -> (statusThinkingAttr, "THINKING")
        StatusStreaming      -> (statusStreamingAttr, "STREAMING")
        StatusRunningTool tn -> (statusThinkingAttr, "TOOL:" <> tn)
        StatusError _        -> (statusErrorAttr, "ERROR")
        StatusCompleted      -> (statusIdleAttr, "COMPLETED")
      m = appMetrics st
      metricsTxt = "Turns: " <> T.pack (show (amTurnCount m))
                <> " | Tokens: " <> T.pack (show (amTotalTokens m))
                <> " | Latency: " <> T.pack (show (amLatencyMs m)) <> "ms"
  in txt " Status: ["
       <+> withAttr statusAttrVal (txt statusLabel)
       <+> txt "] "
       <+> txt (appStatusMessage st)
       <+> padLeft Max (withAttr toolLogAttr (txt metricsTxt))

-- | Draw interactive prompt editor.
drawPromptEditor :: AppState -> Widget ResourceName
drawPromptEditor st =
  let isFocused = focusGetCurrent (appFocusRing st) == Just EditorPrompt
      title = if isFocused then " Prompt Input [FOCUSED] " else " Prompt Input "
  in borderWithLabel (txt title) (renderEditor (txt . T.unlines) isFocused (appEditor st))

-- | Draw help keybindings bar at bottom.
drawHelpBar :: Widget ResourceName
drawHelpBar =
  withAttr toolLogAttr $
    hCenter (txt "[Enter] Submit | [Tab] Focus | [Ctrl+P/H/D/L] Direct Focus | [Ctrl+X] Clear Diff | [Ctrl+C/Esc] Quit")

-- ============================================================================
-- Brick App Definition & Event Loop
-- ============================================================================

-- | Top-level Brick App definition.
tuiApp :: App AppState CustomAppEvent ResourceName
tuiApp = App
  { appDraw         = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = handleAppEvent
  , appStartEvent   = pure ()
  , appAttrMap      = tuiAttrMap
  }

-- | Main Brick event handler wiring custom events and Vty keyboard navigation.
handleAppEvent :: BrickEvent ResourceName CustomAppEvent -> EventM ResourceName AppState ()
handleAppEvent brickEv = case brickEv of
  AppEvent customEv -> do
    modify (handleCustomAppEvent customEv)

  VtyEvent vtyEv -> do
    st <- get
    let (nextSt, mAction) = handleVtyEventPure vtyEv st
    put nextSt
    case mAction of
      Just ActionQuit -> halt
      Just (ActionSubmitPrompt p) -> do
        case appEventChan nextSt of
          Just chan -> do
            let tConfig = TUIConfig
                  { cfgWorkspacePath   = appWorkspacePath nextSt
                  , cfgModelName       = appModelName nextSt
                  , cfgSystemPrompt    = appSystemPrompt nextSt
                  , cfgSessionFilePath = appSessionFilePath nextSt
                  }
            aThread <- liftIO (async (runAgentWorker chan tConfig p))
            modify (\s -> s { appAgentThread = Just aThread })
          Nothing -> pure ()
      Just (ActionScrollUp vp linesCount) ->
        vScrollBy (viewportScroll vp) (-linesCount)
      Just (ActionScrollDown vp linesCount) ->
        vScrollBy (viewportScroll vp) linesCount
      _ -> do
        when (focusGetCurrent (appFocusRing nextSt) == Just EditorPrompt) $ do
          (newEd, _) <- nestEventM (appEditor nextSt) (handleEditorEvent brickEv)
          modify (\s -> s { appEditor = newEd })

  _ -> pure ()

-- | Worker thread simulating or executing agent actions and streaming back events.
runAgentWorker :: BChan CustomAppEvent -> TUIConfig -> Text -> IO ()
runAgentWorker chan _cfg promptText = do
  writeBChan chan (AgentStateChanged "Thinking...")
  writeBChan chan (TokenStreamed ("I received your instruction: \"" <> promptText <> "\".\n\n"))
  writeBChan chan (TokenStreamed "Analyzing workspace and planning changes...\n")
  writeBChan chan (ToolStarted "listDir" (Aeson.object ["directoryPath" Aeson..= ("." :: Text)]))
  writeBChan chan (ToolFinished "listDir" (Right (Aeson.object ["entries" Aeson..= (["src", "app", "test"] :: [Text])])))
  writeBChan chan (TokenStreamed "Completed directory scan. Changes ready.\n")
  writeBChan chan TurnCompleted

-- | Run the Brick TUI application with default configuration.
runTUIApp :: IO ()
runTUIApp = runTUIAppWithConfig defaultTUIConfig

-- | Run the Brick TUI application with custom configuration.
runTUIAppWithConfig :: TUIConfig -> IO ()
runTUIAppWithConfig cfg = do
  chan <- newBChan 100
  let st = initialAppState cfg (Just chan)
  vty <- VtyCross.mkVty Vty.defaultConfig
  void (customMain vty (VtyCross.mkVty Vty.defaultConfig) (Just chan) tuiApp st)
