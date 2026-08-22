{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Concurrent child agent delegation with isolated sandboxes, step budgets, and journal tracking.
module LLMonad.Subagent
  ( -- * Subagent Tool & Runners
    subagentTool
  , subagentToolWith
  , runSubagent
  , runSubagentAsync

    -- * Data Types
  , SubagentArgs (..)
  , SubagentResult (..)

    -- * Filtering & Validation Helpers
  , filterSubagentTools
  , isReadOnlyTool
  ) where

import Control.Concurrent.Async (Async, async)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Exception (SomeException)
import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , toJSON
  , withObject
  , (.:)
  , (.:?)
  )
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import Effectful.Dispatch.Dynamic (EffectHandler, interpose, send)
import qualified Effectful.Exception as EE
import GHC.Generics (Generic)
import LLMonad.Agent (runAgentWith)
import LLMonad.Core (LLM (..))
import LLMonad.Error (LLMError (..), prettyError)
import LLMonad.Interpreter.HTTP (HTTPState (..))
import System.IO.Unsafe (unsafePerformIO)
import LLMonad.Journal
  ( Journal
  , JournalEvent (..)
  , recordEvent
  )
import LLMonad.Schema (ToSchema (..))
import LLMonad.Tools
  ( AgentOpts (..)
  , Tool (..)
  , defaultAgentOpts
  , hoistTool
  , tool'
  )
import LLMonad.Tools.Coding (standardCodingTools)
import LLMonad.Types (ToolSpec (..))
import LLMonad.World
  ( WorktreeConfig (..)
  , WorktreeSummary (..)
  , defaultWorktreeConfig
  , getWorkspaceRoot
  , prettyWorldError
  , World
  , WorldError (..)
  )
import LLMonad.World.Worktree (runWorldWorktree)

-- | Arguments for spawning a child subagent.
data SubagentArgs = SubagentArgs
  { taskInstruction     :: !Text
  , subagentRole        :: !(Maybe Text)
  , maxRounds           :: !(Maybe Int)
  , allowedTools        :: !(Maybe [Text])
  , useIsolatedWorktree :: !(Maybe Bool)
  } deriving (Show, Eq, Generic, ToJSON, ToSchema)

instance FromJSON SubagentArgs where
  parseJSON = withObject "SubagentArgs" $ \o -> do
    task <- o .:? "taskInstruction" >>= \case
      Just t -> pure t
      Nothing -> o .:? "task_instruction" >>= \case
        Just t -> pure t
        Nothing -> o .:? "task" >>= \case
          Just t -> pure t
          Nothing -> o .: "instruction"
    role <- o .:? "subagentRole" >>= \case
      Just r -> pure (Just r)
      Nothing -> o .:? "subagent_role" >>= \case
        Just r -> pure (Just r)
        Nothing -> o .:? "role"
    mr <- o .:? "maxRounds" >>= \case
      Just r -> pure (Just r)
      Nothing -> o .:? "max_rounds" >>= \case
        Just r -> pure (Just r)
        Nothing -> o .:? "rounds"
    tools <- o .:? "allowedTools" >>= \case
      Just ts -> pure (Just ts)
      Nothing -> o .:? "allowed_tools" >>= \case
        Just ts -> pure (Just ts)
        Nothing -> o .:? "tools"
    wt <- o .:? "useIsolatedWorktree" >>= \case
      Just w -> pure (Just w)
      Nothing -> o .:? "use_isolated_worktree" >>= \case
        Just w -> pure (Just w)
        Nothing -> o .:? "isolated_worktree" >>= \case
          Just w -> pure (Just w)
          Nothing -> o .:? "worktree"
    pure (SubagentArgs task role mr tools wt)

-- | Result of subagent execution.
data SubagentResult = SubagentResult
  { srStatus        :: !Text -- ^ "completed", "exhausted", or "failed"
  , srOutput        :: !Text
  , srRoundsUsed    :: !Int
  , srModifiedFiles :: ![FilePath]
  , srGitDiff       :: !(Maybe Text)
  } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

-- | Check whether a tool name belongs to the read-only suite.
isReadOnlyTool :: Text -> Bool
isReadOnlyTool name = name `elem`
  [ "view_file", "viewFile"
  , "grep_search", "grepSearch"
  , "find_by_name", "findByName"
  , "list_dir", "listDir"
  ]

-- | Filter available tools for child agent sandboxing:
-- 1. Strip recursive 'subagent' calls to avoid fork-bomb cycles.
-- 2. Restrict to read-only tools if role is 'explorer' or 'readonly'.
-- 3. Whitelist specific tools if 'allowedTools' is specified.
filterSubagentTools :: SubagentArgs -> [Tool (Eff es)] -> [Tool (Eff es)]
filterSubagentTools args allTools =
  let nonRecursive = filter (\t -> toolSpecName (toolSpec t) /= "subagent" && toolSpecName (toolSpec t) /= "subagentTool") allTools
      roleFiltered = case subagentRole args of
        Just "explorer" -> filter (isReadOnlyTool . toolSpecName . toolSpec) nonRecursive
        Just "readonly" -> filter (isReadOnlyTool . toolSpecName . toolSpec) nonRecursive
        _               -> nonRecursive
      whitelistFiltered = case allowedTools args of
        Just allowed -> filter (\t -> toolSpecName (toolSpec t) `elem` allowed) roleFiltered
        Nothing      -> roleFiltered
   in whitelistFiltered

-- | Global lock to serialize turn execution against shared LLM providers while preserving state isolation.
subagentTurnLock :: MVar ()
subagentTurnLock = unsafePerformIO (newMVar ())
{-# NOINLINE subagentTurnLock #-}

-- | Execute a subagent computation with an isolated conversation state store.
-- Internal prompts, intermediate reasoning, and tool calls are retained in the child state
-- and do not pollute the parent conversation history.
withIsolatedLLM :: (LLM :> es, IOE :> es) => Eff es a -> Eff es a
withIsolatedLLM action = do
  childStateRef <- liftIO (newIORef (HTTPState Nothing []))
  interpose (isolatedLLMHandler childStateRef) action

isolatedLLMHandler ::
  (LLM :> es, IOE :> es) =>
  IORef HTTPState ->
  EffectHandler LLM es
isolatedLLMHandler childStateRef _ = \case
  GetHistory -> liftIO (hsHistory <$> readIORef childStateRef)
  SetHistory msgs -> liftIO (modifyIORef' childStateRef (\s -> s { hsHistory = msgs }))
  PushMessage msg -> liftIO (modifyIORef' childStateRef (\s -> s { hsHistory = hsHistory s ++ [msg] }))
  ClearHistory -> liftIO (modifyIORef' childStateRef (\s -> s { hsHistory = [] }))
  GetSystem -> liftIO (hsSystem <$> readIORef childStateRef)
  SetSystem sys -> liftIO (modifyIORef' childStateRef (\s -> s { hsSystem = Just sys }))
  ClearSystem -> liftIO (modifyIORef' childStateRef (\s -> s { hsSystem = Nothing }))

  ChatRound callParams fmt specs choice -> do
    HTTPState childSys childHist <- liftIO (readIORef childStateRef)
    EE.bracket_ (liftIO (takeMVar subagentTurnLock)) (liftIO (putMVar subagentTurnLock ())) $ do
      parentHist <- send GetHistory
      parentSys <- send GetSystem
      send (SetHistory childHist)
      case childSys of
        Just cs -> send (SetSystem cs)
        Nothing -> send ClearSystem
      resp <- (send (ChatRound callParams fmt specs choice)) `EE.onException` do
        send (SetHistory parentHist)
        case parentSys of
          Just ps -> send (SetSystem ps)
          Nothing -> send ClearSystem
      updatedHist <- send GetHistory
      let newMsgs = drop (length childHist) updatedHist
      liftIO (modifyIORef' childStateRef (\s -> s { hsHistory = hsHistory s ++ newMsgs }))
      send (SetHistory parentHist)
      case parentSys of
        Just ps -> send (SetSystem ps)
        Nothing -> send ClearSystem
      pure resp

  StreamRound callParams fmt specs cb -> do
    HTTPState childSys childHist <- liftIO (readIORef childStateRef)
    EE.bracket_ (liftIO (takeMVar subagentTurnLock)) (liftIO (putMVar subagentTurnLock ())) $ do
      parentHist <- send GetHistory
      parentSys <- send GetSystem
      send (SetHistory childHist)
      case childSys of
        Just cs -> send (SetSystem cs)
        Nothing -> send ClearSystem
      resp <- (send (StreamRound callParams fmt specs cb)) `EE.onException` do
        send (SetHistory parentHist)
        case parentSys of
          Just ps -> send (SetSystem ps)
          Nothing -> send ClearSystem
      updatedHist <- send GetHistory
      let newMsgs = drop (length childHist) updatedHist
      liftIO (modifyIORef' childStateRef (\s -> s { hsHistory = hsHistory s ++ newMsgs }))
      send (SetHistory parentHist)
      case parentSys of
        Just ps -> send (SetSystem ps)
        Nothing -> send ClearSystem
      pure resp

-- | Run a child subagent synchronously.
runSubagent ::
  (World :> es, Journal :> es, LLM :> es, IOE :> es) =>
  SubagentArgs ->
  [Tool (Eff es)] ->
  Eff es SubagentResult
runSubagent args parentTools = withIsolatedLLM $ do
  recordEvent (ToolInvoked "subagent" "subagent" (toJSON args))
  let childTools = filterSubagentTools args parentTools
  let maxR = max 1 (fromMaybe 8 (maxRounds args))
  let opts = defaultAgentOpts { agentMaxRounds = maxR }

  result <- case useIsolatedWorktree args of
    Just True -> do
      root <- getWorkspaceRoot
      let cfg = (defaultWorktreeConfig root) { wtBranchPrefix = "subagent-sandbox" }
      (do
        (out, summary) <- runWorldWorktree cfg $ do
          let worktreeCodingTools = standardCodingTools
          let isCodingTool t = toolSpecName (toolSpec t) `elem`
                [ "view_file", "viewFile"
                , "edit_file", "editFile"
                , "grep_search", "grepSearch"
                , "find_by_name", "findByName"
                , "list_dir", "listDir"
                , "run_command", "runCommand"
                ]
          let allowedNames = map (toolSpecName . toolSpec) childTools
          let worktreeTools = filter (\t -> toolSpecName (toolSpec t) `elem` allowedNames) worktreeCodingTools
                           ++ map (hoistTool raise) (filter (not . isCodingTool) childTools)
          runAgentWith opts worktreeTools (taskInstruction args)
        pure $ SubagentResult "completed" out maxR (wsFilesChanged summary) (Just (wsDiff summary))
        )
        `EE.catch` (\(err :: LLMError) -> case err of
          AgentRoundsExhausted _ ->
            pure $ SubagentResult "exhausted" ("Step budget exhausted (" <> T.pack (show maxR) <> " rounds)") maxR [] Nothing
          other ->
            pure $ SubagentResult "failed" ("Subagent LLM error: " <> prettyError other) 0 [] Nothing)
        `EE.catch` (\(err :: WorldError) ->
          pure $ SubagentResult "failed" ("Worktree sandbox failed: " <> prettyWorldError err) 0 [] Nothing)
        `EE.catch` (\(err :: SomeException) ->
          pure $ SubagentResult "failed" ("Subagent error: " <> T.pack (show err)) 0 [] Nothing)

    _ -> do
      (do
        out <- runAgentWith opts childTools (taskInstruction args)
        pure $ SubagentResult "completed" out maxR [] Nothing
        )
        `EE.catch` (\(err :: LLMError) -> case err of
          AgentRoundsExhausted _ ->
            pure $ SubagentResult "exhausted" ("Step budget exhausted (" <> T.pack (show maxR) <> " rounds)") maxR [] Nothing
          other ->
            pure $ SubagentResult "failed" ("Subagent LLM error: " <> prettyError other) 0 [] Nothing)
        `EE.catch` (\(err :: WorldError) ->
          pure $ SubagentResult "failed" ("Subagent World error: " <> prettyWorldError err) 0 [] Nothing)
        `EE.catch` (\(err :: SomeException) ->
          pure $ SubagentResult "failed" ("Subagent error: " <> T.pack (show err)) 0 [] Nothing)

  recordEvent (ToolCompleted "subagent" (Right (toJSON result)))
  pure result

-- | Spawn child subagent on a concurrent green thread with Async handle.
runSubagentAsync ::
  (World :> es, Journal :> es, LLM :> es, IOE :> es) =>
  SubagentArgs ->
  [Tool (Eff es)] ->
  Eff es (Async SubagentResult)
runSubagentAsync args parentTools = do
  withEffToIO (ConcUnlift Ephemeral Unlimited) $ \unlift -> do
    async (unlift (runSubagent args parentTools))

-- | Callable 'Tool' exposing subagent delegation to the LLM agent.
subagentTool ::
  (World :> es, Journal :> es, LLM :> es, IOE :> es) =>
  [Tool (Eff es)] ->
  Tool (Eff es)
subagentTool parentTools = subagentToolWith defaultAgentOpts parentTools

-- | Callable 'Tool' exposing subagent delegation with custom agent options.
subagentToolWith ::
  (World :> es, Journal :> es, LLM :> es, IOE :> es) =>
  AgentOpts ->
  [Tool (Eff es)] ->
  Tool (Eff es)
subagentToolWith _parentOpts parentTools =
  tool' "subagent" "Delegate a subtask to an isolated child agent with tool restrictions and step budget" $ \args -> do
    res <- runSubagent args parentTools
    pure (Right (toJSON res))
