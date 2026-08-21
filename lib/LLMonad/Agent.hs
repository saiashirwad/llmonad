{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LLMonad.Agent
  ( -- * Agent Execution
    runAgent,
    runAgentWith,
    runAgentWithMaxSteps,

    -- * Step Execution
    stepAgent,
  )
where

import Control.Monad.Error.Class (throwError)
import Control.Monad.State (gets)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import LLMonad.Client (LLMError (..))
import LLMonad.Core
import LLMonad.Prompt (user)
import LLMonad.Tools
import LLMonad.Types

-- | Run an autonomous ReAct tool execution agent with registered tools
runAgent :: Text -> LLM Text
runAgent query = do
  currentTools <- gets (Map.elems . stateTools)
  runAgentWithMaxSteps 10 currentTools query

-- | Run an agent with explicit tools and default 10 step limit
runAgentWith :: [Tool] -> Text -> LLM Text
runAgentWith tools query =
  runAgentWithMaxSteps 10 tools query

-- | Run an agent with explicit step limit and tool list
runAgentWithMaxSteps :: Int -> [Tool] -> Text -> LLM Text
runAgentWithMaxSteps maxSteps tools query = do
  -- Register all provided tools
  mapM_ registerTool tools
  appendHistory (user query)
  agentLoop maxSteps
  where
    agentLoop remainingSteps
      | remainingSteps <= 0 =
          throwError $
            MaxRetriesExceeded maxSteps "Agent reached maximum tool step limit without finishing."
      | otherwise = do
          hist <- getHistory
          resp <- chat hist
          case responseChoices resp of
            [] -> throwError $ InvalidResponse "Empty choices returned from model during agent run"
            (c : _) -> do
              let msg = choiceMessage c
              case messageToolCalls msg of
                Just calls | not (null calls) -> do
                  -- Append assistant message requesting tool calls
                  appendHistory msg
                  toolsMap <- gets stateTools

                  -- Execute each tool call and append tool result messages
                  mapM_ (dispatchAndRecord toolsMap) calls

                  -- Continue loop to let model observe results
                  agentLoop (remainingSteps - 1)
                _ -> do
                  -- Model finished with final message
                  let finalAnswer = messageContent msg
                  appendHistory msg
                  pure finalAnswer

    dispatchAndRecord :: Map Text Tool -> ToolCall -> LLM ()
    dispatchAndRecord toolsMap tcall@ToolCall {..} = do
      resOrErr <- liftIO $ executeToolCall toolsMap tcall
      case resOrErr of
        Right resText ->
          appendHistory (toolMsg toolCallId resText)
        Left errText ->
          appendHistory (toolMsg toolCallId ("Error during execution: " <> errText))

-- | Perform a single step of the agent loop, returning either new messages or final answer
stepAgent :: LLM (Either [Message] Text)
stepAgent = do
  hist <- getHistory
  resp <- chat hist
  case responseChoices resp of
    [] -> throwError $ InvalidResponse "Empty choices returned from model during agent step"
    (c : _) -> do
      let msg = choiceMessage c
      case messageToolCalls msg of
        Just calls | not (null calls) -> do
          appendHistory msg
          toolsMap <- gets stateTools
          let execCall tc = do
                resOrErr <- liftIO $ executeToolCall toolsMap tc
                let tid = toolCallId tc
                case resOrErr of
                  Right res -> pure (toolMsg tid res)
                  Left err -> pure (toolMsg tid ("Error: " <> err))
          toolResMsgs <- mapM execCall calls
          mapM_ appendHistory toolResMsgs
          pure (Left (msg : toolResMsgs))
        _ -> do
          let finalAnswer = messageContent msg
          appendHistory msg
          pure (Right finalAnswer)
