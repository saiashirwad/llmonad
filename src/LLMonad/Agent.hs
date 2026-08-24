{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | First-class agents with isolated model sessions.
module LLMonad.Agent
  ( Agent
  , AgentDef
  , AgentOpts (..)
  , defaultAgentOpts
  , textAgent
  , structuredAgent
  , withAgentOpts
  , bind
  , invoke
  , Session
  , start
  , continue

    -- * Low-level model-session loop
  , runAgent
  , runAgentWith
  , runAgentStructured
  , runAgentStructuredWith
  , useTools
  , useToolsWith
  ) where

import Control.Concurrent.MVar (modifyMVar, newMVar)
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Aeson (FromJSON, encode, object, parseJSON, (.=))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as LBS
import Data.List (find, group, sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Effectful
import LLMonad.Core
  ( LLM
  , chatRound
  , clearSystem
  , getHistory
  , pushMessage
  , setHistory
  , setSystem
  , withTransaction
  )
import LLMonad.Error (LLMError (..))
import LLMonad.Internal.Extract (decodeViaJSON)
import LLMonad.Model (ModelRuntime, runModelRuntime)
import LLMonad.Schema (ToSchema (..))
import LLMonad.Tools (Tool (..), Toolset, hoistTool, toolsetTools)
import LLMonad.Types

-- | Agent loop policy.
data AgentOpts = AgentOpts
  { agentMaxRounds :: Int
  , agentParams :: Params
  }

defaultAgentOpts :: AgentOpts
defaultAgentOpts = AgentOpts 8 defaultParams

data AgentOutput output where
  TextOutput :: AgentOutput Text
  StructuredOutput :: (FromJSON output, ToSchema output) => AgentOutput output

-- | A model-neutral agent definition.
data AgentDef input output = AgentDef
  { definitionSystem :: Maybe Text
  , definitionPrompt :: input -> Text
  , definitionOutput :: AgentOutput output
  , definitionOpts :: AgentOpts
  }

-- | A configured agent. The constructor is private so each invocation keeps
-- its own conversation state.
newtype Agent es input output = Agent
  { runAgentFrom :: [ChatMessage] -> input -> Eff es (output, [ChatMessage])
  }

-- | A stateful conversation with one configured agent.
newtype Session es input output = Session
  { runSession :: input -> Eff es output
  }

-- | Define an agent that returns text.
textAgent :: Text -> (input -> Text) -> AgentDef input Text
textAgent systemPrompt render =
  AgentDef (Just systemPrompt) render TextOutput defaultAgentOpts

-- | Define an agent that returns a schema-derived value.
structuredAgent ::
  (FromJSON output, ToSchema output) =>
  Text ->
  (input -> Text) ->
  AgentDef input output
structuredAgent systemPrompt render =
  AgentDef (Just systemPrompt) render StructuredOutput defaultAgentOpts

-- | Replace the execution policy for an agent definition.
withAgentOpts :: AgentOpts -> AgentDef input output -> AgentDef input output
withAgentOpts opts definition = definition {definitionOpts = opts}

-- | Attach a model and toolset to a model-neutral definition.
bind ::
  (IOE :> es) =>
  ModelRuntime es ->
  Toolset es ->
  AgentDef input output ->
  Agent es input output
bind runtime toolset definition = Agent $ \history input -> do
  case duplicateToolNames (toolsetTools toolset) of
    [] -> pure ()
    names -> liftIO . throwIO . AgentConfigurationError $
      "duplicate tool names: " <> T.intercalate ", " names

  runModelRuntime runtime $ do
    setHistory history
    case definitionSystem definition of
      Nothing -> clearSystem
      Just systemPrompt -> setSystem systemPrompt

    let availableTools = map (hoistTool raise) (toolsetTools toolset)
        prompt = definitionPrompt definition input
        opts = definitionOpts definition

    result <- case definitionOutput definition of
      TextOutput -> runAgentWith opts availableTools prompt
      StructuredOutput -> runAgentStructuredWith opts availableTools prompt

    finalHistory <- getHistory
    pure (result, finalHistory)

-- | Run an agent with a fresh conversation.
invoke :: Agent es input output -> input -> Eff es output
invoke agent input = fst <$> runAgentFrom agent [] input

-- | Start an explicit persistent conversation. Calls to one session are
-- serialized, while different sessions can run concurrently.
start :: (IOE :> es) => Agent es input output -> Eff es (Session es input output)
start agent = do
  historyVar <- liftIO (newMVar [])
  pure . Session $ \input ->
    withEffToIO (ConcUnlift Ephemeral Unlimited) $ \unlift ->
      modifyMVar historyVar $ \history -> do
        (result, nextHistory) <- unlift (runAgentFrom agent history input)
        pure (nextHistory, result)

-- | Continue an explicit persistent conversation.
continue :: Session es input output -> input -> Eff es output
continue = runSession

duplicateToolNames :: [Tool m] -> [Text]
duplicateToolNames =
  mapMaybe duplicateName
    . group
    . sort
    . map (toolSpecName . toolSpec)
  where
    duplicateName (name : _ : _) = Just name
    duplicateName _ = Nothing

-- | Run the low-level tool loop inside an existing LLM session.
runAgent :: (LLM :> es, IOE :> es) => [Tool (Eff es)] -> Text -> Eff es Text
runAgent = runAgentWith defaultAgentOpts

runAgentWith :: (LLM :> es, IOE :> es) => AgentOpts -> [Tool (Eff es)] -> Text -> Eff es Text
runAgentWith opts availableTools instruction = withTransaction $ do
  pushMessage (UserMsg instruction)
  loop (agentMaxRounds opts) []
  where
    specs = map toolSpec availableTools

    loop roundsLeft previousCalls
      | roundsLeft <= 0 = liftIO (throwIO (AgentRoundsExhausted (agentMaxRounds opts)))
      | otherwise = do
          response <- chatRound (agentParams opts) RfText specs ToolAuto
          case crspToolCalls response of
            [] -> pure (crspText response)
            calls
              | roundsLeft <= 1 -> liftIO (throwIO (AgentRoundsExhausted (agentMaxRounds opts)))
              | otherwise -> do
                  let currentCalls = [(toolCallName call, toolCallArguments call) | call <- calls]
                  mapM_ (executeAndRecord specs availableTools) calls
                  when (currentCalls == previousCalls && not (null currentCalls)) $
                    pushMessage (UserMsg repeatedToolWarning)
                  loop (roundsLeft - 1) currentCalls

runAgentStructured ::
  forall output es.
  (LLM :> es, IOE :> es, FromJSON output, ToSchema output) =>
  [Tool (Eff es)] ->
  Text ->
  Eff es output
runAgentStructured = runAgentStructuredWith defaultAgentOpts

runAgentStructuredWith ::
  forall output es.
  (LLM :> es, IOE :> es, FromJSON output, ToSchema output) =>
  AgentOpts ->
  [Tool (Eff es)] ->
  Text ->
  Eff es output
runAgentStructuredWith opts availableTools instruction = withTransaction $ do
  pushMessage (UserMsg instruction)
  toolLoop (agentMaxRounds opts) []
  where
    specs = map toolSpec availableTools
    schemaFormat = RfJsonSchema (schemaTypeName @output) (toSchema @output) True

    toolLoop roundsLeft previousCalls
      | roundsLeft <= 0 = exhausted
      | null availableTools = structuredLoop roundsLeft
      | otherwise = do
          response <- chatRound (agentParams opts) RfText specs ToolAuto
          case crspToolCalls response of
            [] -> decodeOrRetry roundsLeft response
            calls
              | roundsLeft <= 1 -> exhausted
              | otherwise -> do
                  let currentCalls = [(toolCallName call, toolCallArguments call) | call <- calls]
                  mapM_ (executeAndRecord specs availableTools) calls
                  when (currentCalls == previousCalls && not (null currentCalls)) $
                    pushMessage (UserMsg repeatedToolWarning)
                  toolLoop (roundsLeft - 1) currentCalls

    structuredLoop roundsLeft
      | roundsLeft <= 0 = exhausted
      | otherwise = chatRound (agentParams opts) schemaFormat [] ToolAuto >>= decodeOrRetry roundsLeft

    decodeOrRetry roundsLeft response =
      case decodeResponse response of
        Right output -> pure output
        Left detail
          | roundsLeft <= 1 -> liftIO (throwIO (DecodeError detail (crspText response)))
          | otherwise -> do
              pushMessage . UserMsg $
                "Your final response could not be decoded ("
                  <> T.pack detail
                  <> "). Return only valid JSON that conforms to the schema."
              structuredLoop (roundsLeft - 1)

    decodeResponse response = case crspStructuredPayload response of
      Just value -> parseEither parseJSON value
      Nothing -> decodeViaJSON (crspText response)

    exhausted = liftIO (throwIO (AgentRoundsExhausted (agentMaxRounds opts)))

-- | Compatibility name for the single agent-loop implementation.
useTools :: (LLM :> es, IOE :> es) => [Tool (Eff es)] -> Text -> Eff es Text
useTools = runAgent

-- | Compatibility name for the single agent-loop implementation.
useToolsWith :: (LLM :> es, IOE :> es) => AgentOpts -> [Tool (Eff es)] -> Text -> Eff es Text
useToolsWith = runAgentWith

executeAndRecord :: (LLM :> es) => [ToolSpec] -> [Tool (Eff es)] -> ToolCall -> Eff es ()
executeAndRecord specs availableTools call = do
  let implementation = do
        _ <- find ((== toolCallName call) . toolSpecName) specs
        find ((== toolCallName call) . toolSpecName . toolSpec) availableTools
  result <- case implementation of
    Nothing -> pure (Left ("unknown tool: " <> toolCallName call))
    Just selected -> toolRun selected (toolCallArguments call)
  let value = either (object . pure . ("error" .=)) id result
      content = decodeUtf8With lenientDecode . LBS.toStrict . encode $ value
  pushMessage (ToolMsg (toolCallId call) content)

repeatedToolWarning :: Text
repeatedToolWarning =
  "Warning: Repeated identical tool call signature detected. Please adjust your plan or return the final answer."
