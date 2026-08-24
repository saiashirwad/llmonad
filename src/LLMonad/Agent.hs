{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | First-class agents with isolated model sessions.
module LLMonad.Agent (
    Agent,
    AgentDef,
    AgentOpts (..),
    defaultAgentOpts,
    textAgent,
    structuredAgent,
    withAgentOpts,
    mount,
    invoke,
    runAgent,
    runTextLoop,
    runTextLoopWith,
    runStructuredLoop,
    runStructuredLoopWith,
    Session,
    start,
    continue,
    session,
) where

import Control.Category (Category (..))
import Control.Concurrent.MVar (modifyMVar, newMVar)
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Aeson (FromJSON, encode, object, parseJSON, (.=))
import Data.Aeson.Types (parseEither)
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LBS
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Effectful
import LLMonad.Core (
    LLM,
    chatRound,
    clearSystem,
    getHistory,
    pushMessage,
    setHistory,
    setSystem,
    withTransaction,
 )
import LLMonad.Error (LLMError (..))
import LLMonad.Internal.Extract (decodeViaJSON)
import LLMonad.Model (ModelRuntime, runModelRuntime)
import LLMonad.Schema (ToSchema (..))
import LLMonad.Tools (Tool (..), Toolset, duplicateToolNamesIn, hoistTool, toolsetTools)
import LLMonad.Types
import Prelude hiding (id, (.))

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

{- | A configured agent. The constructor is private so each invocation keeps
its own conversation state.

Agents are callable values: 'runAgent' applies one, 'Category' pipes two, and
'invoke' is a friendly spelling of 'runAgent'.
-}
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
withAgentOpts opts definition = definition{definitionOpts = opts}

{- | Attach a model and toolset to a model-neutral definition.

The toolset was validated at assembly time ('tools', '(<>)'), but duplicates
can still slip in through lazy assembly or direct 'Toolset' construction — so
'mount' re-checks here, at the last seam before tokens can be spent. This is
where the model enters, and the only place that needs a 'ModelRuntime'.
-}
mount ::
    (IOE :> es) =>
    ModelRuntime es ->
    Toolset es ->
    AgentDef input output ->
    Agent es input output
mount runtime toolset definition = either rejected wired (checkedToolset toolset)
  where
    -- Matching on the duplicate-name report forces the check at mount time;
    -- only the error itself waits for the first call.
    checkedToolset ts = case duplicateToolNamesIn ts of
        [] -> Right ts
        names -> Left names

    rejected names = Agent $ \_history _input ->
        liftIO . throwIO . AgentConfigurationError $
            "duplicate tool names: " <> T.intercalate ", " names

    -- One call: a fresh model session seeded with @history@, the
    -- definition's system prompt and loop, and the conversation it leaves.
    wired ts = Agent $ \history input ->
        runModelRuntime runtime $ do
            seedSession history
            answer <-
                runLoopFor
                    (definitionOpts definition)
                    [hoistTool raise t | t <- toolsetTools ts]
                    (definitionPrompt definition input)
                    (definitionOutput definition)
            (answer,) <$> getHistory

    seedSession history = do
        setHistory history
        maybe clearSystem setSystem (definitionSystem definition)

{- | A definition's output kind selects its conversation loop. Private until
a third output mode earns it a seam.
-}
runLoopFor ::
    (LLM :> es, IOE :> es) =>
    AgentOpts ->
    [Tool (Eff es)] ->
    Text ->
    AgentOutput output ->
    Eff es output
runLoopFor opts available prompt output = case output of
    TextOutput -> runTextLoopWith opts available prompt
    StructuredOutput -> runStructuredLoopWith opts available prompt

-- | Call an agent. Each call starts a fresh conversation.
runAgent :: Agent es input output -> input -> Eff es output
runAgent agent input = fst <$> runAgentFrom agent [] input

-- | Run an agent with a fresh conversation. Friendly spelling of 'runAgent'.
invoke :: Agent es input output -> input -> Eff es output
invoke = runAgent

{- | Return a stateful twin of an agent. The twin has the same type and stays
callable, but keeps one conversation across calls. Its own calls serialize;
different sessions run concurrently. The history argument every agent accepts
is ignored here: the twin owns its history.
-}
session :: (IOE :> es) => Agent es input output -> Eff es (Agent es input output)
session agent = do
    historyVar <- liftIO (newMVar [])
    let callable _staleHistory input =
            withEffToIO (ConcUnlift Ephemeral Unlimited) $ \unlift ->
                modifyMVar historyVar $ \history -> do
                    (result, nextHistory) <- unlift (runAgentFrom agent history input)
                    pure (nextHistory, (result, nextHistory))
    pure (Agent callable)

{- | Start an explicit persistent conversation. Calls to one session are
serialized, while different sessions can run concurrently.
-}
start :: (IOE :> es) => Agent es input output -> Eff es (Session es input output)
start agent = do
    twin <- session agent
    pure (Session (runAgent twin))

-- | Continue an explicit persistent conversation.
continue :: Session es input output -> input -> Eff es output
continue = runSession

--------------------------------------------------------------------------------
-- Low-level model-session loops. They operate inside an already-established
-- 'LLM' scope; the configured-agent machinery above is the friendly surface.
--------------------------------------------------------------------------------

-- | Text tool loop with default 'AgentOpts'.
runTextLoop :: (LLM :> es, IOE :> es) => [Tool (Eff es)] -> Text -> Eff es Text
runTextLoop = runTextLoopWith defaultAgentOpts

runTextLoopWith :: (LLM :> es, IOE :> es) => AgentOpts -> [Tool (Eff es)] -> Text -> Eff es Text
runTextLoopWith opts availableTools instruction = withTransaction $ do
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
                        mapM_ (executeAndRecord availableTools) calls
                        when (currentCalls == previousCalls && not (null currentCalls)) $
                            pushMessage (UserMsg repeatedToolWarning)
                        loop (roundsLeft - 1) currentCalls

-- | Structured tool loop with default 'AgentOpts'.
runStructuredLoop ::
    forall output es.
    (LLM :> es, IOE :> es, FromJSON output, ToSchema output) =>
    [Tool (Eff es)] ->
    Text ->
    Eff es output
runStructuredLoop = runStructuredLoopWith defaultAgentOpts

runStructuredLoopWith ::
    forall output es.
    (LLM :> es, IOE :> es, FromJSON output, ToSchema output) =>
    AgentOpts ->
    [Tool (Eff es)] ->
    Text ->
    Eff es output
runStructuredLoopWith opts availableTools instruction = withTransaction $ do
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
                        mapM_ (executeAndRecord availableTools) calls
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

{- | Run one tool call and append its result to the conversation. Errors are
recorded as feedback for the model rather than aborting the round.
-}
executeAndRecord :: (LLM :> es) => [Tool (Eff es)] -> ToolCall -> Eff es ()
executeAndRecord availableTools call = do
    result <- case find ((== toolCallName call) . toolSpecName . toolSpec) availableTools of
        Nothing -> pure (Left ("unknown tool: " <> toolCallName call))
        Just selected -> toolRun selected (toolCallArguments call)
    let value = either (object . pure . ("error" .=)) id result
        content = decodeUtf8With lenientDecode . LBS.toStrict . encode $ value
    pushMessage (ToolMsg (toolCallId call) content)

repeatedToolWarning :: Text
repeatedToolWarning =
    "Warning: Repeated identical tool call signature detected. Please adjust your plan or return the final answer."

--------------------------------------------------------------------------------
-- Composition. Agents are functions between Haskell types, so standard
-- classes give pipelines without bespoke combinators. Instances sequence the
-- engine face: each stage starts where the previous stage's conversation ended,
-- which for independent stages behaves exactly like fresh conversations.
--------------------------------------------------------------------------------

instance Functor (Agent es input) where
    fmap f (Agent step) = Agent $
        \history input -> first f <$> step history input

instance Applicative (Agent es input) where
    pure x = Agent (\history _input -> pure (x, history))
    Agent sf <*> Agent sx = Agent $ \history input -> do
        (f, afterF) <- sf history input
        (x, afterX) <- sx afterF input
        pure (f x, afterX)

instance Monad (Agent es input) where
    Agent step >>= f = Agent $ \history input -> do
        (output, history') <- step history input
        runAgentFrom (f output) history' input

instance Category (Agent es) where
    id = Agent $ \history input -> pure (input, history)
    Agent g . Agent f = Agent $ \history input -> do
        (middle, history') <- f history input
        g history' middle
