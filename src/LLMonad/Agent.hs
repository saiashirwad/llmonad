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
import Data.Aeson (FromJSON, Value, encode, object, parseJSON, (.=))
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
import LLMonad.Model (ModelRuntime, runModelRuntime)
import LLMonad.Schema (ToSchema (..))
import LLMonad.Structured (decodeFeedback, decodePayloadOrText)
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
        | roundsLeft <= 0 = exhausted
        | otherwise = do
            response <- chatRound (agentParams opts) RfText specs ToolAuto
            case crspToolCalls response of
                [] -> pure (crspText response)
                calls
                    -- Results from this batch would have no round left in
                    -- which to be consumed, so executing them is pointless.
                    | roundsLeft <= 1 -> exhausted
                    | otherwise -> do
                        recordToolBatch availableTools previousCalls calls
                        loop (roundsLeft - 1) (callSignatures calls)

    exhausted = liftIO (throwIO (AgentRoundsExhausted (agentMaxRounds opts)))

-- | Structured tool loop with default 'AgentOpts'.
runStructuredLoop ::
    forall output es.
    (LLM :> es, IOE :> es, FromJSON output, ToSchema output) =>
    [Tool (Eff es)] ->
    Text ->
    Eff es output
runStructuredLoop = runStructuredLoopWith defaultAgentOpts

-- | How the model is asked for its final answer during one structured run.
data StructuredPhase
    = -- | Tools are offered; answers arrive as plain text.
      WithTools
    | -- | No tools are offered and the JSON schema is forced server-side.
      SchemaForced

{- | Structured tool loop. Two phases: 'WithTools', where tool calls extend
the conversation, and 'SchemaForced'. A run starts in 'SchemaForced' when no
tools are configured, and any undecodable answer demotes the rest of the run
to it — retries never leave that phase.
-}
runStructuredLoopWith ::
    forall output es.
    (LLM :> es, IOE :> es, FromJSON output, ToSchema output) =>
    AgentOpts ->
    [Tool (Eff es)] ->
    Text ->
    Eff es output
runStructuredLoopWith opts availableTools instruction = withTransaction $ do
    pushMessage (UserMsg instruction)
    drive startPhase (agentMaxRounds opts) []
  where
    specs = map toolSpec availableTools
    schemaFormat = RfJsonSchema (schemaTypeName @output) (toSchema @output) True

    -- A run starts in 'WithTools' unless no tools are configured.
    startPhase
        | null availableTools = SchemaForced
        | otherwise = WithTools

    drive :: StructuredPhase -> Int -> [(Text, Value)] -> Eff es output
    drive phase roundsLeft previousCalls
        | roundsLeft <= 0 = exhausted
        | otherwise = do
            response <- request phase
            case phase of
                SchemaForced -> settle roundsLeft response
                WithTools -> case crspToolCalls response of
                    [] -> settle roundsLeft response
                    calls
                        -- Results from this batch would have no round left
                        -- in which to be consumed, so executing is pointless.
                        | roundsLeft <= 1 -> exhausted
                        | otherwise -> do
                            recordToolBatch availableTools previousCalls calls
                            drive phase (roundsLeft - 1) (callSignatures calls)

    request WithTools = chatRound (agentParams opts) RfText specs ToolAuto
    request SchemaForced = chatRound (agentParams opts) schemaFormat [] ToolAuto

    -- Finish with @response@ if it decodes; otherwise spend a round telling
    -- the model what went wrong and demote the rest of the run to the
    -- schema-forced phase.
    settle roundsLeft response =
        case decodePayloadOrText response of
            Right output -> pure output
            Left detail
                | roundsLeft <= 1 -> liftIO (throwIO (DecodeError detail (crspText response)))
                | otherwise -> do
                    pushMessage . UserMsg $ decodeFeedback "final" detail
                    drive SchemaForced (roundsLeft - 1) []

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

{- | Run one batch of tool calls, recording each result as a message, and warn
the model when the batch repeats the previous batch verbatim. Callers only
pass non-empty batches.
-}
recordToolBatch :: (LLM :> es) => [Tool (Eff es)] -> [(Text, Value)] -> [ToolCall] -> Eff es ()
recordToolBatch availableTools previousCalls calls = do
    mapM_ (executeAndRecord availableTools) calls
    when (callSignatures calls == previousCalls) $
        pushMessage (UserMsg repeatedToolWarning)

-- | What identifies a tool call for repeat detection: name plus raw arguments.
callSignatures :: [ToolCall] -> [(Text, Value)]
callSignatures calls = [(toolCallName call, toolCallArguments call) | call <- calls]

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
