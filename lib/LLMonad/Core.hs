{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module LLMonad.Core
  ( -- * Monad & Transformer
    LLMT (..),
    LLM,
    MonadLLM (..),
    MonadIO (..),

    -- * Configuration
    LLMConfig (..),
    defaultConfig,
    withModel,
    withTemperature,
    withMaxTokens,
    withSystemPrompt,
    withMaxRetries,
    withManager,

    -- * State
    LLMState (..),
    initState,
    initStateWithTools,

    -- * Runners
    runLLM,
    runLLM_,
    runLLMWith,
    evalLLM,
    execLLM,
    runLLMWithHistory,
  )
where

import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), ask, runReaderT)
import Control.Monad.State (MonadState, StateT (..), evalStateT, execStateT, gets, modify, runStateT)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import LLMonad.Client
import LLMonad.Provider
import LLMonad.Tools
import LLMonad.Types
import Network.HTTP.Client (Manager)

-- | Configuration settings for LLMonad executions
data LLMConfig = LLMConfig
  { configProvider :: Provider,
    configModel :: Model,
    configTemperature :: Maybe Double,
    configMaxTokens :: Maybe Int,
    configSystemPrompt :: Maybe Text,
    configMaxRetries :: Int,
    configHttpManager :: Maybe Manager
  }

-- | Default configuration derived from a Provider
defaultConfig :: Provider -> LLMConfig
defaultConfig p =
  LLMConfig
    { configProvider = p,
      configModel = maybe "default" id (providerDefaultModel p),
      configTemperature = Nothing,
      configMaxTokens = Nothing,
      configSystemPrompt = Nothing,
      configMaxRetries = 3,
      configHttpManager = Nothing
    }

-- | Override the model
withModel :: Model -> LLMConfig -> LLMConfig
withModel m cfg = cfg {configModel = m}

-- | Set the temperature (e.g. 0.0 for deterministic, 0.7 for creative)
withTemperature :: Double -> LLMConfig -> LLMConfig
withTemperature t cfg = cfg {configTemperature = Just t}

-- | Set max tokens for generation
withMaxTokens :: Int -> LLMConfig -> LLMConfig
withMaxTokens mt cfg = cfg {configMaxTokens = Just mt}

-- | Set a default system prompt
withSystemPrompt :: Text -> LLMConfig -> LLMConfig
withSystemPrompt sp cfg = cfg {configSystemPrompt = Just sp}

-- | Set maximum self-correction retry attempts
withMaxRetries :: Int -> LLMConfig -> LLMConfig
withMaxRetries r cfg = cfg {configMaxRetries = r}

-- | Set custom HTTP Manager
withManager :: Manager -> LLMConfig -> LLMConfig
withManager mgr cfg = cfg {configHttpManager = Just mgr}

-- | Runtime state tracking conversation history and registered tools
data LLMState = LLMState
  { stateHistory :: [Message],
    stateTools :: Map Text Tool
  }

-- | Initial empty state
initState :: LLMState
initState = LLMState {stateHistory = [], stateTools = Map.empty}

-- | Initial state with pre-registered tools
initStateWithTools :: [Tool] -> LLMState
initStateWithTools ts =
  LLMState
    { stateHistory = [],
      stateTools = Map.fromList [(toolName t, t) | t <- ts]
    }

-- | The LLMT monad transformer
newtype LLMT m a = LLMT
  { unLLMT :: ReaderT LLMConfig (StateT LLMState (ExceptT LLMError m)) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader LLMConfig,
      MonadState LLMState,
      MonadError LLMError
    )

instance MonadTrans LLMT where
  lift = LLMT . lift . lift . lift

-- | The primary LLM monad for IO computations
type LLM a = LLMT IO a

-- | Type class for abstracting over LLM interactions
class (Monad m) => MonadLLM m where
  chat :: [Message] -> m ChatResponse
  getHistory :: m [Message]
  setHistory :: [Message] -> m ()
  appendHistory :: Message -> m ()
  clearHistory :: m ()
  registerTool :: Tool -> m ()
  getTools :: m [Tool]

instance (MonadIO m) => MonadLLM (LLMT m) where
  chat msgs = do
    LLMConfig {..} <- ask
    toolsMap <- gets stateTools

    let allMsgs = case configSystemPrompt of
          Just sysPrompt ->
            if any (\m -> messageRole m == SystemRole) msgs
              then msgs
              else systemMsg sysPrompt : msgs
          Nothing -> msgs

        toolsList = Map.elems toolsMap
        toolsValue =
          if null toolsList
            then Nothing
            else
              Just $
                if isAnthropicProtocol configProvider
                  then map toolToAnthropicTool toolsList
                  else map toolToOpenAITool toolsList

        chatReq =
          ChatRequest
            { reqModel = configModel,
              reqMessages = allMsgs,
              reqTemperature = configTemperature,
              reqMaxTokens = configMaxTokens,
              reqStream = False,
              reqTools = toolsValue,
              reqResponseFormat = Nothing
            }

    respOrErr <- liftIO $ case configHttpManager of
      Just mgr -> executeChatRequestWithManager mgr configProvider chatReq
      Nothing -> executeChatRequest configProvider chatReq

    case respOrErr of
      Left err -> throwError err
      Right resp -> pure resp

  getHistory = gets stateHistory

  setHistory h = modify (\s -> s {stateHistory = h})

  appendHistory msg = modify (\s -> s {stateHistory = stateHistory s ++ [msg]})

  clearHistory = modify (\s -> s {stateHistory = []})

  registerTool t = modify (\s -> s {stateTools = Map.insert (toolName t) t (stateTools s)})

  getTools = gets (Map.elems . stateTools)

-- ============================================================================
-- Runners
-- ============================================================================

-- | Run an LLM computation with a configuration, returning Either LLMError a
runLLM :: LLMConfig -> LLM a -> IO (Either LLMError a)
runLLM cfg act =
  runExceptT (evalStateT (runReaderT (unLLMT act) cfg) initState)

-- | Run an LLM computation, throwing runtime error on failure
runLLM_ :: LLMConfig -> LLM a -> IO a
runLLM_ cfg act = do
  res <- runLLM cfg act
  case res of
    Left err -> errorWithoutStackTrace ("LLMonad Error: " <> show err)
    Right a -> pure a

-- | Run an LLM computation with initial tools
runLLMWith :: LLMConfig -> [Tool] -> LLM a -> IO (Either LLMError a)
runLLMWith cfg tools act =
  runExceptT (evalStateT (runReaderT (unLLMT act) cfg) (initStateWithTools tools))

-- | Evaluate an LLM computation with initial state
evalLLM :: LLMConfig -> LLMState -> LLM a -> IO (Either LLMError a)
evalLLM cfg st act =
  runExceptT (evalStateT (runReaderT (unLLMT act) cfg) st)

-- | Execute an LLM computation, returning the final state
execLLM :: LLMConfig -> LLMState -> LLM a -> IO (Either LLMError LLMState)
execLLM cfg st act =
  runExceptT (execStateT (runReaderT (unLLMT act) cfg) st)

-- | Run an LLM computation with initial history, returning result and updated history
runLLMWithHistory :: LLMConfig -> [Message] -> LLM a -> IO (Either LLMError (a, [Message]))
runLLMWithHistory cfg initialHistory act = do
  let st = LLMState {stateHistory = initialHistory, stateTools = Map.empty}
  res <- runExceptT (runStateT (runReaderT (unLLMT act) cfg) st)
  case res of
    Left err -> pure (Left err)
    Right (a, finalSt) -> pure (Right (a, stateHistory finalSt))
