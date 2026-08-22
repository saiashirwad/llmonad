{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Live HTTP interpreter for the LLM effect.
module LLMonad.Interpreter.HTTP
  ( LLMConfig (..)
  , HTTPState (..)
  , defaultConfig
  , runLLMHTTP
  , runLLMHTTPWithState
  ) where

import Control.Exception (throwIO)
import Data.IORef
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import LLMonad.Core
import LLMonad.Provider
import LLMonad.Types

-- | Session configuration for LLM execution.
data LLMConfig = LLMConfig
  { configProvider :: Provider
  , configModel    :: Model
  , configParams   :: Params
  }

instance Show LLMConfig where
  show c = "LLMConfig { configProvider = " <> show (providerName (configProvider c)) <> ", configModel = " <> show (configModel c) <> ", ... }"

-- | Point a config at a provider and model with default parameters.
defaultConfig :: Provider -> Model -> LLMConfig
defaultConfig p m = LLMConfig p m defaultParams

data HTTPState = HTTPState
  { hsSystem  :: Maybe Text
  , hsHistory :: [ChatMessage]
  }

-- | Run the LLM dynamic effect with live HTTP requests.
runLLMHTTP :: (IOE :> es) => LLMConfig -> Eff (LLM : es) a -> Eff es a
runLLMHTTP cfg action = do
  stateRef <- liftIO (newIORef (HTTPState Nothing []))
  interpret (httpHandler cfg stateRef) action

-- | Run the LLM dynamic effect with explicit initial state and return final state.
runLLMHTTPWithState ::
  (IOE :> es) =>
  LLMConfig ->
  Maybe Text ->
  [ChatMessage] ->
  Eff (LLM : es) a ->
  Eff es (a, Maybe Text, [ChatMessage])
runLLMHTTPWithState cfg initSys initHist action = do
  stateRef <- liftIO (newIORef (HTTPState initSys initHist))
  res <- interpret (httpHandler cfg stateRef) action
  HTTPState finalSys finalHist <- liftIO (readIORef stateRef)
  pure (res, finalSys, finalHist)

httpHandler :: (IOE :> es) => LLMConfig -> IORef HTTPState -> EffectHandler LLM es
httpHandler cfg stateRef _ = \case
  GetHistory -> liftIO (hsHistory <$> readIORef stateRef)
  SetHistory msgs -> liftIO (modifyIORef' stateRef (\s -> s { hsHistory = msgs }))
  PushMessage msg -> liftIO (modifyIORef' stateRef (\s -> s { hsHistory = hsHistory s ++ [msg] }))
  ClearHistory -> liftIO (modifyIORef' stateRef (\s -> s { hsHistory = [] }))
  GetSystem -> liftIO (hsSystem <$> readIORef stateRef)
  SetSystem sys -> liftIO (modifyIORef' stateRef (\s -> s { hsSystem = Just sys }))
  ClearSystem -> liftIO (modifyIORef' stateRef (\s -> s { hsSystem = Nothing }))

  ChatRound callParams fmt specs choice -> do
    HTTPState sys hist <- liftIO (readIORef stateRef)
    let effectiveParams = configParams cfg `overrideParams` callParams
        req = CompletionRequest
          { crModel = configModel cfg
          , crSystem = sys
          , crMessages = hist
          , crParams = effectiveParams
          , crTools = specs
          , crToolChoice = choice
          , crResponseFormat = fmt
          }
    result <- liftIO (providerComplete (configProvider cfg) req)
    case result of
      Left err -> liftIO (throwIO err)
      Right resp -> do
        let assistantMsg = AssistantMsg (crspText resp) (crspToolCalls resp)
        liftIO (modifyIORef' stateRef (\s -> s { hsHistory = hsHistory s ++ [assistantMsg] }))
        pure resp

  StreamRound callParams fmt specs cb -> do
    HTTPState sys hist <- liftIO (readIORef stateRef)
    let effectiveParams = configParams cfg `overrideParams` callParams
        req = CompletionRequest
          { crModel = configModel cfg
          , crSystem = sys
          , crMessages = hist
          , crParams = effectiveParams
          , crTools = specs
          , crToolChoice = ToolAuto
          , crResponseFormat = fmt
          }
    result <- liftIO (providerStream (configProvider cfg) req cb)
    case result of
      Left err -> liftIO (throwIO err)
      Right resp -> do
        let assistantMsg = AssistantMsg (crspText resp) (crspToolCalls resp)
        liftIO (modifyIORef' stateRef (\s -> s { hsHistory = hsHistory s ++ [assistantMsg] }))
        pure resp
