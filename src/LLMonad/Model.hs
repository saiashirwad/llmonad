{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | Configured model adapters for first-class agents.
module LLMonad.Model (
    ModelRuntime,
    model,
    modelWithConfig,
    mockModel,
    runModelRuntime,
) where

import Effectful (Eff, IOE, (:>))
import LLMonad.Core (LLM)
import LLMonad.Error (LLMError)
import LLMonad.Interpreter.HTTP (LLMConfig, defaultConfig, runLLMHTTP)
import LLMonad.Interpreter.Mock (runLLMMock)
import LLMonad.Provider (Provider)
import LLMonad.Types (CompletionResponse, Model)

-- | A model and its adapter. Each run creates a fresh model session.
newtype ModelRuntime es = ModelRuntime
    { runModelRuntime :: forall a. Eff (LLM : es) a -> Eff es a
    }

-- | Configure a provider and model name with default request parameters.
model :: (IOE :> es) => Provider -> Model -> ModelRuntime es
model provider modelName = modelWithConfig (defaultConfig provider modelName)

-- | Configure a model with an explicit HTTP configuration.
modelWithConfig :: (IOE :> es) => LLMConfig -> ModelRuntime es
modelWithConfig config = ModelRuntime (runLLMHTTP config)

{- | Configure a deterministic model script. The script restarts for each
invocation, which keeps independent agent calls deterministic.
-}
mockModel :: [Either LLMError CompletionResponse] -> ModelRuntime es
mockModel script = ModelRuntime (fmap fst . runLLMMock script)
