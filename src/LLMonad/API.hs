{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | High-level curried functional API for LLMonad.
module LLMonad.API (
    AskFunction (..),
    ask,
    ask',
) where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import LLMonad.Core (LLM)
import LLMonad.Schema (ToSchema)
import LLMonad.Structured (askStructured)

-- | Typeclass driving curried function execution for LLM queries.
class AskFunction es fn a | fn -> es a where
    askApply :: Text -> [Text] -> fn

instance (LLM :> es, FromJSON a, ToSchema a) => AskFunction es (Eff es a) a where
    askApply t args =
        let fullPrompt = if null args then t else t <> ":\n" <> T.unwords args
         in askStructured fullPrompt

instance (AskFunction es fn a) => AskFunction es (Text -> fn) a where
    askApply t args nextArg = askApply @es @fn @a t (args ++ [nextArg])

-- | Curried ask combinator with return type as the first visible type application.
ask :: forall a es fn. (AskFunction es fn a) => Text -> fn
ask t = askApply @es @fn @a t []

-- | Curried ask' combinator for prompt templates with multiple parameters.
ask' :: forall a es fn. (AskFunction es fn a) => Text -> fn
ask' t = askApply @es @fn @a t []
