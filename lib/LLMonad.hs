{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module LLMonad
  ( -- * Core types
    LLM,
    LLMEnv (..),
    runLLM,
    
    -- * API functions
    ask,
    ask',
    
    -- * Schema class
    Schema (..),
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.ByteString.Lazy.Char8 qualified as LB
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import LLMonad.Internal.Client (callLLM, LLMEnv (..))
import LLMonad.Internal.Schema (Schema (..))

-- | LLM monad for composable AI operations
newtype LLM a = LLM {unLLM :: ReaderT LLMEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader LLMEnv)

-- | Run an LLM computation with the given environment
runLLM :: LLMEnv -> LLM a -> IO a
runLLM env = flip runReaderT env . unLLM

-- | Query the LLM with a prompt and input data
ask :: forall a b. (FromJSON a, ToJSON a, Schema a, ToJSON b) => Text -> b -> LLM a
ask stem input =
  let encodedInput = decodeUtf8 (LB.toStrict $ encode input)
      fullPrompt = stem <> "\n\nInput: " <> encodedInput
   in callLLM fullPrompt

-- | Query the LLM with a prompt and two input arguments
ask' :: forall a b c. (FromJSON a, ToJSON a, Schema a, ToJSON b, ToJSON c) => Text -> b -> c -> LLM a
ask' stem a b =
  let firstEncoded = decodeUtf8 (LB.toStrict $ encode a)
      secondEncoded = decodeUtf8 (LB.toStrict $ encode b)
      fullPrompt = stem <> "\n\nFirst: " <> firstEncoded <> "\nSecond: " <> secondEncoded
   in callLLM fullPrompt
