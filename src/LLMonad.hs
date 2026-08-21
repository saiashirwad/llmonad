{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Top-level LLMonad interface.
module LLMonad
  ( -- * Core Effect & Operations
    LLM (..)
  , chatRound
  , streamRound
  , getHistory
  , setHistory
  , pushMessage
  , clearHistory
  , getSystem
  , setSystem
  , clearSystem

    -- * Text Generation Helpers
  , generateText
  , generateTextWith
  , streamText
  , streamTextWith

    -- * Interpreters
  , LLMConfig (..)
  , defaultConfig
  , runLLMHTTP
  , runLLMHTTPWithState
  , runLLMMock
  , runLLMMockFull
  , textResp
  , toolResp
  , structuredResp

    -- * Middleware
  , withCache
  , CacheStore (..)
  , newInMemoryCache
  , withTrace
  , Trace (..)
  , withRateLimit
  , RateLimiter (..)
  , newRateLimiter

    -- * Providers
  , Provider (..)
  , StructuredMode (..)
  , nonStreamingFallback
  , module LLMonad.Providers.OpenAICompatible
  , module LLMonad.Providers.Anthropic

    -- * Types
  , Model (..)
  , Role (..)
  , ToolCall (..)
  , ChatMessage (..)
  , Params (..)
  , defaultParams
  , overrideParams
  , ResponseFormat (..)
  , ToolSpec (..)
  , ToolChoice (..)
  , CompletionRequest (..)
  , CompletionResponse (..)
  , FinishReason (..)
  , Usage (..)
  , StreamEvent (..)

    -- * Error Handling & Recovery
  , LLMError (..)
  , isTransient
  , prettyError
  , attempt
  , retry

    -- * Prompt Helpers
  , embed
  , embedShow

    -- * Schema & Structured Output
  , ToSchema (..)
  , HasSchema
  , schema
  , schemaName
  , askStructured
  , extractWithRetry

    -- * Curried Functional API
  , AskFunction (..)
  , ask
  , ask'

    -- * Tools & Agent
  , Tool (..)
  , tool
  , tool'
  , toolSync
  , mkTool
  , useTools
  , useToolsWith
  , AgentOpts (..)
  , defaultAgentOpts
  ) where

import LLMonad.API
import LLMonad.Core
import LLMonad.Interpreter.HTTP
import LLMonad.Interpreter.Mock
import LLMonad.Middleware.Cache
import LLMonad.Middleware.RateLimit
import LLMonad.Middleware.Trace
import LLMonad.Provider
import LLMonad.Providers.Anthropic
import LLMonad.Providers.OpenAICompatible
import LLMonad.Schema
import LLMonad.Structured
import LLMonad.Tools
import LLMonad.Types

