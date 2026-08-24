{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
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
  , withCacheModel
  , isCacheableResponse
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
  , withTransaction

    -- * Prompt Helpers & Message Algebra
  , embed
  , embedShow
  , Prompt (..)
  , fewShot
  , user
  , assistant
  , system
  , toolResult
  , ToPromptArg (..)

    -- * Streaming
  , streamSSE

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
  , ToolIO
  , Toolset
  , tools
  , noTools
  , tool
  , tool'
  , toolSync
  , mkTool
  , liftTool
  , hoistTool
  , useTools
  , useToolsWith
  , runAgent
  , runAgentWith
  , runAgentStructured
  , runAgentStructuredWith
  , AgentOpts (..)
  , defaultAgentOpts
  , Agent
  , AgentDef
  , textAgent
  , structuredAgent
  , withAgentOpts
  , bind
  , invoke
  , Session
  , start
  , continue

    -- * Model Runtimes
  , ModelRuntime
  , model
  , modelWithConfig
  , mockModel

    -- * Workflow Concurrency
  , concurrently
  , race
  , mapConcurrentlyN
  , WorkflowError (..)

    -- * Standard Coding Tools
  , module LLMonad.Tools.Coding

    -- * Template Haskell
  , prompt
  , makeTool
  , makeToolNamed

    -- * Execution Sandboxing & World Effect
  , module LLMonad.World
  , module LLMonad.World.Local
  , module LLMonad.World.Worktree
  , module LLMonad.World.Memory

    -- * Session Persistence & Journal Effect
  , Journal (..)
  , JournalEvent (TurnStarted, ModelTurn, ToolInvoked, ToolCompleted, MetricsReported, TurnFinished)
  , pattern JournalUserMsg
  , pattern ModelTurnSimple
  , pattern ToolInvokedSimple
  , ModelMetrics (..)
  , ReplaySummary (..)
  , JournalState (..)
  , recordEvent
  , getEvents
  , clearEvents
  , recordUserMsg
  , recordModelTurn
  , recordModelTurnWithCalls
  , recordToolCall
  , recordToolCallWithId
  , recordToolResult
  , recordMetrics
  , recordTurnStart
  , recordTurnFinish
  , runJournalFile
  , runJournalFileWithEvents
  , runJournalFileWorld
  , runJournalMemory
  , runJournalMemoryWithState
  , runJournalMemorySimple
  , resumeSession
  , resumeSessionWorld
  , replayAudit
  , replayAuditSummary
  , loadJournalFile
  , loadJournalFileWorld
  , loadJournalText
  , reconstructChatHistory
  ) where

import LLMonad.API
import LLMonad.Agent
import LLMonad.Core
import LLMonad.Interpreter.HTTP
import LLMonad.Interpreter.Mock
import LLMonad.Journal
import LLMonad.Journal.File
import LLMonad.Journal.Memory
import LLMonad.Middleware.Cache
import LLMonad.Middleware.RateLimit
import LLMonad.Middleware.Trace
import LLMonad.Model
import LLMonad.Prompt
import LLMonad.Provider
import LLMonad.Providers.Anthropic
import LLMonad.Providers.OpenAICompatible
import LLMonad.Schema
import LLMonad.Streaming
import LLMonad.Structured
import LLMonad.TH
import LLMonad.Tools
import LLMonad.Tools.Coding
import LLMonad.Types
import LLMonad.World
import LLMonad.World.Local
import LLMonad.World.Memory
import LLMonad.World.Worktree
import LLMonad.Workflow

