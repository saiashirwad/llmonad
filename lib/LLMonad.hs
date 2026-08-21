{-# LANGUAGE OverloadedStrings #-}

module LLMonad
  ( -- * Core Monad & Transformer
    LLM,
    LLMT (..),
    MonadLLM (..),
    MonadIO (..),

    -- * Configuration & State
    LLMConfig (..),
    defaultConfig,
    withModel,
    withTemperature,
    withMaxTokens,
    withSystemPrompt,
    withMaxRetries,
    withManager,
    LLMState (..),
    initState,
    initStateWithTools,

    -- * Execution Runners
    runLLM,
    runLLM_,
    runLLMWith,
    evalLLM,
    execLLM,
    runLLMWithHistory,

    -- * Conversational & Prompt DSL
    ask,
    ask_,
    tell,
    reply,
    reset,
    getConversation,
    user,
    system,
    assistant,
    toolResult,
    fewShot,
    renderTemplate,

    -- * Structured Output Extraction
    askStructured,
    generateObject,
    generateObjectWithRetry,

    -- * Type-Driven JSON Schema
    HasSchema (..),
    JSONSchema (..),
    schemaToValue,
    schemaToOpenAISchema,

    -- * Tool Definition & Function Calling
    Tool (..),
    defTool,
    defToolSync,

    -- * Autonomous ReAct Agents
    runAgent,
    runAgentWith,
    runAgentWithMaxSteps,
    stepAgent,

    -- * Real-Time Streaming
    streamChat,
    streamChatWithMessages,

    -- * Providers & Protocol Adapters
    Provider (..),
    Protocol (..),
    Auth (..),
    deepseek,
    openai,
    anthropic,
    openrouter,
    ollama,
    together,
    mistral,
    groq,
    openaiCompatible,
    anthropicCompatible,

    -- * Provider Combinators & Inspectors
    withBaseUrl,
    withAuth,
    withDefaultModel,
    withHeader,
    providerChatEndpoint,
    providerAuthHeaders,
    isAnthropicProtocol,
    supportsJsonSchema,

    -- * Domain Types
    Model (..),
    Role (..),
    Message (..),
    Prompt (..),
    toPrompt,
    ToolCall (..),
    FunctionCall (..),
    ChatRequest (..),
    ChatResponse (..),
    Choice (..),
    FinishReason (..),
    Usage (..),
    extractTextContent,

    -- * Error Handling
    LLMError (..),
  )
where

import LLMonad.Agent
import LLMonad.Client
import LLMonad.Core
import LLMonad.Prompt
import LLMonad.Provider
import LLMonad.Schema
import LLMonad.Streaming
import LLMonad.Structured
import LLMonad.Tools
import LLMonad.Types
