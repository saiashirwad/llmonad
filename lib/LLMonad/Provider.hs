{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LLMonad.Provider
  ( -- * Core Provider Model
    Provider (..),
    Protocol (..),
    Auth (..),

    -- * Provider Presets
    deepseek,
    openai,
    anthropic,
    openrouter,
    ollama,
    together,
    mistral,
    groq,

    -- * Generic Protocol Adapters
    openaiCompatible,
    anthropicCompatible,

    -- * Provider Combinators
    withBaseUrl,
    withAuth,
    withDefaultModel,
    withHeader,

    -- * Protocol Inspection
    providerChatEndpoint,
    providerAuthHeaders,
    isAnthropicProtocol,
    supportsJsonSchema,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLMonad.Types (Model (..))

-- | Protocol dialect used by the LLM backend
data Protocol
  = OpenAICompat
  | AnthropicMessages
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Authentication strategy for the provider
data Auth
  = BearerAuth Text
  | ApiKeyHeader Text Text
  | NoAuth
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | First-class Provider configuration
data Provider = Provider
  { providerProtocol :: Protocol,
    providerBaseUrl :: Text,
    providerAuth :: Auth,
    providerDefaultModel :: Maybe Model,
    providerHeaders :: [(Text, Text)]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ============================================================================
-- Combinators
-- ============================================================================

-- | Override the base URL of a provider
withBaseUrl :: Text -> Provider -> Provider
withBaseUrl url p = p {providerBaseUrl = url}

-- | Set the authentication strategy of a provider
withAuth :: Auth -> Provider -> Provider
withAuth a p = p {providerAuth = a}

-- | Set the default model of a provider
withDefaultModel :: Model -> Provider -> Provider
withDefaultModel m p = p {providerDefaultModel = Just m}

-- | Add a custom HTTP header
withHeader :: Text -> Text -> Provider -> Provider
withHeader k v p = p {providerHeaders = (k, v) : providerHeaders p}

-- ============================================================================
-- Standard Presets
-- ============================================================================

-- | DeepSeek (V3 / R1)
deepseek :: Text -> Provider
deepseek key =
  Provider
    { providerProtocol = OpenAICompat,
      providerBaseUrl = "https://api.deepseek.com",
      providerAuth = BearerAuth key,
      providerDefaultModel = Just "deepseek-chat",
      providerHeaders = []
    }

-- | OpenAI (GPT-4o, GPT-4o-mini, o1)
openai :: Text -> Provider
openai key =
  Provider
    { providerProtocol = OpenAICompat,
      providerBaseUrl = "https://api.openai.com",
      providerAuth = BearerAuth key,
      providerDefaultModel = Just "gpt-4o-mini",
      providerHeaders = []
    }

-- | Anthropic (Claude 3.5 Sonnet, Claude 3.5 Haiku, Claude 3 Opus)
anthropic :: Text -> Provider
anthropic key =
  Provider
    { providerProtocol = AnthropicMessages,
      providerBaseUrl = "https://api.anthropic.com",
      providerAuth = ApiKeyHeader "x-api-key" key,
      providerDefaultModel = Just "claude-3-5-sonnet-20241022",
      providerHeaders = [("anthropic-version", "2023-06-01")]
    }

-- | OpenRouter
openrouter :: Text -> Provider
openrouter key =
  Provider
    { providerProtocol = OpenAICompat,
      providerBaseUrl = "https://openrouter.ai/api",
      providerAuth = BearerAuth key,
      providerDefaultModel = Just "deepseek/deepseek-chat",
      providerHeaders = []
    }

-- | Local Ollama instance
ollama :: Provider
ollama =
  Provider
    { providerProtocol = OpenAICompat,
      providerBaseUrl = "http://localhost:11434",
      providerAuth = NoAuth,
      providerDefaultModel = Just "llama3.2",
      providerHeaders = []
    }

-- | Together AI
together :: Text -> Provider
together key =
  Provider
    { providerProtocol = OpenAICompat,
      providerBaseUrl = "https://api.together.xyz",
      providerAuth = BearerAuth key,
      providerDefaultModel = Just "meta-llama/Llama-3.3-70B-Instruct-Turbo",
      providerHeaders = []
    }

-- | Mistral AI
mistral :: Text -> Provider
mistral key =
  Provider
    { providerProtocol = OpenAICompat,
      providerBaseUrl = "https://api.mistral.ai",
      providerAuth = BearerAuth key,
      providerDefaultModel = Just "mistral-large-latest",
      providerHeaders = []
    }

-- | Groq
groq :: Text -> Provider
groq key =
  Provider
    { providerProtocol = OpenAICompat,
      providerBaseUrl = "https://api.groq.com/openai",
      providerAuth = BearerAuth key,
      providerDefaultModel = Just "llama-3.3-70b-versatile",
      providerHeaders = []
    }

-- ============================================================================
-- Generic Protocol Adapters
-- ============================================================================

-- | Connect to any OpenAI-compatible API endpoint (vLLM, LocalAI, LM Studio, Perplexity, custom proxies)
openaiCompatible :: Text -> Maybe Text -> Provider
openaiCompatible baseUrl maybeKey =
  Provider
    { providerProtocol = OpenAICompat,
      providerBaseUrl = baseUrl,
      providerAuth = maybe NoAuth BearerAuth maybeKey,
      providerDefaultModel = Nothing,
      providerHeaders = []
    }

-- | Connect to any Anthropic Messages-compatible API endpoint (e.g. AWS Bedrock or GCP Vertex proxies)
anthropicCompatible :: Text -> Text -> Provider
anthropicCompatible baseUrl apiKey =
  Provider
    { providerProtocol = AnthropicMessages,
      providerBaseUrl = baseUrl,
      providerAuth = ApiKeyHeader "x-api-key" apiKey,
      providerDefaultModel = Just "claude-3-5-sonnet-20241022",
      providerHeaders = [("anthropic-version", "2023-06-01")]
    }

-- ============================================================================
-- Protocol Inspection
-- ============================================================================

-- | Endpoint path for chat completions
providerChatEndpoint :: Provider -> Text
providerChatEndpoint Provider {..} = case providerProtocol of
  OpenAICompat ->
    if "/v1" `T.isSuffixOf` providerBaseUrl
      then "/chat/completions"
      else "/v1/chat/completions"
  AnthropicMessages ->
    if "/v1" `T.isSuffixOf` providerBaseUrl
      then "/messages"
      else "/v1/messages"

-- | Authentication and protocol headers
providerAuthHeaders :: Provider -> [(Text, Text)]
providerAuthHeaders Provider {..} =
  let authHeaders = case providerAuth of
        BearerAuth token -> [("Authorization", "Bearer " <> token)]
        ApiKeyHeader hname token -> [(hname, token)]
        NoAuth -> []
   in authHeaders ++ providerHeaders

-- | Check if the provider uses the Anthropic Messages API
isAnthropicProtocol :: Provider -> Bool
isAnthropicProtocol Provider {..} = providerProtocol == AnthropicMessages

-- | Check if the provider supports strict JSON Schema output mode
supportsJsonSchema :: Provider -> Bool
supportsJsonSchema Provider {..} = case providerProtocol of
  OpenAICompat -> True
  AnthropicMessages -> False
