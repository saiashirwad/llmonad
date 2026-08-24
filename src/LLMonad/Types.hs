{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Provider-neutral vocabulary shared by the DSL, the transports, and the
tests. Nothing in this module talks to the network.

These types deliberately model the /intersection/ of the OpenAI
Chat Completions protocol and Anthropic's Messages protocol, so a single
'CompletionRequest' can be rendered by either transport.
-}
module LLMonad.Types (
    -- * Models
    Model (..),

    -- * Messages
    Role (..),
    ToolCall (..),
    ChatMessage (..),

    -- * Sampling parameters
    Params (..),
    defaultParams,
    overrideParams,

    -- * Requests
    ResponseFormat (..),
    ToolSpec (..),
    ToolChoice (..),
    CompletionRequest (..),

    -- * Responses
    CompletionResponse (..),
    FinishReason (..),
    Usage (..),
    StreamEvent (..),
) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), Value, (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)

{- | Identifier of the model to hit, e.g. @\"gpt-4o-mini\"@ or
@\"claude-sonnet-4-5\"@. Has an 'IsString' instance so literals work
under @OverloadedStrings@.
-}
newtype Model = Model {unModel :: Text}
    deriving newtype (Eq, Ord, Show, IsString)

-- | The four speaker roles, provider-neutrally.
data Role = SystemRole | UserRole | AssistantRole | ToolRole
    deriving (Eq, Ord, Show)

-- | One invocation of a tool requested by the model.
data ToolCall = ToolCall
    { toolCallId :: Text
    , toolCallName :: Text
    , toolCallArguments :: Value
    }
    deriving (Eq, Ord, Show, Generic)

instance ToJSON ToolCall where
    toJSON (ToolCall cid name args) =
        Aeson.object
            [ "id" .= cid
            , "name" .= name
            , "arguments" .= args
            ]

instance FromJSON ToolCall where
    parseJSON = Aeson.withObject "ToolCall" $ \o -> do
        cid <-
            o .:? "id" >>= \case
                Just i -> pure i
                Nothing ->
                    o .:? "toolCallId" >>= \case
                        Just i -> pure i
                        Nothing -> pure ""
        name <-
            o .:? "name" >>= \case
                Just n -> pure n
                Nothing ->
                    o .:? "toolName" >>= \case
                        Just n -> pure n
                        Nothing -> pure ""
        args <-
            o .:? "arguments" >>= \case
                Just a -> pure a
                Nothing ->
                    o .:? "args" >>= \case
                        Just a -> pure a
                        Nothing -> pure Aeson.Null
        pure (ToolCall cid name args)

{- | A single message in a conversation, covering every role and the
tool-calling shapes of both supported protocols.
-}
data ChatMessage
    = SystemMsg Text
    | UserMsg Text
    | AssistantMsg Text [ToolCall]
    | -- | Result of executing a tool call (@toolCallId@ ties it back).
      ToolMsg Text Text
    deriving (Eq, Ord, Show)

{- | Sampling and request parameters. Every field is optional; unset fields
are simply omitted from the wire request, deferring to provider defaults.
-}
data Params = Params
    { paramTemperature :: Maybe Double
    , paramTopP :: Maybe Double
    , paramMaxTokens :: Maybe Int
    , paramStopSequences :: [Text]
    , paramTimeoutSeconds :: Maybe Int
    {- ^ Overall request timeout in seconds (headers must arrive within
    this window). Defaults to a generous 300s.
    -}
    }
    deriving (Eq, Ord, Show)

-- | All-nothing parameters.
defaultParams :: Params
defaultParams = Params Nothing Nothing Nothing [] Nothing

{- | Layer two parameter sets: fields set in the second argument win.

> effective = callSite `overrideParams` sessionDefaults  -- WRONG ORDER
> effective = sessionDefaults `overrideParams` callSite   -- call site wins
-}
overrideParams :: Params -> Params -> Params
overrideParams base over =
    Params
        { paramTemperature = paramTemperature over <|> paramTemperature base
        , paramTopP = paramTopP over <|> paramTopP base
        , paramMaxTokens = paramMaxTokens over <|> paramMaxTokens base
        , paramStopSequences = if null (paramStopSequences over) then paramStopSequences base else paramStopSequences over
        , paramTimeoutSeconds = paramTimeoutSeconds over <|> paramTimeoutSeconds base
        }

-- | What shape of answer we want back from the model.
data ResponseFormat
    = -- | Ordinary prose.
      RfText
    | -- | Any valid JSON object (legacy \"json mode\").
      RfJsonObject
    | {- | JSON conforming to a named JSON Schema. Providers that support
      native structured output enforce this server-side; others get a
      best-effort downgrade (see the provider modules).
      -}
      RfJsonSchema Text Value Bool
    deriving (Eq, Ord, Show)

{- | A tool advertised to the model (its name, description, and JSON Schema
for arguments). Built for you by 'LLMonad.Tools.mkTool'.
-}
data ToolSpec = ToolSpec
    { toolSpecName :: Text
    , toolSpecDescription :: Text
    , toolSpecParameters :: Value
    }
    deriving (Eq, Ord, Show)

-- | Who decides when a tool is called.
data ToolChoice
    = -- | The model chooses (the default).
      ToolAuto
    | {- | Force a specific tool by name (used for structured output on
      Anthropic).
      -}
      ToolForce Text
    deriving (Eq, Ord, Show)

-- | A fully specified, provider-neutral completion request.
data CompletionRequest = CompletionRequest
    { crModel :: Model
    , crSystem :: Maybe Text
    -- ^ System prompt, rendered however the provider likes.
    , crMessages :: [ChatMessage]
    -- ^ Conversation so far, oldest first.
    , crParams :: Params
    , crTools :: [ToolSpec]
    , crToolChoice :: ToolChoice
    , crResponseFormat :: ResponseFormat
    }
    deriving (Eq, Show)

-- | Why generation stopped.
data FinishReason
    = FrStop
    | FrLength
    | FrToolUse
    | FrContentFilter
    | FrOther Text
    deriving (Eq, Ord, Show)

-- | Token accounting, when the provider reports it.
data Usage = Usage
    { usageInputTokens :: Int
    , usageOutputTokens :: Int
    }
    deriving (Eq, Ord, Show)

-- | The model's final answer to a request.
data CompletionResponse = CompletionResponse
    { crspText :: Text
    {- ^ Assistant text (for structured output: the JSON payload, when the
    provider delivered it as text).
    -}
    , crspToolCalls :: [ToolCall]
    -- ^ Tool invocations the model asked for, in order.
    , crspStructuredPayload :: Maybe Value
    {- ^ Structured output delivered out-of-band (e.g. Anthropic returns
    the JSON inside a forced @tool_use@ block). When present,
    'LLMonad.Core.ask' decodes this directly instead of parsing
    'crspText'.
    -}
    , crspFinishReason :: FinishReason
    , crspUsage :: Maybe Usage
    }
    deriving (Eq, Show)

-- | Incremental events produced while streaming.
data StreamEvent
    = -- | A fragment of assistant text.
      SEText Text
    | -- | Always the last event; carries the assembled response.
      SEFinished CompletionResponse
    deriving (Eq, Show)
