{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LLMonad.Types
  ( -- * Model & Roles
    Model (..),
    Role (..),

    -- * Messages & Prompts
    Message (..),
    ContentPart (..),
    Prompt (..),
    toPrompt,

    -- * Message Constructors
    systemMsg,
    userMsg,
    assistantMsg,
    toolMsg,

    -- * Tool Invocation Types
    ToolCall (..),
    FunctionCall (..),

    -- * Request and Response
    ChatRequest (..),
    ChatResponse (..),
    Choice (..),
    FinishReason (..),
    Usage (..),

    -- * Extraction Helpers
    extractTextContent,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value (..),
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
  )
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- | Model identifier with IsString support (e.g. "deepseek-chat", "gpt-4o", "claude-3-5-sonnet")
newtype Model = Model {unModel :: Text}
  deriving newtype (Show, Eq, Ord, IsString, ToJSON, FromJSON)

-- | Role of a message in an interaction
data Role
  = SystemRole
  | UserRole
  | AssistantRole
  | ToolRole
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Role where
  toJSON SystemRole = "system"
  toJSON UserRole = "user"
  toJSON AssistantRole = "assistant"
  toJSON ToolRole = "tool"

instance FromJSON Role where
  parseJSON = \case
    String "system" -> pure SystemRole
    String "user" -> pure UserRole
    String "assistant" -> pure AssistantRole
    String "tool" -> pure ToolRole
    other -> fail ("Unknown role: " <> show other)

-- | Multi-modal content part
data ContentPart
  = TextContent Text
  | ImageUrlContent Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | A single message in a conversation
data Message = Message
  { messageRole :: Role,
    messageContent :: Text,
    messageName :: Maybe Text,
    messageToolCallId :: Maybe Text,
    messageToolCalls :: Maybe [ToolCall]
  }
  deriving (Show, Eq, Generic)

instance IsString Message where
  fromString str = userMsg (T.pack str)

instance ToJSON Message where
  toJSON Message {..} =
    let base =
          [ "role" .= messageRole,
            "content" .= messageContent
          ]
        withName = case messageName of
          Just n -> ("name" .= n) : base
          Nothing -> base
        withToolId = case messageToolCallId of
          Just tid -> ("tool_call_id" .= tid) : withName
          Nothing -> withName
        withCalls = case messageToolCalls of
          Just calls -> ("tool_calls" .= calls) : withToolId
          Nothing -> withToolId
     in object withCalls

instance FromJSON Message where
  parseJSON = withObject "Message" $ \o -> do
    r <- o .: "role"
    c <- o .:? "content"
    let contentVal = case c of
          Just (String t) -> t
          Just Null -> ""
          Nothing -> ""
          Just other -> T.pack (show other)
    n <- o .:? "name"
    tid <- o .:? "tool_call_id"
    tcalls <- o .:? "tool_calls"
    pure
      Message
        { messageRole = r,
          messageContent = contentVal,
          messageName = n,
          messageToolCallId = tid,
          messageToolCalls = tcalls
        }

-- | A composable Prompt that forms a Monoid over messages
newtype Prompt = Prompt {unPrompt :: [Message]}
  deriving newtype (Show, Eq, Semigroup, Monoid)

instance IsString Prompt where
  fromString s = Prompt [userMsg (T.pack s)]

-- | Convert single message or list of messages into a Prompt
toPrompt :: [Message] -> Prompt
toPrompt = Prompt

-- | Smart constructors for messages
systemMsg :: Text -> Message
systemMsg content = Message SystemRole content Nothing Nothing Nothing

userMsg :: Text -> Message
userMsg content = Message UserRole content Nothing Nothing Nothing

assistantMsg :: Text -> Message
assistantMsg content = Message AssistantRole content Nothing Nothing Nothing

toolMsg :: Text -> Text -> Message
toolMsg tid content = Message ToolRole content Nothing (Just tid) Nothing

-- | Tool call requested by the model
data ToolCall = ToolCall
  { toolCallId :: Text,
    toolCallType :: Text,
    toolCallFunction :: FunctionCall
  }
  deriving (Show, Eq, Generic)

instance ToJSON ToolCall where
  toJSON ToolCall {..} =
    object
      [ "id" .= toolCallId,
        "type" .= toolCallType,
        "function" .= toolCallFunction
      ]

instance FromJSON ToolCall where
  parseJSON = withObject "ToolCall" $ \o ->
    ToolCall
      <$> o .: "id"
      <*> o .: "type"
      <*> o .: "function"

-- | Function invocation details inside a ToolCall
data FunctionCall = FunctionCall
  { funcName :: Text,
    funcArguments :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON FunctionCall where
  toJSON FunctionCall {..} =
    object
      [ "name" .= funcName,
        "arguments" .= funcArguments
      ]

instance FromJSON FunctionCall where
  parseJSON = withObject "FunctionCall" $ \o ->
    FunctionCall
      <$> o .: "name"
      <*> o .: "arguments"

-- | Reason why generation stopped
data FinishReason
  = FinishStop
  | FinishToolCalls
  | FinishLength
  | FinishContentFilter
  | FinishOther Text
  deriving (Show, Eq, Generic)

instance ToJSON FinishReason where
  toJSON FinishStop = "stop"
  toJSON FinishToolCalls = "tool_calls"
  toJSON FinishLength = "length"
  toJSON FinishContentFilter = "content_filter"
  toJSON (FinishOther o) = String o

instance FromJSON FinishReason where
  parseJSON = \case
    String "stop" -> pure FinishStop
    String "tool_calls" -> pure FinishToolCalls
    String "length" -> pure FinishLength
    String "content_filter" -> pure FinishContentFilter
    String "function_call" -> pure FinishToolCalls
    String other -> pure (FinishOther other)
    Null -> pure FinishStop
    other -> fail ("Invalid finish reason: " <> show other)

-- | Token usage details
data Usage = Usage
  { promptTokens :: Int,
    completionTokens :: Int,
    totalTokens :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON Usage where
  toJSON Usage {..} =
    object
      [ "prompt_tokens" .= promptTokens,
        "completion_tokens" .= completionTokens,
        "total_tokens" .= totalTokens
      ]

instance FromJSON Usage where
  parseJSON = withObject "Usage" $ \o ->
    Usage
      <$> (o .:? "prompt_tokens" >>= maybe (pure 0) pure)
      <*> (o .:? "completion_tokens" >>= maybe (pure 0) pure)
      <*> (o .:? "total_tokens" >>= maybe (pure 0) pure)

-- | Single choice returned in response to a prompt
data Choice = Choice
  { choiceIndex :: Int,
    choiceMessage :: Message,
    choiceFinishReason :: Maybe FinishReason
  }
  deriving (Show, Eq, Generic)

instance ToJSON Choice where
  toJSON Choice {..} =
    object
      [ "index" .= choiceIndex,
        "message" .= choiceMessage,
        "finish_reason" .= choiceFinishReason
      ]

instance FromJSON Choice where
  parseJSON = withObject "Choice" $ \o ->
    Choice
      <$> (o .:? "index" >>= maybe (pure 0) pure)
      <*> o .: "message"
      <*> o .:? "finish_reason"

-- | Standard request dispatched to LLM providers
data ChatRequest = ChatRequest
  { reqModel :: Model,
    reqMessages :: [Message],
    reqTemperature :: Maybe Double,
    reqMaxTokens :: Maybe Int,
    reqStream :: Bool,
    reqTools :: Maybe [Value],
    reqResponseFormat :: Maybe Value
  }
  deriving (Show, Eq, Generic)

instance ToJSON ChatRequest where
  toJSON ChatRequest {..} =
    let fields =
          [ "model" .= reqModel,
            "messages" .= reqMessages,
            "stream" .= reqStream
          ]
            ++ maybe [] (\t -> ["temperature" .= t]) reqTemperature
            ++ maybe [] (\m -> ["max_tokens" .= m]) reqMaxTokens
            ++ maybe [] (\tools -> ["tools" .= tools]) reqTools
            ++ maybe [] (\rf -> ["response_format" .= rf]) reqResponseFormat
     in object fields

-- | Standard response returned from LLM providers
data ChatResponse = ChatResponse
  { responseId :: Maybe Text,
    responseModel :: Maybe Model,
    responseChoices :: [Choice],
    responseUsage :: Maybe Usage
  }
  deriving (Show, Eq, Generic)

instance ToJSON ChatResponse where
  toJSON ChatResponse {..} =
    object
      [ "id" .= responseId,
        "model" .= responseModel,
        "choices" .= responseChoices,
        "usage" .= responseUsage
      ]

instance FromJSON ChatResponse where
  parseJSON = withObject "ChatResponse" $ \o ->
    ChatResponse
      <$> o .:? "id"
      <*> o .:? "model"
      <*> o .: "choices"
      <*> o .:? "usage"

-- | Extract text content from the first choice
extractTextContent :: ChatResponse -> Text
extractTextContent resp = case responseChoices resp of
  [] -> ""
  (c : _) -> messageContent (choiceMessage c)
