{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Types, data structures, and JSON serialization instances for the Journal effect.
module LLMonad.Journal.Types
  ( -- * Core Event Types
    JournalEvent (..)
  , pattern JournalUserMsg
  , pattern ModelTurnSimple
  , pattern ToolInvokedSimple
  , ToolCall (..)
  , ModelMetrics (..)
  , promptTokens
  , completionTokens
  , totalTokens
  , latencyMs
  , metricModel
  , emptyModelMetrics

    -- * Tool Results
  , ToolResult

    -- * Audit & Replay Types
  , ReplaySummary (..)
  , emptyReplaySummary

    -- * Journal State
  , JournalState (..)
  , initJournalState
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), Value, (.:), (.:?), (.=), (.!=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import LLMonad.Types (ToolCall (..))

-- | Result of running a tool: either an error message or a JSON payload.
type ToolResult = Either Text Value

-- | Model token usage and execution timing metrics.
data ModelMetrics = ModelMetrics
  { mmPromptTokens     :: !Int
  , mmCompletionTokens :: !Int
  , mmTotalTokens      :: !Int
  , mmLatencyMs        :: !Double
  , mmModel            :: !Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Accessor for prompt tokens.
promptTokens :: ModelMetrics -> Int
promptTokens = mmPromptTokens

-- | Accessor for completion tokens.
completionTokens :: ModelMetrics -> Int
completionTokens = mmCompletionTokens

-- | Accessor for total tokens.
totalTokens :: ModelMetrics -> Int
totalTokens = mmTotalTokens

-- | Accessor for latency in milliseconds.
latencyMs :: ModelMetrics -> Double
latencyMs = mmLatencyMs

-- | Accessor for model name.
metricModel :: ModelMetrics -> Text
metricModel = mmModel

-- | Construct an empty 'ModelMetrics' structure.
emptyModelMetrics :: Text -> ModelMetrics
emptyModelMetrics modelName = ModelMetrics
  { mmPromptTokens     = 0
  , mmCompletionTokens = 0
  , mmTotalTokens      = 0
  , mmLatencyMs        = 0.0
  , mmModel            = modelName
  }

-- | Discriminated union of all event records captured during an agent session.
data JournalEvent
  = TurnStarted !Text
  | UserMsg !Text
  | ModelTurn !Text ![ToolCall]
  | ToolInvoked !Text !Text !Value
  | ToolCompleted !Text !ToolResult
  | MetricsReported !ModelMetrics
  | TurnFinished !Text
  deriving (Show, Eq, Generic)

-- | Unambiguous pattern synonym for 'UserMsg' event constructor.
pattern JournalUserMsg :: Text -> JournalEvent
pattern JournalUserMsg txt = UserMsg txt

-- | Convenience pattern synonym for 'ModelTurn' with empty tool calls.
pattern ModelTurnSimple :: Text -> JournalEvent
pattern ModelTurnSimple txt <- ModelTurn txt _
  where
    ModelTurnSimple txt = ModelTurn txt []

-- | Convenience pattern synonym for 'ToolInvoked' where toolCallId equals toolName.
pattern ToolInvokedSimple :: Text -> Value -> JournalEvent
pattern ToolInvokedSimple name args <- ToolInvoked _ name args
  where
    ToolInvokedSimple name args = ToolInvoked name name args

instance ToJSON JournalEvent where
  toJSON = \case
    TurnStarted tid ->
      Aeson.object ["type" .= ("TurnStarted" :: Text), "turnId" .= tid]
    UserMsg content ->
      Aeson.object ["type" .= ("UserMsg" :: Text), "content" .= content]
    ModelTurn content calls ->
      Aeson.object
        [ "type" .= ("ModelTurn" :: Text)
        , "content" .= content
        , "toolCalls" .= calls
        ]
    ToolInvoked callId name args ->
      Aeson.object
        [ "type" .= ("ToolInvoked" :: Text)
        , "toolCallId" .= callId
        , "toolName" .= name
        , "arguments" .= args
        ]
    ToolCompleted callId res ->
      Aeson.object
        [ "type" .= ("ToolCompleted" :: Text)
        , "toolCallId" .= callId
        , "result" .= res
        ]
    MetricsReported metrics ->
      Aeson.object ["type" .= ("MetricsReported" :: Text), "metrics" .= metrics]
    TurnFinished tid ->
      Aeson.object ["type" .= ("TurnFinished" :: Text), "turnId" .= tid]

instance FromJSON JournalEvent where
  parseJSON = Aeson.withObject "JournalEvent" $ \o -> do
    tag <- o .: "type"
    case (tag :: Text) of
      "TurnStarted" -> TurnStarted <$> o .: "turnId"
      "UserMsg" -> UserMsg <$> o .: "content"
      "ModelTurn" -> do
        content <- o .: "content"
        calls <- (o .:? "toolCalls" <|> o .:? "tool_calls") .!= []
        pure (ModelTurn content calls)
      "ToolInvoked" -> do
        name <- o .:? "toolName" >>= \case
          Just n -> pure n
          Nothing -> o .:? "name" >>= \case
            Just n -> pure n
            Nothing -> pure ""
        callId <- o .:? "toolCallId" >>= \case
          Just cid -> pure cid
          Nothing -> o .:? "id" >>= \case
            Just cid -> pure cid
            Nothing -> pure name
        args <- o .:? "arguments" >>= \case
          Just a -> pure a
          Nothing -> o .:? "args" >>= \case
            Just a -> pure a
            Nothing -> pure Aeson.Null
        pure (ToolInvoked callId (if T.null name then callId else name) args)
      "ToolCompleted" -> do
        callId <- o .:? "toolCallId" >>= \case
          Just cid -> pure cid
          Nothing -> o .:? "id" >>= \case
            Just cid -> pure cid
            Nothing -> o .:? "toolName" >>= \case
              Just tn -> pure tn
              Nothing -> pure ""
        res <- (o .: "result") <|> (Right <$> o .: "result")
        pure (ToolCompleted callId res)
      "MetricsReported" -> MetricsReported <$> o .: "metrics"
      "TurnFinished" -> TurnFinished <$> o .: "turnId"
      other -> AesonTypes.prependFailure "parsing JournalEvent failed: "
                 (fail ("unknown event type: " ++ show other))

-- | Summary and validation report produced by 'replayAudit'.
data ReplaySummary = ReplaySummary
  { rsTotalTurns        :: !Int
  , rsUserMessages      :: !Int
  , rsModelTurns        :: !Int
  , rsToolInvocations   :: !Int
  , rsToolCompletions   :: !Int
  , rsPromptTokens      :: !Int
  , rsCompletionTokens  :: !Int
  , rsTotalTokens       :: !Int
  , rsTotalLatencyMs    :: !Double
  , rsIsValidSequence   :: !Bool
  , rsValidationErrors  :: ![Text]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Empty replay summary with all counters initialized to zero.
emptyReplaySummary :: ReplaySummary
emptyReplaySummary = ReplaySummary
  { rsTotalTurns        = 0
  , rsUserMessages      = 0
  , rsModelTurns        = 0
  , rsToolInvocations   = 0
  , rsToolCompletions   = 0
  , rsPromptTokens      = 0
  , rsCompletionTokens  = 0
  , rsTotalTokens       = 0
  , rsTotalLatencyMs    = 0.0
  , rsIsValidSequence   = True
  , rsValidationErrors  = []
  }

-- | State tracking for in-memory journal session execution.
data JournalState = JournalState
  { jsEvents    :: ![JournalEvent]
  , jsSessionId :: !(Maybe Text)
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Initialize empty in-memory journal state.
initJournalState :: JournalState
initJournalState = JournalState
  { jsEvents    = []
  , jsSessionId = Nothing
  }
