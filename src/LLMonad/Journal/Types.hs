{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Types, data structures, and JSON serialization instances for the Journal effect.
module LLMonad.Journal.Types
  ( -- * Core Event Types
    JournalEvent (..)
  , pattern JournalUserMsg
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

import Data.Aeson (FromJSON (..), ToJSON (..), Value, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import Data.Text (Text)
import GHC.Generics (Generic)

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
  | ModelTurn !Text
  | ToolInvoked !Text !Value
  | ToolCompleted !Text !ToolResult
  | MetricsReported !ModelMetrics
  | TurnFinished !Text
  deriving (Show, Eq, Generic)

-- | Unambiguous pattern synonym for 'UserMsg' event constructor.
pattern JournalUserMsg :: Text -> JournalEvent
pattern JournalUserMsg txt = UserMsg txt

instance ToJSON JournalEvent where
  toJSON = \case
    TurnStarted tid ->
      Aeson.object ["type" .= ("TurnStarted" :: Text), "turnId" .= tid]
    UserMsg content ->
      Aeson.object ["type" .= ("UserMsg" :: Text), "content" .= content]
    ModelTurn content ->
      Aeson.object ["type" .= ("ModelTurn" :: Text), "content" .= content]
    ToolInvoked toolName args ->
      Aeson.object ["type" .= ("ToolInvoked" :: Text), "toolName" .= toolName, "arguments" .= args]
    ToolCompleted toolName res ->
      Aeson.object ["type" .= ("ToolCompleted" :: Text), "toolName" .= toolName, "result" .= res]
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
      "ModelTurn" -> ModelTurn <$> o .: "content"
      "ToolInvoked" -> ToolInvoked <$> o .: "toolName" <*> o .: "arguments"
      "ToolCompleted" -> ToolCompleted <$> o .: "toolName" <*> o .: "result"
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
