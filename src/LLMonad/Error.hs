{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | The error type for everything that can go wrong while talking to an LLM.
--
-- All LLMonad functions report failures by throwing 'LLMError' as an
-- exception inside the 'LLMonad.Core.LLM' monad. Use
-- 'LLMonad.Core.attempt' to catch them locally, or
-- 'LLMonad.Core.retry' to automatically recover from transient ones.
module LLMonad.Error
  ( -- * Errors
    LLMError (..)
  , isTransient
  , prettyError
  ) where

import Control.Exception (Exception)
import Data.Text (Text)
import qualified Data.Text as T

-- | Everything that can go wrong while working with an LLM.
data LLMError
  = -- | Transport-level failure (DNS, TLS, connection reset, ...).
    HttpError Text
  | -- | The provider answered with a non-2xx status.
    ApiError
      { apiStatus :: Int
      , apiBody :: Text
      }
  | -- | HTTP 429 specifically; carries @Retry-After@ when provided.
    RateLimitError
      { rateLimitStatus :: Int
      , rateLimitBody :: Text
      , rateLimitRetryAfterSecs :: Maybe Int
      }
  | -- | The model's output could not be decoded into the requested type.
    DecodeError
      { decodeDetail :: String
      , decodeRaw :: Text
      }
  | -- | A schema could not be built or was rejected outright.
    SchemaError Text
  | -- | The provider returned no assistant message at all.
    NoAssistantMessage
  | -- | Arguments for a tool call failed to decode.
    ToolArgumentError
      { toolArgTool :: Text
      , toolArgDetail :: String
      }
  | -- | The agent loop burned through its round budget without a final answer.
    AgentRoundsExhausted Int
  | -- | The configured provider cannot do what was asked (e.g. structured
    -- output against a text-only endpoint).
    UnsupportedCapability Text
  deriving (Eq, Show)

instance Exception LLMError

-- | Does this error describe a transient condition worth retrying?
--
-- Used by 'LLMonad.Core.retry': rate limits, transport hiccups, and
-- server-side (5xx \/ 408 \/ 409) failures are transient; decode failures,
-- schema problems, and 4xx client errors are not.
isTransient :: LLMError -> Bool
isTransient (HttpError _) = True
isTransient RateLimitError {} = True
isTransient (ApiError s _) = s == 408 || s == 409 || s >= 500
isTransient _ = False

-- | Human-readable one-liner for logs and CLI output.
prettyError :: LLMError -> Text
prettyError e = case e of
  HttpError t -> "network error: " <> t
  ApiError s b -> "API error " <> T.pack (show s) <> ": " <> T.take 400 b
  RateLimitError s _ (Just ra) ->
    "rate limited (HTTP " <> T.pack (show s) <> "), retry after " <> T.pack (show ra) <> "s"
  RateLimitError s _ Nothing -> "rate limited (HTTP " <> T.pack (show s) <> ")"
  DecodeError d raw ->
    "could not decode model output as the requested type ("
      <> T.pack d
      <> "); raw output: "
      <> T.take 400 (T.strip raw)
  SchemaError t -> "schema error: " <> t
  NoAssistantMessage -> "provider returned no assistant message"
  ToolArgumentError t d -> "tool \"" <> t <> "\" got unusable arguments: " <> T.pack d
  AgentRoundsExhausted n -> "agent did not settle after " <> T.pack (show n) <> " rounds"
  UnsupportedCapability t -> "provider does not support: " <> t
