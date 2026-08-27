{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Structured output decoding and retry loop.
module LLMonad.Structured (
    askStructured,
    extractWithRetry,

    -- * Shared structured-decode policy
    decodePayloadOrText,
    decodeFeedback,
) where

import Control.Exception qualified as E
import Data.Aeson (FromJSON, parseJSON)
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import LLMonad.Core
import LLMonad.Internal.Extract (decodeViaJSON)
import LLMonad.Schema (ToSchema (..))
import LLMonad.Types

{- | Decode a completion from its typed structured payload or, absent that,
by parsing the raw text. One home for both call sites so the payload /
text precedence can never drift apart.
-}
decodePayloadOrText :: (FromJSON a) => CompletionResponse -> Either String a
decodePayloadOrText response = case crspStructuredPayload response of
    Just val -> parseEither parseJSON val
    Nothing -> decodeViaJSON (crspText response)

{- | The corrective user message fed back to the model after a failed
decode. @subject@ names the turn being rejected (@\"final\"@ when the agent
settles on its last output, @\"previous\"@ mid-retry loop), keeping one
shared wording instead of drifted copies.
-}
decodeFeedback :: Text -> String -> Text
decodeFeedback subject detail =
    "Your "
        <> subject
        <> " response could not be decoded ("
        <> T.pack detail
        <> "). Return only valid JSON that conforms to the schema."

-- | Ask the model for structured output conforming to ToSchema.
askStructured :: forall a es. (IOE :> es, LLM :> es, FromJSON a, ToSchema a) => Text -> Eff es a
askStructured prompt = withTransaction $ do
    pushMessage (UserMsg prompt)
    resp <- chatRound defaultParams (RfJsonSchema (schemaTypeName @a) (toSchema @a) True) [] ToolAuto
    case decodePayloadOrText resp of
        Right a -> pure a
        Left err -> liftIO . E.throwIO $ DecodeError err (crspText resp)

-- | Extract structured output with self-correcting error recovery retry loop.
extractWithRetry :: forall a es. (IOE :> es, LLM :> es, FromJSON a, ToSchema a) => Int -> Text -> Eff es a
extractWithRetry maxTries initialPrompt = withTransaction $ do
    pushMessage (UserMsg initialPrompt)
    loop maxTries
  where
    loop n = do
        resp <- chatRound defaultParams (RfJsonSchema (schemaTypeName @a) (toSchema @a) True) [] ToolAuto
        case decodePayloadOrText resp of
            Right a -> pure a
            Left err
                | n <= 1 -> liftIO . E.throwIO $ DecodeError err (crspText resp)
                | otherwise -> do
                    pushMessage (UserMsg (decodeFeedback "previous" err))
                    loop (n - 1)
