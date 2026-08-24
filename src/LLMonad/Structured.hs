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
) where

import Control.Exception (throw)
import Data.Aeson (FromJSON, parseJSON)
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import LLMonad.Core
import LLMonad.Internal.Extract (decodeViaJSON)
import LLMonad.Schema (ToSchema (..))
import LLMonad.Types

-- | Ask the model for structured output conforming to ToSchema.
askStructured :: forall a es. (LLM :> es, FromJSON a, ToSchema a) => Text -> Eff es a
askStructured prompt = withTransaction $ do
    pushMessage (UserMsg prompt)
    resp <- chatRound defaultParams (RfJsonSchema (schemaTypeName @a) (toSchema @a) True) [] ToolAuto
    case crspStructuredPayload resp of
        Just val -> case parseEither parseJSON val of
            Right a -> pure a
            Left err -> throw (DecodeError err (crspText resp))
        Nothing -> case decodeViaJSON (crspText resp) of
            Right a -> pure a
            Left err -> throw (DecodeError err (crspText resp))

-- | Extract structured output with self-correcting error recovery retry loop.
extractWithRetry :: forall a es. (LLM :> es, FromJSON a, ToSchema a) => Int -> Text -> Eff es a
extractWithRetry maxTries initialPrompt = withTransaction $ do
    pushMessage (UserMsg initialPrompt)
    loop maxTries
  where
    loop n = do
        resp <- chatRound defaultParams (RfJsonSchema (schemaTypeName @a) (toSchema @a) True) [] ToolAuto
        let parsed = case crspStructuredPayload resp of
                Just val -> parseEither parseJSON val
                Nothing -> decodeViaJSON (crspText resp)
        case parsed of
            Right a -> pure a
            Left err
                | n <= 1 -> throw (DecodeError err (crspText resp))
                | otherwise -> do
                    let feedback = "Your previous response could not be decoded (" <> T.pack err <> "). Please return ONLY valid JSON conforming to the schema."
                    pushMessage (UserMsg feedback)
                    loop (n - 1)
