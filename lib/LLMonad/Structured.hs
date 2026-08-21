{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LLMonad.Structured
  ( -- * Structured Extraction
    askStructured,
    generateObject,
    generateObjectWithRetry,

    -- * Helper functions
    buildStructuredSystemPrompt,
    cleanJsonMarkdown,
  )
where

import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ask)
import Data.Aeson (FromJSON, eitherDecodeStrict, encode)
import Data.ByteString.Lazy qualified as LB
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import LLMonad.Client (LLMError (..), executeChatRequest, executeChatRequestWithManager)
import LLMonad.Core
import LLMonad.Provider
import LLMonad.Schema
import LLMonad.Types

-- | Query the LLM and parse the result into a strongly-typed Haskell record with automatic self-correcting retry loop
askStructured :: forall a. (FromJSON a, HasSchema a) => Text -> LLM a
askStructured prompt = do
  generateObject @a [userMsg prompt]

-- | Extract a structured Haskell type from a sequence of conversation messages
generateObject :: forall a. (FromJSON a, HasSchema a) => [Message] -> LLM a
generateObject msgs = do
  cfg <- ask
  let retries = configMaxRetries cfg
  generateObjectWithRetry @a retries msgs

-- | Extract structured object with explicit retry count on schema validation failures
generateObjectWithRetry ::
  forall a.
  (FromJSON a, HasSchema a) =>
  Int ->
  [Message] ->
  LLM a
generateObjectWithRetry maxRetries initialMsgs = do
  let targetSchema = schema @a
      schemaVal = schemaToValue targetSchema
      schemaJsonText = decodeUtf8 (LB.toStrict (encode schemaVal))
      sysPrompt = buildStructuredSystemPrompt schemaJsonText

      baseMsgs =
        if any (\m -> messageRole m == SystemRole) initialMsgs
          then initialMsgs
          else systemMsg sysPrompt : initialMsgs

  attemptLoop maxRetries baseMsgs
  where
    attemptLoop remainingTries currentMsgs = do
      LLMConfig {..} <- ask
      let targetSchema = schema @a
          schemaVal = schemaToValue targetSchema
          schemaJsonText = decodeUtf8 (LB.toStrict (encode schemaVal))

          jsonFormatVal =
            if supportsJsonSchema configProvider
              then Just (schemaToOpenAISchema "StructuredOutput" targetSchema)
              else Nothing

          chatReq =
            ChatRequest
              { reqModel = configModel,
                reqMessages = currentMsgs,
                reqTemperature = maybe (Just 0.0) Just configTemperature,
                reqMaxTokens = configMaxTokens,
                reqStream = False,
                reqTools = Nothing,
                reqResponseFormat = jsonFormatVal
              }

      respOrErr <- liftIO $ case configHttpManager of
        Just mgr -> executeChatRequestWithManager mgr configProvider chatReq
        Nothing -> executeChatRequest configProvider chatReq

      case respOrErr of
        Left err -> throwError err
        Right resp -> do
          let rawText = extractTextContent resp
              cleanedText = cleanJsonMarkdown rawText

          case eitherDecodeStrict (encodeUtf8 cleanedText) of
            Right (val :: a) -> pure val
            Left parseErr ->
              if remainingTries <= 0
                then
                  throwError $
                    MaxRetriesExceeded
                      maxRetries
                      ( "Structured JSON decoding failed after "
                          <> T.pack (show maxRetries)
                          <> " retries. Last error: "
                          <> T.pack parseErr
                          <> ". Output: "
                          <> rawText
                      )
                else do
                  let retryMsg =
                        userMsg $
                          "Your previous output failed JSON schema validation with error: "
                            <> T.pack parseErr
                            <> ".\nPrevious output was:\n"
                            <> rawText
                            <> "\n\nPlease repair your output and return ONLY valid JSON matching this schema:\n"
                            <> schemaJsonText
                      updatedMsgs = currentMsgs ++ [assistantMsg rawText, retryMsg]
                  attemptLoop (remainingTries - 1) updatedMsgs

-- | Construct system prompt enforcing JSON output
buildStructuredSystemPrompt :: Text -> Text
buildStructuredSystemPrompt schemaJson =
  "You are a structured data extraction engine.\n"
    <> "You MUST respond with valid JSON that strictly conforms to the following JSON Schema.\n"
    <> "Do not include any explanation, conversational text, or markdown code block fences outside the JSON.\n"
    <> "JSON Schema:\n"
    <> schemaJson

-- | Strip markdown code fences (e.g. ```json ... ```) if present
cleanJsonMarkdown :: Text -> Text
cleanJsonMarkdown txt =
  let t = T.strip txt
   in if "```json" `T.isPrefixOf` t
        then T.dropWhileEnd (== '`') $ T.strip $ T.drop 7 t
        else
          if "```" `T.isPrefixOf` t
            then T.dropWhileEnd (== '`') $ T.strip $ T.drop 3 t
            else t
