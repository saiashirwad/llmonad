{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLMonad.Client
  ( -- * Error Types
    LLMError (..),

    -- * Client Execution
    executeChatRequest,
    executeChatRequestWithManager,

    -- * Manager Helpers
    getGlobalManager,
  )
where

import Control.Exception (Exception, SomeException, try)
import Data.Aeson
  ( Value (..),
    eitherDecodeStrict,
    encode,
    object,
    (.=),
  )
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LB
import Data.CaseInsensitive qualified as CI
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import LLMonad.Provider
import LLMonad.Types
import Network.HTTP.Client
  ( Manager,
    Request (..),
    RequestBody (..),
    Response (..),
    httpLbs,
    newManager,
    parseRequest,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Status (statusCode)
import System.IO.Unsafe (unsafePerformIO)

-- | Error conditions in LLM calls
data LLMError
  = NetworkError Text
  | HttpError Int Text
  | JsonDecodeError Text Text
  | ProviderError Text
  | ToolExecutionError Text Text
  | MaxRetriesExceeded Int Text
  | InvalidResponse Text
  deriving (Show, Eq, Generic)

instance Exception LLMError

-- | Global HTTP manager for connection reuse
globalManager :: Manager
{-# NOINLINE globalManager #-}
globalManager = unsafePerformIO (newManager tlsManagerSettings)

getGlobalManager :: IO Manager
getGlobalManager = pure globalManager

-- | Execute a ChatRequest using the global connection manager
executeChatRequest :: Provider -> ChatRequest -> IO (Either LLMError ChatResponse)
executeChatRequest = executeChatRequestWithManager globalManager

-- | Execute a ChatRequest using a provided HTTP Manager
executeChatRequestWithManager :: Manager -> Provider -> ChatRequest -> IO (Either LLMError ChatResponse)
executeChatRequestWithManager mgr provider req = do
  if isAnthropicProtocol provider
    then executeAnthropicRequest mgr provider req
    else executeOpenAICompatibleRequest mgr provider req

-- ============================================================================
-- OpenAI-Compatible Execution (OpenAI, Groq, Ollama, Custom)
-- ============================================================================

executeOpenAICompatibleRequest ::
  Manager ->
  Provider ->
  ChatRequest ->
  IO (Either LLMError ChatResponse)
executeOpenAICompatibleRequest mgr provider chatReq = do
  let baseUrl = providerBaseUrl provider
      endpoint = providerChatEndpoint provider
      fullUrl = T.unpack (baseUrl <> endpoint)

  reqResult <- try (parseRequest fullUrl)
  case reqResult of
    Left (e :: SomeException) ->
      pure $ Left $ NetworkError ("Failed to parse URL: " <> T.pack (show e))
    Right initReq -> do
      let headers =
            [ ("Content-Type", "application/json")
            ]
              ++ providerAuthHeaders provider

          httpHeaders = map (\(k, v) -> (toHeaderName k, encodeUtf8 v)) headers
          reqBody = encode chatReq

          finalReq =
            initReq
              { method = "POST",
                requestHeaders = httpHeaders,
                requestBody = RequestBodyLBS reqBody
              }

      respResult <- try (httpLbs finalReq mgr)
      case respResult of
        Left (e :: SomeException) ->
          pure $ Left $ NetworkError ("HTTP connection failed: " <> T.pack (show e))
        Right resp -> do
          let status = responseStatus resp
              statusCodeVal = statusCode status
              bodyBs = responseBody resp
              bodyText = decodeUtf8 (LB.toStrict bodyBs)

          if statusCodeVal < 200 || statusCodeVal >= 300
            then pure $ Left $ HttpError statusCodeVal bodyText
            else case eitherDecodeStrict (LB.toStrict bodyBs) of
              Left err ->
                pure $ Left $ JsonDecodeError (T.pack err) bodyText
              Right (chatResp :: ChatResponse) ->
                pure $ Right chatResp

-- ============================================================================
-- Anthropic Execution
-- ============================================================================

executeAnthropicRequest ::
  Manager ->
  Provider ->
  ChatRequest ->
  IO (Either LLMError ChatResponse)
executeAnthropicRequest mgr provider chatReq = do
  let baseUrl = providerBaseUrl provider
      endpoint = providerChatEndpoint provider
      fullUrl = T.unpack (baseUrl <> endpoint)

  reqResult <- try (parseRequest fullUrl)
  case reqResult of
    Left (e :: SomeException) ->
      pure $ Left $ NetworkError ("Failed to parse URL: " <> T.pack (show e))
    Right initReq -> do
      let headers =
            [ ("Content-Type", "application/json")
            ]
              ++ providerAuthHeaders provider

          httpHeaders = map (\(k, v) -> (toHeaderName k, encodeUtf8 v)) headers
          anthropicBody = buildAnthropicPayload chatReq
          reqBody = encode anthropicBody

          finalReq =
            initReq
              { method = "POST",
                requestHeaders = httpHeaders,
                requestBody = RequestBodyLBS reqBody
              }

      respResult <- try (httpLbs finalReq mgr)
      case respResult of
        Left (e :: SomeException) ->
          pure $ Left $ NetworkError ("Anthropic connection failed: " <> T.pack (show e))
        Right resp -> do
          let status = responseStatus resp
              statusCodeVal = statusCode status
              bodyBs = responseBody resp
              bodyText = decodeUtf8 (LB.toStrict bodyBs)

          if statusCodeVal < 200 || statusCodeVal >= 300
            then pure $ Left $ HttpError statusCodeVal bodyText
            else parseAnthropicResponse bodyBs bodyText

-- | Format Anthropic Messages request JSON
buildAnthropicPayload :: ChatRequest -> Value
buildAnthropicPayload ChatRequest {..} =
  let (systemMsgs, nonSystemMsgs) =
        partitionMessages reqMessages

      systemText =
        if null systemMsgs
          then Nothing
          else Just (T.intercalate "\n\n" (map messageContent systemMsgs))

      formattedMessages = map formatAnthropicMessage nonSystemMsgs
      maxToks = maybe 4096 id reqMaxTokens

      baseFields =
        [ "model" .= reqModel,
          "max_tokens" .= maxToks,
          "messages" .= formattedMessages
        ]
          ++ maybe [] (\s -> ["system" .= s]) systemText
          ++ maybe [] (\t -> ["temperature" .= t]) reqTemperature
          ++ maybe [] (\tools -> ["tools" .= tools]) reqTools
   in object baseFields

partitionMessages :: [Message] -> ([Message], [Message])
partitionMessages msgs =
  ( filter (\m -> messageRole m == SystemRole) msgs,
    filter (\m -> messageRole m /= SystemRole) msgs
  )

formatAnthropicMessage :: Message -> Value
formatAnthropicMessage Message {..} = case messageRole of
  UserRole ->
    object
      [ "role" .= ("user" :: Text),
        "content" .= messageContent
      ]
  AssistantRole ->
    case messageToolCalls of
      Just tcalls ->
        let toolBlocks = map formatAnthropicToolUse tcalls
            textBlock =
              if T.null messageContent
                then []
                else [object ["type" .= ("text" :: Text), "text" .= messageContent]]
         in object
              [ "role" .= ("assistant" :: Text),
                "content" .= (textBlock ++ toolBlocks)
              ]
      Nothing ->
        object
          [ "role" .= ("assistant" :: Text),
            "content" .= messageContent
          ]
  ToolRole ->
    let tid = maybe "" id messageToolCallId
     in object
          [ "role" .= ("user" :: Text),
            "content"
              .= [ object
                     [ "type" .= ("tool_result" :: Text),
                       "tool_use_id" .= tid,
                       "content" .= messageContent
                     ]
                 ]
          ]
  SystemRole ->
    object
      [ "role" .= ("user" :: Text),
        "content" .= messageContent
      ]

formatAnthropicToolUse :: ToolCall -> Value
formatAnthropicToolUse ToolCall {..} =
  let parsedInput = case eitherDecodeStrict (encodeUtf8 (funcArguments toolCallFunction)) of
        Right (v :: Value) -> v
        Left _ -> object []
   in object
        [ "type" .= ("tool_use" :: Text),
          "id" .= toolCallId,
          "name" .= funcName toolCallFunction,
          "input" .= parsedInput
        ]

-- | Parse Anthropic response format into standard ChatResponse
parseAnthropicResponse :: LB.ByteString -> Text -> IO (Either LLMError ChatResponse)
parseAnthropicResponse bodyBs bodyText = do
  case eitherDecodeStrict (LB.toStrict bodyBs) of
    Left err ->
      pure $ Left $ JsonDecodeError (T.pack err) bodyText
    Right (val :: Value) -> do
      case val of
        Object obj -> do
          let respId = case KM.lookup "id" obj of
                Just (String s) -> Just s
                _ -> Nothing
              respModel = case KM.lookup "model" obj of
                Just (String s) -> Just (Model s)
                _ -> Nothing
              stopReason = case KM.lookup "stop_reason" obj of
                Just (String "end_turn") -> Just FinishStop
                Just (String "tool_use") -> Just FinishToolCalls
                Just (String "max_tokens") -> Just FinishLength
                Just (String other) -> Just (FinishOther other)
                _ -> Just FinishStop

              contentArray = case KM.lookup "content" obj of
                Just (Array arr) -> V.toList arr
                _ -> []

              (textContent, toolCallsList) = parseAnthropicContent contentArray

              msg =
                Message
                  { messageRole = AssistantRole,
                    messageContent = textContent,
                    messageName = Nothing,
                    messageToolCallId = Nothing,
                    messageToolCalls =
                      if null toolCallsList
                        then Nothing
                        else Just toolCallsList
                  }

              choice =
                Choice
                  { choiceIndex = 0,
                    choiceMessage = msg,
                    choiceFinishReason = stopReason
                  }

              usage = case KM.lookup "usage" obj of
                Just (Object uo) ->
                  let inTok = case KM.lookup "input_tokens" uo of
                        Just (Number n) -> round n
                        _ -> 0
                      outTok = case KM.lookup "output_tokens" uo of
                        Just (Number n) -> round n
                        _ -> 0
                   in Just (Usage inTok outTok (inTok + outTok))
                _ -> Nothing

          pure $
            Right $
              ChatResponse
                { responseId = respId,
                  responseModel = respModel,
                  responseChoices = [choice],
                  responseUsage = usage
                }
        _ -> pure $ Left $ InvalidResponse "Anthropic response root was not an object"

parseAnthropicContent :: [Value] -> (Text, [ToolCall])
parseAnthropicContent blocks =
  let textParts =
        mapMaybe
          ( \case
              Object o -> case (KM.lookup "type" o, KM.lookup "text" o) of
                (Just (String "text"), Just (String t)) -> Just t
                _ -> Nothing
              _ -> Nothing
          )
          blocks

      toolCalls =
        mapMaybe
          ( \case
              Object o -> case (KM.lookup "type" o, KM.lookup "id" o, KM.lookup "name" o, KM.lookup "input" o) of
                (Just (String "tool_use"), Just (String tid), Just (String tname), Just tinput) ->
                  let argsStr = decodeUtf8 (LB.toStrict (encode tinput))
                   in Just
                        ToolCall
                          { toolCallId = tid,
                            toolCallType = "function",
                            toolCallFunction =
                              FunctionCall
                                { funcName = tname,
                                  funcArguments = argsStr
                                }
                          }
                _ -> Nothing
              _ -> Nothing
          )
          blocks
   in (T.intercalate "\n" textParts, toolCalls)

toHeaderName :: Text -> HeaderName
toHeaderName = CI.mk . encodeUtf8
