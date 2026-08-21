{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLMonad.Streaming
  ( -- * Streaming Chat
    streamChat,
    streamChatWithMessages,

    -- * Stream Parsing
    parseSSEChunk,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ask)
import Control.Monad.State (gets)
import Data.Aeson (Value (..), eitherDecodeStrict, encode)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Char8 qualified as BC
import Data.CaseInsensitive qualified as CI
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Vector qualified as V
import LLMonad.Client (LLMError (..))
import LLMonad.Core
import LLMonad.Prompt (assistant, user)
import LLMonad.Provider
import LLMonad.Tools
import LLMonad.Types
import Network.HTTP.Client
  ( Request (..),
    RequestBody (..),
    Response (..),
    brRead,
    newManager,
    parseRequest,
    withResponse,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- | Stream tokens from the model in real time and execute callback on each text chunk
streamChat :: (Text -> IO ()) -> Text -> LLM Text
streamChat onToken prompt = do
  appendHistory (user prompt)
  hist <- getHistory
  streamChatWithMessages onToken hist

-- | Stream tokens from a specific sequence of messages
streamChatWithMessages :: (Text -> IO ()) -> [Message] -> LLM Text
streamChatWithMessages onToken msgs = do
  LLMConfig {..} <- ask
  toolsMap <- gets stateTools

  let allMsgs = case configSystemPrompt of
        Just sysPrompt ->
          if any (\m -> messageRole m == SystemRole) msgs
            then msgs
            else systemMsg sysPrompt : msgs
        Nothing -> msgs

      toolsList = Map.elems toolsMap
      toolsVal =
        if null toolsList
          then Nothing
          else
            Just $
              if isAnthropicProtocol configProvider
                then map toolToAnthropicTool toolsList
                else map toolToOpenAITool toolsList

      chatReq =
        ChatRequest
          { reqModel = configModel,
            reqMessages = allMsgs,
            reqTemperature = configTemperature,
            reqMaxTokens = configMaxTokens,
            reqStream = True,
            reqTools = toolsVal,
            reqResponseFormat = Nothing
          }

      baseUrl = providerBaseUrl configProvider
      endpoint = providerChatEndpoint configProvider
      fullUrl = T.unpack (baseUrl <> endpoint)

  reqResult <- liftIO $ try (parseRequest fullUrl)
  case reqResult of
    Left (e :: SomeException) ->
      throwError $ NetworkError ("Failed to parse streaming URL: " <> T.pack (show e))
    Right initReq -> do
      let headers =
            [ ("Content-Type", "application/json"),
              ("Accept", "text/event-stream")
            ]
              ++ providerAuthHeaders configProvider

          httpHeaders = map (\(k, v) -> (CI.mk (encodeUtf8 k), encodeUtf8 v)) headers
          reqBody = encode chatReq

          finalReq =
            initReq
              { method = "POST",
                requestHeaders = httpHeaders,
                requestBody = RequestBodyLBS reqBody
              }

      accumRef <- liftIO $ newIORef ""

      let consumeStream bodyReader = do
            let loop = do
                  chunkBs <- brRead bodyReader
                  if BC.null chunkBs
                    then pure ()
                    else do
                      let textLines = T.lines (decodeUtf8 chunkBs)
                      mapM_ (processLine accumRef onToken) textLines
                      loop
            loop

      streamRes <- liftIO $ try $ do
        mgr <- maybe (newManager tlsManagerSettings) pure configHttpManager
        withResponse finalReq mgr (\resp -> consumeStream (responseBody resp))

      case streamRes of
        Left (e :: SomeException) ->
          throwError $ NetworkError ("Streaming error: " <> T.pack (show e))
        Right () -> do
          fullText <- liftIO $ readIORef accumRef
          appendHistory (assistant fullText)
          pure fullText

processLine :: IORef Text -> (Text -> IO ()) -> Text -> IO ()
processLine accumRef onChunk line = do
  let stripped = T.strip line
  if "data: [DONE]" `T.isPrefixOf` stripped || T.null stripped
    then pure ()
    else
      if "data: " `T.isPrefixOf` stripped
        then do
          let jsonText = T.drop 6 stripped
          case parseSSEChunk jsonText of
            Just tokenText -> do
              onChunk tokenText
              curr <- readIORef accumRef
              writeIORef accumRef (curr <> tokenText)
            Nothing -> pure ()
        else pure ()

-- | Parse a JSON payload from an SSE line
parseSSEChunk :: Text -> Maybe Text
parseSSEChunk jsonText =
  case eitherDecodeStrict (encodeUtf8 jsonText) of
    Right (Object obj) ->
      -- Check OpenAI format
      case KM.lookup "choices" obj of
        Just (Array arr) | not (V.null arr) ->
          case V.head arr of
            Object choiceObj ->
              case KM.lookup "delta" choiceObj of
                Just (Object deltaObj) ->
                  case KM.lookup "content" deltaObj of
                    Just (String c) -> Just c
                    _ -> Nothing
                _ -> Nothing
            _ -> Nothing
        _ ->
          -- Check Anthropic format
          case KM.lookup "delta" obj of
            Just (Object deltaObj) ->
              case KM.lookup "text" deltaObj of
                Just (String t) -> Just t
                _ -> Nothing
            _ -> Nothing
    _ -> Nothing
