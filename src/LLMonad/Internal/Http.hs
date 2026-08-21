{-# LANGUAGE OverloadedStrings #-}

-- | Thin http-client wrappers shared by the transports: JSON POST with
-- error classification, and chunk-streaming POST for SSE bodies.
module LLMonad.Internal.Http
  ( postJSON
  , postJSONStream
  , defaultTimeoutMicros
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (mk)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import LLMonad.Error (LLMError (..))
import LLMonad.Internal.GlobalManager (globalManager)
import Network.HTTP.Client
  ( Request
  , RequestBody (RequestBodyLBS)
  , ResponseTimeout
  , brRead
  , httpLbs
  , method
  , parseRequest
  , requestBody
  , requestHeaders
  , responseBody
  , responseHeaders
  , responseStatus
  , responseTimeout
  , responseTimeoutMicro
  , withResponse
  )
import Network.HTTP.Client.TLS ()
import Network.HTTP.Types (Header, statusCode, statusIsSuccessful)

-- | 5 minutes: LLMs can be slow, especially with big prompts.
defaultTimeoutMicros :: Int
defaultTimeoutMicros = 300 * 1000 * 1000

timeoutFor :: Maybe Int -> ResponseTimeout
timeoutFor secs = responseTimeoutMicro (maybe defaultTimeoutMicros (* 1000000) secs)

-- | POST a JSON document; classify non-2xx responses into 'LLMError's.
--
-- Returns @(status, headers, body)@ for successful (2xx) calls.
postJSON ::
  Text ->
  -- | URL
  [(Text, Text)] ->
  -- | Extra headers
  Maybe Int ->
  -- | Timeout seconds ('Nothing' = default)
  Value ->
  -- | Payload
  IO (Either LLMError (Int, [Header], LBS.ByteString))
postJSON url extraHeaders timeoutSecs payload =
  buildRequest url extraHeaders timeoutSecs payload >>= \case
    Left e -> pure (Left e)
    Right req ->
      try (httpLbs req globalManager) >>= \case
        Left ex -> pure (Left (HttpError (T.pack (show (ex :: SomeException)))))
        Right res ->
          let status = statusCode (responseStatus res)
              body = responseBody res
           in if statusIsSuccessful (responseStatus res)
                then pure (Right (status, responseHeaders res, body))
                else pure (Left (classifyHttpError status (responseHeaders res) body))

-- | POST a JSON document and stream the response body through a callback.
--
-- Non-2xx statuses are fully drained and returned as errors instead of
-- being streamed.
postJSONStream ::
  Text ->
  [(Text, Text)] ->
  Maybe Int ->
  Value ->
  (BS.ByteString -> IO ()) ->
  IO (Either LLMError ())
postJSONStream url extraHeaders timeoutSecs payload cb =
  buildRequest url extraHeaders timeoutSecs payload >>= \case
    Left e -> pure (Left e)
    Right req ->
      try (withResponse req globalManager $ \res ->
            let status = statusCode (responseStatus res)
             in if statusIsSuccessful (responseStatus res)
                  then pump (responseBody res) (Right ())
                  else do
                    body <- drain (responseBody res)
                    pure (Left (classifyHttpError status (responseHeaders res) body)))
        >>= \case
          Left ex -> pure (Left (HttpError (T.pack (show (ex :: SomeException)))))
          Right r -> pure r
  where
    pump body acc = do
      chunk <- brRead body
      if BS.null chunk
        then pure acc
        else do
          cb chunk
          pump body acc
    drain body = go []
      where
        go acc = do
          chunk <- brRead body
          if BS.null chunk
            then pure (LBS.fromChunks (reverse acc))
            else go (chunk : acc)

buildRequest ::
  Text ->
  [(Text, Text)] ->
  Maybe Int ->
  Value ->
  IO (Either LLMError Request)
buildRequest url extraHeaders timeoutSecs payload =
  try (parseRequest (ensureScheme url)) >>= \case
    Left ex ->
      pure (Left (HttpError ("invalid URL " <> url <> ": " <> T.pack (show (ex :: SomeException)))))
    Right req0 ->
      let req =
            req0
              { method = "POST"
              , requestHeaders =
                  [("Content-Type", "application/json"), ("Accept", "application/json")]
                    ++ [(mk (encodeUtf8 k), encodeUtf8 v) | (k, v) <- extraHeaders]
              , requestBody = RequestBodyLBS (encode payload)
              , responseTimeout = timeoutFor timeoutSecs
              }
       in pure (Right req)

ensureScheme :: Text -> String
ensureScheme u = if "://" `T.isInfixOf` u then T.unpack u else "https://" <> T.unpack u

classifyHttpError :: Int -> [Header] -> LBS.ByteString -> LLMError
classifyHttpError status hdrs body
  | status == 429 =
      RateLimitError
        { rateLimitStatus = status
        , rateLimitBody = decodeUtf8With lenientDecode (LBS.toStrict body)
        , rateLimitRetryAfterSecs = parseRetryAfter hdrs
        }
  | otherwise =
      ApiError
        { apiStatus = status
        , apiBody = decodeUtf8With lenientDecode (LBS.toStrict body)
        }

parseRetryAfter :: [Header] -> Maybe Int
parseRetryAfter hdrs =
  case lookup "Retry-After" hdrs of
    Nothing -> Nothing
    Just bs ->
      case reads (T.unpack (decodeUtf8With lenientDecode bs)) of
        [(n, "")] -> Just n
        _ -> Nothing
