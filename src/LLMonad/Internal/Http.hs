{-# LANGUAGE OverloadedStrings #-}

-- | Thin http-client wrappers shared by the transports: JSON POST with
-- error classification, and chunk-streaming POST for SSE bodies.
module LLMonad.Internal.Http
  ( postJSON
  , postJSONStream
  , defaultTimeoutMicros
  , maxResponseBodyBytes
  , timeoutFor
  , parseRetryAfter
  , trySync
  ) where

import Control.Exception (SomeAsyncException (..), SomeException, fromException, throwIO, try)
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
  ( BodyReader
  , Request
  , RequestBody (RequestBodyLBS)
  , ResponseTimeout
  , brRead
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

-- | Default response body size limit (10 MiB) to prevent unbounded memory growth.
maxResponseBodyBytes :: Int
maxResponseBodyBytes = 10 * 1024 * 1024

-- | Run an IO action and catch only synchronous exceptions.
-- Asynchronous exceptions (such as ThreadKilled or timeout cancellations)
-- are immediately rethrown to preserve concurrency control.
trySync :: IO a -> IO (Either SomeException a)
trySync act = try act >>= \case
  Left ex -> case fromException ex of
    Just (SomeAsyncException _) -> throwIO ex
    Nothing -> pure (Left ex)
  Right a -> pure (Right a)

timeoutFor :: Maybe Int -> ResponseTimeout
timeoutFor mSecs = responseTimeoutMicro (maybe defaultTimeoutMicros toMicros mSecs)
  where
    maxSecs = maxBound `quot` 1000000
    toMicros s
      | s <= 0 = 0
      | s > maxSecs = maxBound
      | otherwise = s * 1000000

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
      trySync (withResponse req globalManager $ \res -> do
        let status = statusCode (responseStatus res)
            hdrs = responseHeaders res
        bodyRes <- drainWithLimit maxResponseBodyBytes (responseBody res)
        case bodyRes of
          Left err -> pure (Left err)
          Right body ->
            if statusIsSuccessful (responseStatus res)
              then pure (Right (status, hdrs, body))
              else pure (Left (classifyHttpError status hdrs body)))
        >>= \case
          Left ex -> pure (Left (HttpError (T.pack (show (ex :: SomeException)))))
          Right r -> pure r

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
      trySync (withResponse req globalManager $ \res ->
        let status = statusCode (responseStatus res)
            hdrs = responseHeaders res
         in if statusIsSuccessful (responseStatus res)
              then pump (responseBody res) 0
              else do
                bodyRes <- drainWithLimit maxResponseBodyBytes (responseBody res)
                case bodyRes of
                  Left err -> pure (Left err)
                  Right body -> pure (Left (classifyHttpError status hdrs body)))
        >>= \case
          Left ex -> pure (Left (HttpError (T.pack (show (ex :: SomeException)))))
          Right r -> pure r
  where
    pump body total = do
      chunk <- brRead body
      if BS.null chunk
        then pure (Right ())
        else
          let newTotal = total + BS.length chunk
           in if newTotal > maxResponseBodyBytes
                then pure (Left (HttpError ("Response stream exceeded maximum limit of " <> T.pack (show maxResponseBodyBytes) <> " bytes")))
                else do
                  cb chunk
                  pump body newTotal

drainWithLimit :: Int -> BodyReader -> IO (Either LLMError LBS.ByteString)
drainWithLimit limit body = go 0 []
  where
    go total acc = do
      chunk <- brRead body
      if BS.null chunk
        then pure (Right (LBS.fromChunks (reverse acc)))
        else
          let newTotal = total + BS.length chunk
           in if newTotal > limit
                then pure (Left (HttpError ("Response body exceeded maximum limit of " <> T.pack (show limit) <> " bytes")))
                else go newTotal (chunk : acc)

buildRequest ::
  Text ->
  [(Text, Text)] ->
  Maybe Int ->
  Value ->
  IO (Either LLMError Request)
buildRequest url extraHeaders timeoutSecs payload =
  trySync (parseRequest (ensureScheme url)) >>= \case
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
parseRetryAfter hdrs = do
  bs <- lookup "Retry-After" hdrs
  let raw = T.strip (decodeUtf8With lenientDecode bs)
      clean = T.dropWhileEnd (\c -> c == 's' || c == 'S') raw
  case reads (T.unpack clean) of
    [(n, "")] | n >= 0 -> Just n
    _ -> case reads (T.unpack clean) :: [(Double, String)] of
      [(d, "")] | d >= 0 -> Just (ceiling d)
      _ -> Nothing
