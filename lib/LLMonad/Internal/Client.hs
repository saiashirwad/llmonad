{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LLMonad.Internal.Client
  ( callLLM,
    LLMEnv (..),
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson (FromJSON (..), ToJSON, Value, eitherDecode', encode, withObject, (.:), (.:?))
import Data.Aeson.Types (KeyValue ((.=)), object)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as LB
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import LLMonad.Internal.Schema (Schema (..))
import Network.HTTP.Simple
  ( Request,
    getResponseBody,
    getResponseStatusCode,
    httpLBS,
    parseRequest,
    setRequestBodyJSON,
    setRequestHeader,
    setRequestMethod,
  )

-- | Groq API response types
data GroqChoice = GroqChoice
  { index :: Maybe Int,
    message :: GroqMessage,
    logprobs :: Maybe Value,
    finish_reason :: Maybe Text
  }
  deriving (Show, Generic, FromJSON)

data GroqMessage = GroqMessage
  { role :: Maybe Text,
    content :: Text
  }
  deriving (Show, Generic, FromJSON)

data GroqResponse = GroqResponse
  { choices :: [GroqChoice],
    groqId :: Maybe Text,
    groqObject :: Maybe Text,
    created :: Maybe Int,
    model :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON GroqResponse where
  parseJSON = withObject "GroqResponse" $ \o -> GroqResponse
    <$> o .: "choices"
    <*> o .:? "id"
    <*> o .:? "object"
    <*> o .:? "created"
    <*> o .:? "model"

data LLMEnv = LLMEnv
  { endpoint :: String,
    apiKey :: Text
  }

-- | Make a call to the LLM with a prompt
callLLM ::
  forall a m.
  (FromJSON a, ToJSON a, Schema a, MonadIO m, MonadReader LLMEnv m) =>
  Text ->
  m a
callLLM prompt = do
  LLMEnv {..} <- asks id
  req0 <- liftIO $ parseRequest endpoint

  let exampleValue = genericSchema @a
      exampleJson = T.strip $ decodeUtf8 $ LB.toStrict $ encode exampleValue
      systemPrompt = buildSystemPrompt exampleJson
      jsonBody = buildRequestBody prompt systemPrompt
      req = buildRequest apiKey jsonBody req0

  resp <- liftIO $ httpLBS req
  let body = getResponseBody resp
      statusCode = getResponseStatusCode resp

  if statusCode /= 200
    then error ("HTTP Error " <> show statusCode <> ": " <> LB.unpack body)
    else parseResponse body

-- | Build the system prompt for the LLM
buildSystemPrompt :: Text -> Text
buildSystemPrompt exampleJson =
  "You are a JSON generator. Return ONLY valid JSON that matches this exact structure. "
    <> "No explanations, no markdown, no code blocks, just the raw JSON. "
    <> "CRITICAL: If the example is a primitive JSON value (like \"example\", 42, true), return ONLY a primitive value, NOT wrapped in an object. "
    <> "If the example is \"example\", return something like \"Charlie\" (with quotes for strings). "
    <> "If the example is 42, return something like 7 (without quotes for numbers). "
    <> "If the example is true, return true or false (without quotes for booleans). "
    <> "For objects, return ONLY the fields shown in the example, with the exact same names. "
    <> "Example of expected format: "
    <> exampleJson
    <> "\n\nThe user will provide a task and inputs. Complete the task and return the result in the exact format shown above."

-- | Build the request body
buildRequestBody :: Text -> Text -> Value
buildRequestBody prompt systemPrompt =
  object
    [ "model" .= ("llama3-8b-8192" :: Text),
      "messages"
        .= [ object
               [ "role" .= ("system" :: Text),
                 "content" .= systemPrompt
               ],
             object
               [ "role" .= ("user" :: Text),
                 "content" .= prompt
               ]
           ],
      "stream" .= False,
      "temperature" .= (0.0 :: Double),
      "max_tokens" .= (1024 :: Int)
    ]

-- | Build the HTTP request
buildRequest :: Text -> Value -> Request -> Request
buildRequest apiKey jsonBody =
  setRequestMethod "POST"
    . setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 apiKey]
    . setRequestHeader "Content-Type" ["application/json"]
    . setRequestBodyJSON jsonBody

-- | Parse the response from the LLM
parseResponse :: forall a m. (FromJSON a, MonadIO m) => ByteString -> m a
parseResponse body = do
  case eitherDecode' body of
    Left e -> error ("Groq response decode failed: " <> e <> "\nRaw: " <> LB.unpack body)
    Right groqResp -> case choices groqResp of
      [] -> error "No choices in Groq response"
      (choice : _) -> do
        let contentText = content (message choice)
        case eitherDecode' (LB.fromStrict $ encodeUtf8 contentText) of
          Left e -> error ("JSON decode failed: " <> e <> "\nContent: " <> T.unpack contentText)
          Right a -> pure a
