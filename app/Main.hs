{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), asks)
import Data.Aeson (FromJSON, ToJSON, Value (..), eitherDecode', encode, toJSON)
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (KeyValue ((.=)), object)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as LB
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Vector qualified as V
import GHC.Generics
import Network.HTTP.Simple
  ( getResponseBody,
    getResponseStatusCode,
    httpLBS,
    parseRequest,
    setRequestBodyJSON,
    setRequestHeader,
    setRequestMethod,
  )

--------------------------------------------------------------------------------
-- ❶  A *very* thin LLM monad --------------------------------------------------
--------------------------------------------------------------------------------

data LLMEnv = LLMEnv
  { -- | e.g. "https://api.groq.com/openai/v1/chat/completions"
    endpoint :: String,
    apiKey :: Text
  }
  deriving (Show)

newtype LLM a = LLM {unLLM :: ReaderT LLMEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader LLMEnv)

-- | Run an LLM action with an environment.
runLLM :: LLMEnv -> LLM a -> IO a
runLLM env = flip runReaderT env . unLLM

--------------------------------------------------------------------------------
-- ❷  Groq API response types -------------------------------------------------
--------------------------------------------------------------------------------

newtype GroqChoice = GroqChoice
  { message :: GroqMessage
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

newtype GroqMessage = GroqMessage
  { content :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

newtype GroqResponse = GroqResponse
  { choices :: [GroqChoice]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

--------------------------------------------------------------------------------
-- ❸  Generic helper that actually fires the request ---------------------------
--------------------------------------------------------------------------------

callLLM ::
  forall a.
  (FromJSON a, ToJSON a, Schema a) =>
  -- | *Concrete* prompt (already assembled)
  Text ->
  LLM a
callLLM prompt = do
  LLMEnv {..} <- asks id
  req0 <- liftIO $ Network.HTTP.Simple.parseRequest endpoint

  -- Generate an example of the expected type using Schema instance
  let exampleValue = genericSchema @a
      exampleJson = T.strip $ decodeUtf8 $ LB.toStrict $ encode exampleValue
      systemPrompt =
        "You are a JSON generator. Return ONLY valid JSON that matches this exact structure. "
          <> "No explanations, no markdown, no code blocks, just the raw JSON. "
          <> "IMPORTANT: For primitive types (string, number, boolean), return just the raw value, not wrapped in an object. "
          <> "For objects, return ONLY the fields shown in the example, with the exact same names. "
          <> "Example of expected format: "
          <> exampleJson

  let jsonBody =
        object
          [ "model" .= ("llama3-8b-8192" :: Text), -- Groq model
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
  let req =
        Network.HTTP.Simple.setRequestMethod "POST" $
          Network.HTTP.Simple.setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 apiKey] $
            Network.HTTP.Simple.setRequestHeader "Content-Type" ["application/json"] $
              Network.HTTP.Simple.setRequestBodyJSON jsonBody req0
  resp <- liftIO $ Network.HTTP.Simple.httpLBS req
  let body :: ByteString
      body = Network.HTTP.Simple.getResponseBody resp
      statusCode = Network.HTTP.Simple.getResponseStatusCode resp

  -- Check if we got a successful response
  if statusCode /= 200
    then error ("HTTP Error " <> show statusCode <> ": " <> LB.unpack body)
    else do
      -- Parse Groq response and extract content
      case eitherDecode' body of
        Left e -> error ("Groq response decode failed: " <> e <> "\nRaw: " <> LB.unpack body)
        Right groqResp -> case choices groqResp of
          [] -> error "No choices in Groq response"
          (choice : _) -> do
            let contentText = content (message choice)

            -- Parse the content directly as our target type
            case eitherDecode' (LB.fromStrict $ encodeUtf8 contentText) of
              Left e -> error ("JSON decode failed: " <> e <> "\nContent: " <> T.unpack contentText)
              Right a -> pure a

--------------------------------------------------------------------------------
-- ❹  "Aeson magic" domain types ----------------------------------------------
--------------------------------------------------------------------------------

data Person = Person {name :: Text, age :: Maybe Int}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Analysis = Analysis {sentiment :: Text, keywords :: [Text]}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- ❺  Tiny combinator layer – *this is what the screenshot shows* --------------
--------------------------------------------------------------------------------

-- | Type class for generating JSON schema examples
class Schema a where
  genericSchema :: Value
  default genericSchema :: (Generic a, GSchema (Rep a)) => Value
  genericSchema = gschema (from (undefined :: a))

-- Generic schema generation
class GSchema f where
  gschema :: f p -> Value

-- Product types (records)
instance (GSchema a, GSchema b) => GSchema (a :*: b) where
  gschema _ = case (gschema (undefined :: a p), gschema (undefined :: b p)) of
    (Object o1, Object o2) -> Object (o1 <> o2)
    (v1, v2) -> Array (V.fromList [v1, v2])

-- Sum types (constructors)
instance (GSchema a, GSchema b) => GSchema (a :+: b) where
  gschema _ = gschema (undefined :: a p) -- Just use left constructor as example

-- Metadata (for data types and constructors)
instance (GSchema a) => GSchema (M1 D c a) where
  gschema _ = gschema (undefined :: a p)

instance (GSchema a) => GSchema (M1 C c a) where
  gschema _ = gschema (undefined :: a p)

-- Constructor arguments
instance GSchema U1 where
  gschema _ = Object mempty

-- Record fields (selector metadata wrapping K1)
instance (Selector s, Schema a) => GSchema (M1 S s (K1 i a)) where
  gschema _ =
    let fieldName = T.pack (selName (undefined :: M1 S s (K1 i a) p))
        fieldValue = genericSchema @a
     in Object (KM.singleton (K.fromText fieldName) fieldValue)

-- Basic instances
instance Schema Bool where
  genericSchema = Bool True

instance Schema Text where
  genericSchema = String "example"

instance Schema Int where
  genericSchema = Number 42

instance (Schema a) => Schema (Maybe a) where
  genericSchema = case genericSchema @a of
    Null -> Null
    v -> v

instance (Schema a) => Schema [a] where
  genericSchema = Array (V.singleton (genericSchema @a))

-- These instances are automatically derived!
instance Schema Person

instance Schema Analysis

-- | One-argument helper (screenshot's 'ask').
ask ::
  forall a.
  (FromJSON a, ToJSON a, Schema a) =>
  -- | Prompt stem
  Text ->
  -- | User input
  Text ->
  LLM a
ask stem input = callLLM (stem <> "\n\nInput: " <> input)

-- | Two-argument helper (screenshot's 'ask'').
ask' ::
  forall a.
  (FromJSON a, ToJSON a, Schema a) =>
  -- | Prompt stem
  Text ->
  -- | First  argument
  Text ->
  -- | Second argument
  Text ->
  LLM a
ask' stem a b = callLLM (stem <> "\n\nFirst: " <> a <> "\nSecond: " <> b)

compareDates :: Text -> Text -> LLM Bool
compareDates = ask' "Is the first date before the second date? Return true or false as JSON boolean."

summarize :: Text -> LLM Text
summarize = ask "Summarize this text in one sentence. Return as JSON string."

extractPersonInfo :: Text -> LLM Person
extractPersonInfo = ask "Extract person information from this text."

analyzeTextSentiment :: Text -> LLM Analysis
analyzeTextSentiment = ask "Analyze this text for sentiment and keywords."

exampleTextProcessing :: Text -> LLM (Person, Analysis, Text)
exampleTextProcessing input = do
  person <- extractPersonInfo input
  sentiment <- analyzeTextSentiment input
  summary <- summarize input
  pure (person, sentiment, summary)

main :: IO ()
main = do
  let env =
        LLMEnv
          { endpoint = "https://api.groq.com/openai/v1/chat/completions",
            apiKey = "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"
          }
  putStrLn "Running exampleTextProcessing…"
  (p, a, s) <-
    runLLM env $
      exampleTextProcessing
        "Albert Einstein was a theoretical physicist who developed the theory of relativity."
  print p
  print a
  print s
