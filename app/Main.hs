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

data LLMEnv = LLMEnv
  { endpoint :: String,
    apiKey :: Text
  }
  deriving (Show)

newtype LLM a = LLM {unLLM :: ReaderT LLMEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader LLMEnv)

runLLM :: LLMEnv -> LLM a -> IO a
runLLM env = flip runReaderT env . unLLM

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

callLLM ::
  forall a.
  (FromJSON a, ToJSON a, Schema a) =>
  Text ->
  LLM a
callLLM prompt = do
  LLMEnv {..} <- asks id
  req0 <- liftIO $ Network.HTTP.Simple.parseRequest endpoint

  let exampleValue = genericSchema @a
      exampleJson = T.strip $ decodeUtf8 $ LB.toStrict $ encode exampleValue
      systemPrompt =
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

  if statusCode /= 200
    then error ("HTTP Error " <> show statusCode <> ": " <> LB.unpack body)
    else do
      case eitherDecode' body of
        Left e -> error ("Groq response decode failed: " <> e <> "\nRaw: " <> LB.unpack body)
        Right groqResp -> case choices groqResp of
          [] -> error "No choices in Groq response"
          (choice : _) -> do
            let contentText = content (message choice)
            case eitherDecode' (LB.fromStrict $ encodeUtf8 contentText) of
              Left e -> error ("JSON decode failed: " <> e <> "\nContent: " <> T.unpack contentText)
              Right a -> pure a

class Schema a where
  genericSchema :: Value
  default genericSchema :: (Generic a, GSchema (Rep a)) => Value
  genericSchema = gschema (from (undefined :: a))

class GSchema f where
  gschema :: f p -> Value

instance (GSchema a, GSchema b) => GSchema (a :*: b) where
  gschema _ = case (gschema (undefined :: a p), gschema (undefined :: b p)) of
    (Object o1, Object o2) -> Object (o1 <> o2)
    (v1, v2) -> Array (V.fromList [v1, v2])

instance (GSchema a, GSchema b) => GSchema (a :+: b) where
  gschema _ = gschema (undefined :: a p) -- Just use left constructor as example

instance (GSchema a) => GSchema (M1 D c a) where
  gschema _ = gschema (undefined :: a p)

instance (GSchema a) => GSchema (M1 C c a) where
  gschema _ = gschema (undefined :: a p)

instance GSchema U1 where
  gschema _ = Object mempty

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

ask :: forall a b. (FromJSON a, ToJSON a, Schema a, ToJSON b) => Text -> b -> LLM a
ask stem input = callLLM (stem <> "\n\nInput: " <> decodeUtf8 (LB.toStrict $ encode input))

ask' :: forall a b c. (FromJSON a, ToJSON a, Schema a, ToJSON b, ToJSON c) => Text -> b -> c -> LLM a
ask' stem a b = callLLM (stem <> "\n\nFirst: " <> decodeUtf8 (LB.toStrict $ encode a) <> "\nSecond: " <> decodeUtf8 (LB.toStrict $ encode b))

data Person = Person {name :: Text, age :: Maybe Int}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Analysis = Analysis {sentiment :: Text, keywords :: [Text]}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Schema Person

instance Schema Analysis

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
  let env = LLMEnv {endpoint = "https://api.groq.com/openai/v1/chat/completions", apiKey = "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"}

  -- Examples with polymorphic inputs and outputs
  putStrLn "=== Polymorphic LLM Examples ==="

  -- 1. Numbers in, numbers out
  result1 <- runLLM env $ adder 2 5
  putStrLn $ "2 + 5 = " <> show result1

  -- 2. Lists as input
  result2 <- runLLM env $ ask @Bool "Is this list empty?" [1, 2, 3 :: Int]
  putStrLn $ "Is [1,2,3] empty? " <> show result2

  -- 3. Custom types as input
  result3 <- runLLM env $ ask @Analysis "Analyze this person's data and provide sentiment and keywords" (Person "Alice" (Just 30))
  putStrLn $ "Analysis of Alice: " <> show result3

  -- 4. Multiple custom types
  result4 <- runLLM env $ comparePersons (Person "Bob" (Just 25)) (Person "Charlie" (Just 40))
  putStrLn $ "Who is older? " <> show result4

  -- 5. Complex nested structures
  result5 <-
    runLLM env $
      ask @Bool
        "Does this analysis indicate positivity?"
        (Analysis "happy" ["joy", "excitement"])
  putStrLn $ "Is analysis positive? " <> show result5

  -- 6. Original text processing example
  putStrLn "\n=== Original Text Processing Example ==="
  (person, analysis, summary) <-
    runLLM env $
      exampleTextProcessing "Albert Einstein was a theoretical physicist who developed the theory of relativity."
  print person
  print analysis
  print summary
  where
    adder :: Int -> Int -> LLM Int
    adder = ask' "add these two numbers"

    comparePersons :: Person -> Person -> LLM Text
    comparePersons = ask' "Compare these two people and return the name of the older one"
