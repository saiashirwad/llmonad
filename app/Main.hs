{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLMonad
import System.Environment (getEnv)

-- | Example data types
data Person = Person
  { name :: Text,
    age :: Maybe Int
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data Analysis = Analysis
  { sentiment :: Text,
    keywords :: [Text]
  }
  deriving (Show, Generic, FromJSON, ToJSON)

instance Schema Person

instance Schema Analysis

olderPerson :: Person -> Person -> LLM Person
olderPerson = ask' "Compare these two people and return the older person"

adder :: Int -> Int -> LLM Int
adder = ask' "add these two numbers"

main :: IO ()
main = do
  apiKey <- T.pack <$> getEnv "GROQ_API_KEY"
  let env =
        LLMEnv
          { endpoint = "https://api.groq.com/openai/v1/chat/completions",
            apiKey = apiKey
          }

  let exec x = runLLM env x >>= print

  -- Example usage
  exec $ adder 2 5
  exec $ ask @Bool "Is this list empty?" [1, 2, 3 :: Int]

  let bob = Person "Bob" (Just 25)
  let charlie = Person "Charlie" (Just 40)
  exec $ olderPerson bob charlie
