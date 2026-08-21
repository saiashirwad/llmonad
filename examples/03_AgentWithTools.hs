{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import LLMonad
import System.Environment (lookupEnv)

data CurrencyConvertArgs = CurrencyConvertArgs
  { amount :: Double,
    fromCurrency :: Text,
    toCurrency :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, HasSchema)

data CurrencyConvertRes = CurrencyConvertRes
  { convertedAmount :: Double,
    rate :: Double
  }
  deriving (Show, Generic, FromJSON, ToJSON, HasSchema)

currencyTool :: Tool
currencyTool =
  defToolSync
    "convert_currency"
    "Convert an amount from one fiat currency to another (USD, EUR, GBP, JPY)"
    ( \(CurrencyConvertArgs amt fromCurr toCurr) ->
        let rateVal = case (fromCurr, toCurr) of
              ("USD", "EUR") -> 0.92
              ("EUR", "USD") -> 1.09
              ("USD", "GBP") -> 0.79
              ("USD", "JPY") -> 155.0
              _ -> 1.0
         in CurrencyConvertRes (amt * rateVal) rateVal
    )

main :: IO ()
main = do
  putStrLn "=== Example 03: Autonomous ReAct Agent ==="

  deepseekKey <- lookupEnv "DEEPSEEK_API_KEY"
  openaiKey <- lookupEnv "OPENAI_API_KEY"
  anthropicKey <- lookupEnv "ANTHROPIC_API_KEY"

  let provider = case (deepseekKey, openaiKey, anthropicKey) of
        (Just k, _, _) -> deepseek (T.pack k)
        (_, Just k, _) -> openai (T.pack k)
        (_, _, Just k) -> anthropic (T.pack k)
        (Nothing, Nothing, Nothing) -> ollama

  let config = defaultConfig provider

  result <- runLLM config $ do
    let tools = [currencyTool]
    ans <- runAgentWith tools "How much is 250 USD in EUR, and how much is 500 USD in JPY?"
    liftIO $ TIO.putStrLn $ "Agent Response:\n" <> ans

  case result of
    Left err -> putStrLn $ "Error: " <> show err
    Right () -> putStrLn "\nAgent finished."
