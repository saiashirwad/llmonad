{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | LLMonad Example 3: Autonomous Agent with Tools & Cycle Detection
-- Demonstrates registering type-safe tools, running multi-step ReAct agent loops,
-- structured agent outputs, and loop protection.
module Main where

import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Effectful
import GHC.Generics (Generic)
import LLMonad
import System.Environment (lookupEnv)

data StockPriceArgs = StockPriceArgs
  { ticker :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data ConversionArgs = ConversionArgs
  { amount :: Double
  , fromCurrency :: Text
  , toCurrency :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data PortfolioValuation = PortfolioValuation
  { symbol :: Text
  , priceUSD :: Double
  , priceEUR :: Double
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

-- | IO tool looking up stock prices
stockPriceTool :: Tool (Eff es)
stockPriceTool = mkTool "stock_price" "Look up current stock price in USD for a given ticker" $ \(args :: StockPriceArgs) -> do
  pure $ case T.toUpper (ticker args) of
    "AAPL" -> (225.50 :: Double)
    "MSFT" -> 440.20
    "GOOGL" -> 180.10
    _ -> 100.00

-- | Pure tool converting currency
currencyConversionTool :: Tool (Eff es)
currencyConversionTool = toolSync "convert_currency" "Convert amount between currencies" $ \(args :: ConversionArgs) ->
  let rate = if fromCurrency args == "USD" && toCurrency args == "EUR" then 0.92 else 1.0
   in amount args * rate

tools :: [Tool (Eff es)]
tools = [stockPriceTool, currencyConversionTool]

workflow :: (LLM :> es, IOE :> es) => Eff es ()
workflow = do
  liftIO (putStrLn "--- 1. Multi-Step ReAct Agent (runAgent) ---")
  answer <- runAgent tools "What is the stock price of AAPL in USD, and what is that amount in EUR?"
  liftIO (TIO.putStrLn ("Agent Answer:\n" <> answer))

  liftIO (putStrLn "\n--- 2. Autonomous Agent with Structured Result (runAgentStructured) ---")
  valuation <- runAgentStructured @PortfolioValuation tools "Get AAPL price in USD and EUR as structured record."
  liftIO (putStrLn ("Structured Valuation:\n" <> show valuation))

main :: IO ()
main = do
  mKey <- lookupEnv "OPENAI_API_KEY"
  case mKey of
    Just k -> do
      let cfg = defaultConfig (openAIProvider (T.pack k)) "gpt-4o-mini"
      runEff (runLLMHTTP cfg workflow)
    Nothing -> do
      putStrLn "Note: OPENAI_API_KEY not set; executing against pure in-memory mock handler.\n"
      let sampleValuation = PortfolioValuation "AAPL" 225.50 (225.50 * 0.92)
          script =
            [ Right (toolResp [ToolCall "c1" "stock_price" (toJSON (StockPriceArgs "AAPL"))])
            , Right (toolResp [ToolCall "c2" "convert_currency" (toJSON (ConversionArgs 225.50 "USD" "EUR"))])
            , Right (textResp "AAPL is $225.50 USD, which equals 207.46 EUR.")
            , Right (toolResp [ToolCall "c3" "stock_price" (toJSON (StockPriceArgs "AAPL"))])
            , Right (structuredResp (toJSON sampleValuation))
            ]
      (res, _) <- runEff (runLLMMock script workflow)
      pure res
