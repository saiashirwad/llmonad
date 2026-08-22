{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | LLMonad Example 4: Template Haskell QuasiQuotes & Tool Generation
-- Demonstrates compile-time prompt interpolation with [prompt| ... |]
-- and automatic Haskell function tool generation with makeTool.
module Main where

import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Effectful
import GHC.Generics (Generic)
import LLMonad
import System.Environment (lookupEnv)

data TaxLookupArgs = TaxLookupArgs
  { country :: Text
  , baseAmount :: Double
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

-- | Regular Haskell function to be turned into a tool
computeTax :: TaxLookupArgs -> IO Double
computeTax args = pure $ case T.toUpper (country args) of
  "DE" -> baseAmount args * 0.19
  "UK" -> baseAmount args * 0.20
  "US" -> baseAmount args * 0.08
  _ -> baseAmount args * 0.10

-- Close the declaration group so Template Haskell can reify computeTax
$(return [])

-- Auto-generate a Tool using Template Haskell
taxTool :: ToolIO
taxTool = $(makeTool 'computeTax)

workflow :: (LLM :> es, IOE :> es) => Eff es ()
workflow = do
  liftIO (putStrLn "--- 1. Compile-Time [prompt| ... |] Interpolation ---")
  let customerName = "Alice" :: Text
      item = "Laptop" :: Text
      price = 1200.00 :: Double
      query = [prompt|Customer #{customerName} purchased #{item} for $#{price}. Compute the applicable sales tax in Germany.|]
  liftIO (TIO.putStrLn ("Interpolated Prompt:\n" <> query <> "\n"))

  liftIO (putStrLn "--- 2. Autonomous Execution with makeTool Splice ---")
  answer <- runAgent [liftTool taxTool] query
  liftIO (TIO.putStrLn ("Agent Answer:\n" <> answer))

main :: IO ()
main = do
  mKey <- lookupEnv "OPENAI_API_KEY"
  case mKey of
    Just k -> do
      let cfg = defaultConfig (openAIProvider (T.pack k)) "gpt-4o-mini"
      runEff (runLLMHTTP cfg workflow)
    Nothing -> do
      putStrLn "Note: OPENAI_API_KEY not set; executing against pure in-memory mock handler.\n"
      let script =
            [ Right (toolResp [ToolCall "call-1" "computeTax" (toJSON (TaxLookupArgs "DE" 1200.00))])
            , Right (textResp "The applicable sales tax for Germany (19%) on a $1200 Laptop is $228.00.")
            ]
      (res, _) <- runEff (runLLMMock script workflow)
      pure res
