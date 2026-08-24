{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{- | LLMonad Example 4: Template Haskell QuasiQuotes & Tool Generation
Demonstrates compile-time prompt interpolation with [prompt| ... |]
and automatic Haskell function tool generation with makeTool.
-}
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

{- | Wire the generated tool onto any model runtime. This is the only place a
model is named; swapping DeepSeek, OpenAI, or a mock script changes nothing else.
-}
taxAgent :: ModelRuntime es -> Agent es Text Text
taxAgent runtime =
    bind
        runtime
        (tools [hoistTool liftIO taxTool])
        (textAgent "Answer sales-tax questions using the provided tool." id)

query :: Text
query =
    let customerName = "Alice" :: Text
        item = "Laptop" :: Text
        price = 1200.00 :: Double
     in [prompt|Customer #{customerName} purchased #{item} for $#{price}. Compute the applicable sales tax in Germany.|]

main :: IO ()
main = do
    mKey <- lookupEnv "OPENAI_API_KEY"
    case mKey of
        Just k -> do
            putStrLn "--- Live: OpenAI with makeTool Splice ---"
            reply <- runEff (invoke (taxAgent (model (openAIProvider (T.pack k)) "gpt-4o-mini")) query)
            TIO.putStrLn ("Agent Answer:\n" <> reply)
        Nothing -> do
            putStrLn "Note: OPENAI_API_KEY not set; executing against pure in-memory mock handler.\n"
            let script =
                    [ Right (toolResp [ToolCall "call-1" "computeTax" (toJSON (TaxLookupArgs "DE" 1200.00))])
                    , Right (textResp "The applicable sales tax for Germany (19%) on a $1200 Laptop is $228.00.")
                    ]
            reply <- runEff (invoke (taxAgent (mockModel script)) query)
            TIO.putStrLn ("Agent Answer:\n" <> reply)
