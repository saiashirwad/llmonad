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

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import DeepSeek (deepSeekRuntime)
import Effectful (IOE, liftIO, runEff, (:>))
import GHC.Generics (Generic)
import LLMonad

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
taxAgent :: (IOE :> es) => ModelRuntime es -> Agent es Text Text
taxAgent runtime =
    mount
        runtime
        (tools [hoistTool liftIO taxTool])
        (textAgent "Answer sales-tax questions using the provided tool." id)

taxQuery :: Text
taxQuery =
    let customerName = "Alice" :: Text
        item = "Laptop" :: Text
        price = 1200.00 :: Double
     in [prompt|Customer #{customerName} purchased #{item} for $#{price}. Compute the applicable sales tax in Germany.|]

main :: IO ()
main = do
    runtime <- deepSeekRuntime "deepseek-v4-flash"
    reply <- runEff (invoke (taxAgent runtime) taxQuery)
    TIO.putStrLn ("Agent Answer:\n" <> reply)
