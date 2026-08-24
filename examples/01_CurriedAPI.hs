{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | LLMonad Example 1: Curried Functional API
Demonstrates curried ask and ask' combinators, multi-argument functions,
and Monadic / Applicative composition.
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

data Sentiment = Positive | Negative | Neutral
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data SentimentReport = SentimentReport
    { sentiment :: Sentiment
    , confidence :: Double
    , explanation :: Text
    }
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- | 1-argument curried function returning Text
summarize :: (LLM :> es) => Text -> Eff es Text
summarize = ask "Summarize this input in one concise sentence"

-- | 1-argument curried function returning structured record
analyzeSentiment :: (LLM :> es) => Text -> Eff es SentimentReport
analyzeSentiment = ask "Analyze the sentiment of this text with confidence and explanation"

-- | 2-argument curried function returning Bool
compareThemes :: (LLM :> es) => Text -> Text -> Eff es Bool
compareThemes = ask' "Do these two text snippets discuss the same primary theme?"

workflow :: (LLM :> es, IOE :> es) => Eff es ()
workflow = do
    liftIO (putStrLn "--- 1. Single-Argument Curried ask ---")
    let review = "The Haskell effectful ecosystem is fast, clean, and a joy to develop with."
    summary <- summarize review
    liftIO (TIO.putStrLn ("Summary: " <> summary))

    liftIO (putStrLn "\n--- 2. Typed Structured Extraction with Curried ask ---")
    report <- analyzeSentiment review
    liftIO (putStrLn ("Sentiment Report: " <> show report))

    liftIO (putStrLn "\n--- 3. Multi-Argument Curried ask' ---")
    let snippetA = "Functional programming emphasizes pure functions and immutable data structures."
        snippetB = "Haskell prevents unintended side effects using monads and type systems."
    sameTheme <- compareThemes snippetA snippetB
    liftIO (putStrLn ("Discuss the same theme? " <> show sameTheme))

    liftIO (putStrLn "\n--- 4. Applicative Composition (<*>) ---")
    (s, r) <- (,) <$> summarize review <*> analyzeSentiment review
    liftIO (TIO.putStrLn ("Applicative Summary: " <> s))
    liftIO (putStrLn ("Applicative Report:  " <> show r))

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
                    [ Right (structuredResp (toJSON ("A summary of the review" :: Text)))
                    , Right (structuredResp (toJSON (SentimentReport Positive 0.98 "Very enthusiastic tone")))
                    , Right (structuredResp (toJSON True))
                    , Right (structuredResp (toJSON ("A summary of the review" :: Text)))
                    , Right (structuredResp (toJSON (SentimentReport Positive 0.98 "Very enthusiastic tone")))
                    ]
            (res, _) <- runEff (runLLMMock script workflow)
            pure res
