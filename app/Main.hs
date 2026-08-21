{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | A guided tour of everything LLMonad can do. Point it at any provider:
--
-- > export OPENAI_API_KEY=sk-...   ./llmonad-demo        # or
-- > export GROQ_API_KEY=gsk_...    ./llmonad-demo        # or
-- > export ANTHROPIC_API_KEY=...   ./llmonad-demo        # or
-- > ollama serve && ./llmonad-demo                       # local, no key
--
-- @LLMONAD_MODEL@ overrides the default model for the chosen provider.
module Main where

import Control.Exception (try)
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Effectful
import GHC.Generics (Generic)
import LLMonad
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)

--------------------------------------------------------------------------------
-- Pick a provider from the environment
--------------------------------------------------------------------------------

pickConfig :: IO (Maybe LLMConfig)
pickConfig = do
  model <- fmap T.pack <$> lookupEnv "LLMONAD_MODEL"
  let withDefault def = Model (fromMaybe def model)
  tryKeys
    [ ("ANTHROPIC_API_KEY", \k -> defaultConfig (anthropicProvider (T.pack k)) (withDefault "claude-sonnet-4-5"))
    , ("OPENAI_API_KEY", \k -> defaultConfig (openAIProvider (T.pack k)) (withDefault "gpt-4o-mini"))
    , ("GROQ_API_KEY", \k -> defaultConfig (groqProvider (T.pack k)) (withDefault "llama-3.3-70b-versatile"))
    , ("OPENROUTER_API_KEY", \k -> defaultConfig (openRouterProvider (T.pack k)) (withDefault "openai/gpt-4o-mini"))
    , ("DEEPSEEK_API_KEY", \k -> defaultConfig (deepSeekProvider (T.pack k)) (withDefault "deepseek-chat"))
    ]
    >>= \case
      Just cfg -> pure (Just cfg)
      Nothing -> do
        host <- lookupEnv "OLLAMA_HOST"
        pure $ case host of
          Just h -> Just (defaultConfig (ollamaProvider (T.pack h)) (withDefault "llama3.2"))
          Nothing -> Nothing
  where
    tryKeys [] = pure Nothing
    tryKeys ((var, mk) : rest) =
      lookupEnv var >>= \case
        Just k | not (null k) -> pure (Just (mk k))
        _ -> tryKeys rest

--------------------------------------------------------------------------------
-- Demo types
--------------------------------------------------------------------------------

data Sentiment = Positive | Negative | Mixed | Neutral
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data ReviewVerdict = ReviewVerdict
  { title :: Text
  , sentiment :: Sentiment
  , score :: Double
  , keywords :: [Text]
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data CalcArgs = CalcArgs
  { operation :: Text
  , a :: Double
  , b :: Double
  }
  deriving (Show, Generic, FromJSON, ToSchema)

calculator :: Tool
calculator =
  mkTool "calculator" "Evaluate arithmetic: operation is one of add, subtract, multiply, divide" $ \(args :: CalcArgs) ->
    pure $ case operation args of
      "add" -> a args + b args
      "subtract" -> a args - b args
      "multiply" -> a args * b args
      _ -> a args / b args -- divide (and anything else)

--------------------------------------------------------------------------------
-- Demos
--------------------------------------------------------------------------------

demoStreaming :: (LLM :> es, IOE :> es) => Eff es ()
demoStreaming = do
  setSystem "You write in one short paragraph, no preamble."
  _ <- streamText (\t -> TIO.putStr t >> hFlush stdout) "Explain monads to a tired Ruby developer."
  liftIO (putStrLn "\n")

demoTypedAsk :: (LLM :> es, IOE :> es) => Eff es ()
demoTypedAsk = do
  let review :: Text
      review = "Chainsaw Massacre 9 was, against all odds, tender. The gore is minimal, \
               \the pacing patient, and by the end I cared about the sheriff."
  verdict <- ask @ReviewVerdict ("Extract structured data from this movie review:\n" <> review)
  liftIO (print verdict)

demoMemory :: (LLM :> es, IOE :> es) => Eff es ()
demoMemory = do
  setSystem "You are terse."
  _ <- generateText "Remember this code word for later: 'quokka'. Just acknowledge."
  reply <- generateText "What was the code word I told you?"
  liftIO (TIO.putStrLn ("model remembers: " <> reply))

demoTools :: (LLM :> es, IOE :> es) => Eff es ()
demoTools = do
  answer <- useTools [calculator] "Use the calculator tool to compute 17 * 23 - 4, then tell me the result."
  liftIO (TIO.putStrLn answer)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = pickConfig >>= \case
  Nothing -> do
    putStrLn "No provider configured. Set one of:"
    putStrLn "  ANTHROPIC_API_KEY / OPENAI_API_KEY / GROQ_API_KEY / OPENROUTER_API_KEY / DEEPSEEK_API_KEY"
    putStrLn "  or OLLAMA_HOST=http://localhost:11434 for a local model."
    putStrLn "Optionally set LLMONAD_MODEL to override the default model."
    exitFailure
  Just cfg -> do
    putStrLn ("=== llmonad demo — provider: " <> T.unpack (providerName (configProvider cfg)) <> ", model: " <> show (configModel cfg) <> " ===\n")

    runDemo "1. streaming" (runEff (runLLMHTTP cfg demoStreaming))
    runDemo "2. typed ask" (runEff (runLLMHTTP cfg demoTypedAsk))
    runDemo "3. conversation memory" (runEff (runLLMHTTP cfg demoMemory))
    runDemo "4. tools" (runEff (runLLMHTTP cfg demoTools))

runDemo :: Text -> IO () -> IO ()
runDemo label act = do
  putStrLn ("--- " <> T.unpack label <> " ---")
  r <- try act
  case r of
    Right () -> putStrLn ""
    Left e -> putStrLn ("  !! " <> T.unpack (prettyError e) <> "\n")
