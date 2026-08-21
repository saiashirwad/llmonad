{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Composable prompt and message algebra.
module LLMonad.Prompt
  ( Prompt (..)
  , fewShot
  ) where

import Data.String (IsString)
import Data.Text (Text)
import LLMonad.Types (ChatMessage (..))

-- | A composable prompt monoid.
newtype Prompt = Prompt { unPrompt :: Text }
  deriving newtype (Eq, Show, Semigroup, Monoid, IsString)

-- | Construct a few-shot message sequence from pairs of user inputs and assistant responses.
fewShot :: [(Text, Text)] -> Text -> [ChatMessage]
fewShot examples query =
  concatMap (\(u, a) -> [UserMsg u, AssistantMsg a []]) examples ++ [UserMsg query]
