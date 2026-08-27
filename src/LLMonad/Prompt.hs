{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Composable prompt and message algebra.
module LLMonad.Prompt (
    Prompt (..),
    fewShot,
    user,
    assistant,
    system,
    toolResult,
    ToPromptArg (..),
) where

import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import LLMonad.Types (ChatMessage (..))

-- | A composable prompt monoid.
newtype Prompt = Prompt {unPrompt :: Text}
    deriving newtype (Eq, Show, Semigroup, Monoid, IsString)

-- | Construct a few-shot message sequence from pairs of user inputs and assistant responses.
fewShot :: [(Text, Text)] -> Text -> [ChatMessage]
fewShot examples query =
    concatMap (\(u, a) -> [UserMsg u, AssistantMsg a []]) examples ++ [UserMsg query]

-- | User message smart constructor.
user :: Text -> ChatMessage
user = UserMsg

-- | Assistant message smart constructor.
assistant :: Text -> ChatMessage
assistant t = AssistantMsg t []

-- | System message smart constructor.
system :: Text -> ChatMessage
system = SystemMsg

-- | Tool result message smart constructor.
toolResult :: Text -> Text -> ChatMessage
toolResult = ToolMsg

{- | Class for types that can be interpolated into prompts.

Only 'Text' and 'String' need their own instances; every 'Show' type --
numbers, 'Bool', anything derived -- shares the one catch-all below, so
no per-type instance can drift from it.
-}
class ToPromptArg a where
    toPromptArg :: a -> Text

instance ToPromptArg Text where
    toPromptArg = id

instance ToPromptArg String where
    toPromptArg = T.pack

instance {-# OVERLAPPABLE #-} (Show a) => ToPromptArg a where
    toPromptArg = T.pack . show
