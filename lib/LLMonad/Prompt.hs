{-# LANGUAGE OverloadedStrings #-}

module LLMonad.Prompt
  ( -- * Message Constructors
    user,
    system,
    assistant,
    toolResult,
    toolCallsMsg,

    -- * Conversational Monadic Actions
    ask,
    ask_,
    tell,
    reply,
    reset,
    getConversation,

    -- * Prompt Templating & Few-Shot
    fewShot,
    renderTemplate,
  )
where

import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as T
import LLMonad.Core
import LLMonad.Types

-- | Create a user message
user :: Text -> Message
user = userMsg

-- | Create a system message
system :: Text -> Message
system = systemMsg

-- | Create an assistant message
assistant :: Text -> Message
assistant = assistantMsg

-- | Create a tool execution result message
toolResult :: Text -> Text -> Message
toolResult = toolMsg

-- | Create an assistant message containing tool calls
toolCallsMsg :: [ToolCall] -> Message
toolCallsMsg calls = Message AssistantRole "" Nothing Nothing (Just calls)

-- | Ask a question in the current conversation, appending prompt and response to history
ask :: Text -> LLM Text
ask query = do
  appendHistory (user query)
  hist <- getHistory
  resp <- chat hist
  let replyText = extractTextContent resp
  appendHistory (assistant replyText)
  pure replyText

-- | Ask a question and discard the response text
ask_ :: Text -> LLM ()
ask_ = void . ask

-- | Add a user message to the conversation history without invoking the model
tell :: Text -> LLM ()
tell = appendHistory . user

-- | Add an assistant response to the conversation history without invoking the model
reply :: Text -> LLM ()
reply = appendHistory . assistant

-- | Clear all conversation history in the current session
reset :: LLM ()
reset = clearHistory

-- | Get all messages in the conversation history
getConversation :: LLM [Message]
getConversation = getHistory

-- | Construct a few-shot message sequence followed by a user query
fewShot :: [(Text, Text)] -> Text -> [Message]
fewShot examples query =
  let pairs = concatMap (\(inp, out) -> [user inp, assistant out]) examples
   in pairs ++ [user query]

-- | Substitute template variables in the form of {{key}} with values
renderTemplate :: Text -> [(Text, Text)] -> Text
renderTemplate = foldl replaceVar
  where
    replaceVar acc (k, v) = T.replace ("{{" <> k <> "}}") v acc
