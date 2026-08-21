{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Core dynamic effect for Large Language Model interactions.
module LLMonad.Core
  ( -- * The LLM Effect
    LLM (..)

    -- * Smart Constructors
  , chatRound
  , streamRound
  , getHistory
  , setHistory
  , pushMessage
  , clearHistory
  , getSystem
  , setSystem
  , clearSystem

    -- * Text Generation Helpers
  , generateText
  , generateTextWith
  , streamText
  , streamTextWith

    -- * Prompt Interpolation
  , embed
  , embedShow

    -- * Error Handling & Recovery
  , LLMError (..)
  , isTransient
  , prettyError
  , attempt
  , retry
  ) where

import Control.Exception (throw)
import Data.Aeson (ToJSON (..), encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Effectful
import Effectful.Dispatch.Dynamic
import qualified Effectful.Exception as E
import LLMonad.Error (LLMError (..), isTransient, prettyError)
import LLMonad.Types

-- | The core dynamic effect representing model conversations and generation.
data LLM :: Effect where
  ChatRound    :: Params -> ResponseFormat -> [ToolSpec] -> ToolChoice -> LLM m CompletionResponse
  StreamRound  :: Params -> ResponseFormat -> [ToolSpec] -> (StreamEvent -> IO ()) -> LLM m CompletionResponse
  GetHistory   :: LLM m [ChatMessage]
  SetHistory   :: [ChatMessage] -> LLM m ()
  PushMessage  :: ChatMessage -> LLM m ()
  ClearHistory :: LLM m ()
  GetSystem    :: LLM m (Maybe Text)
  SetSystem    :: Text -> LLM m ()
  ClearSystem  :: LLM m ()

type instance DispatchOf LLM = Dynamic

-- | Perform a single request/response turn with the model.
chatRound :: (LLM :> es) => Params -> ResponseFormat -> [ToolSpec] -> ToolChoice -> Eff es CompletionResponse
chatRound p fmt specs choice = send (ChatRound p fmt specs choice)

-- | Perform a streaming request/response turn with token events routed to callback.
streamRound :: (LLM :> es) => Params -> ResponseFormat -> [ToolSpec] -> (StreamEvent -> IO ()) -> Eff es CompletionResponse
streamRound p fmt specs cb = send (StreamRound p fmt specs cb)

-- | Retrieve the current accumulated conversation history.
getHistory :: (LLM :> es) => Eff es [ChatMessage]
getHistory = send GetHistory

-- | Overwrite the conversation history.
setHistory :: (LLM :> es) => [ChatMessage] -> Eff es ()
setHistory msgs = send (SetHistory msgs)

-- | Append a single message to the conversation history.
pushMessage :: (LLM :> es) => ChatMessage -> Eff es ()
pushMessage msg = send (PushMessage msg)

-- | Clear all conversation history.
clearHistory :: (LLM :> es) => Eff es ()
clearHistory = send ClearHistory

-- | Retrieve the active system prompt, if set.
getSystem :: (LLM :> es) => Eff es (Maybe Text)
getSystem = send GetSystem

-- | Set the system prompt.
setSystem :: (LLM :> es) => Text -> Eff es ()
setSystem sys = send (SetSystem sys)

-- | Clear the system prompt.
clearSystem :: (LLM :> es) => Eff es ()
clearSystem = send ClearSystem

-- | Send a user prompt and return the assistant reply text.
generateText :: (LLM :> es) => Text -> Eff es Text
generateText = generateTextWith defaultParams

-- | Send a user prompt with explicit parameters and return the assistant reply text.
generateTextWith :: (LLM :> es) => Params -> Text -> Eff es Text
generateTextWith params prompt = do
  pushMessage (UserMsg prompt)
  resp <- chatRound params RfText [] ToolAuto
  pure (crspText resp)

-- | Stream assistant reply tokens through a callback and return the full reply text.
streamText :: (LLM :> es) => (Text -> IO ()) -> Text -> Eff es Text
streamText = streamTextWith defaultParams

-- | Stream assistant reply tokens with explicit parameters.
streamTextWith :: (LLM :> es) => Params -> (Text -> IO ()) -> Text -> Eff es Text
streamTextWith params cb prompt = do
  pushMessage (UserMsg prompt)
  resp <- streamRound params RfText [] forward
  pure (crspText resp)
  where
    forward ev = case ev of
      SEText t -> cb t
      SEFinished _ -> pure ()

-- | Render a JSON serializable value into prompt text.
embed :: ToJSON a => a -> Text
embed = decodeUtf8With lenientDecode . LBS.toStrict . encode . toJSON

-- | Render a value using Show into prompt text.
embedShow :: Show a => a -> Text
embedShow = T.pack . show

-- | Attempt an action and catch any thrown 'LLMError'.
attempt :: Eff es a -> Eff es (Either LLMError a)
attempt = E.try

-- | Retry an action up to N times on transient errors.
retry :: Int -> Eff es a -> Eff es a
retry maxAttempts act = go maxAttempts
  where
    go n
      | n <= 1 = act
      | otherwise = act `E.catch` \case
          err | isTransient err -> go (n - 1)
          other -> throw other
