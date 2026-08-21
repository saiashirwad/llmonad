{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Autonomous multi-step ReAct agent execution.
module LLMonad.Agent
  ( runAgent
  , runAgentWith
  , runAgentStructured
  , runAgentStructuredWith
  , AgentOpts (..)
  , defaultAgentOpts
  ) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Aeson (FromJSON, encode, object, parseJSON, (.=))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Effectful
import LLMonad.Core
  ( LLM
  , chatRound
  , pushMessage
  )
import LLMonad.Error (LLMError (..))
import LLMonad.Internal.Extract (decodeViaJSON)
import LLMonad.Schema (ToSchema (..))
import LLMonad.Tools (AgentOpts (..), Tool (..), defaultAgentOpts)
import LLMonad.Types

-- | Run autonomous ReAct agent loop.
runAgent :: (LLM :> es, IOE :> es) => [Tool] -> Text -> Eff es Text
runAgent = runAgentWith defaultAgentOpts

-- | Run autonomous ReAct agent loop with custom options.
runAgentWith :: (LLM :> es, IOE :> es) => AgentOpts -> [Tool] -> Text -> Eff es Text
runAgentWith opts tools instruction = do
  pushMessage (UserMsg instruction)
  loop (agentMaxRounds opts) []
  where
    specs = map toolSpec tools

    loop roundsLeft prevSignatures
      | roundsLeft <= 0 = liftIO (throwIO (AgentRoundsExhausted (agentMaxRounds opts)))
      | otherwise = do
          resp <- chatRound (agentParams opts) RfText specs ToolAuto
          case crspToolCalls resp of
            [] -> pure (crspText resp)
            calls
              | roundsLeft <= 1 -> liftIO (throwIO (AgentRoundsExhausted (agentMaxRounds opts)))
              | otherwise -> do
                  let currentSignatures = [(toolCallName c, toolCallArguments c) | c <- calls]
                  when (currentSignatures == prevSignatures && not (null currentSignatures)) $ do
                    pushMessage (UserMsg "Warning: Repeated identical tool call signature detected. Please adjust your plan or return the final answer.")
                  mapM_ (executeAndRecord specs tools) calls
                  loop (roundsLeft - 1) currentSignatures

-- | Run autonomous ReAct agent loop returning structured output.
runAgentStructured :: forall a es. (LLM :> es, IOE :> es, FromJSON a, ToSchema a) => [Tool] -> Text -> Eff es a
runAgentStructured = runAgentStructuredWith defaultAgentOpts

-- | Run autonomous ReAct agent loop returning structured output with custom options.
runAgentStructuredWith ::
  forall a es.
  (LLM :> es, IOE :> es, FromJSON a, ToSchema a) =>
  AgentOpts ->
  [Tool] ->
  Text ->
  Eff es a
runAgentStructuredWith opts tools instruction = do
  pushMessage (UserMsg instruction)
  loop (agentMaxRounds opts) []
  where
    specs = map toolSpec tools
    schemaFormat = RfJsonSchema (schemaTypeName @a) (toSchema @a) True

    loop roundsLeft prevSignatures
      | roundsLeft <= 0 = liftIO (throwIO (AgentRoundsExhausted (agentMaxRounds opts)))
      | otherwise = do
          resp <- chatRound (agentParams opts) schemaFormat specs ToolAuto
          case crspToolCalls resp of
            [] -> do
              let parsed = case crspStructuredPayload resp of
                    Just val -> parseEither parseJSON val
                    Nothing -> decodeViaJSON (crspText resp)
              case parsed of
                Right a -> pure a
                Left err
                  | roundsLeft <= 1 -> liftIO (throwIO (DecodeError err (crspText resp)))
                  | otherwise -> do
                      let feedback =
                            "Your final response could not be decoded ("
                              <> T.pack err
                              <> "). Please return ONLY valid JSON conforming to the schema."
                      pushMessage (UserMsg feedback)
                      loop (roundsLeft - 1) prevSignatures
            calls
              | roundsLeft <= 1 -> liftIO (throwIO (AgentRoundsExhausted (agentMaxRounds opts)))
              | otherwise -> do
                  let currentSignatures = [(toolCallName c, toolCallArguments c) | c <- calls]
                  when (currentSignatures == prevSignatures && not (null currentSignatures)) $ do
                    pushMessage (UserMsg "Warning: Repeated identical tool call signature detected. Please adjust your plan or return the final answer.")
                  mapM_ (executeAndRecord specs tools) calls
                  loop (roundsLeft - 1) currentSignatures

-- | Execute a single tool call and record the result message into conversation history.
executeAndRecord :: (IOE :> es, LLM :> es) => [ToolSpec] -> [Tool] -> ToolCall -> Eff es ()
executeAndRecord specs tools call = do
  let payload = case find ((== toolCallName call) . toolSpecName) specs of
        Nothing -> Left ("unknown tool: " <> toolCallName call)
        Just _ -> case find ((== toolCallName call) . toolSpecName . toolSpec) tools of
          Nothing -> Left ("unknown tool implementation: " <> toolCallName call)
          Just t -> Right t
  result <- case payload of
    Left errMsg -> pure (Left errMsg)
    Right t -> liftIO (toolRun t (toolCallArguments call))
  let value = case result of
        Right v -> v
        Left errMsg -> object ["error" .= errMsg]
      content = decodeUtf8With lenientDecode . LBS.toStrict . encode $ value
  pushMessage (ToolMsg (toolCallId call) content)

