{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Types and combinators for LLM tool-calling (function calling).
--
-- Define tools by wrapping Haskell functions whose argument types implement
-- 'Data.Aeson.FromJSON' and 'LLMonad.Schema.ToSchema':
--
-- @
-- data LookupUser = LookupUser { userId :: Text }
--   deriving (Generic, FromJSON, ToSchema)
--
-- lookupUserTool :: Tool
-- lookupUserTool =
--   tool "lookup_user" "Find a user by id" $ \(LookupUser uid) -> do
--     -- do something
--     pure (object ["name" .= ("Alice" :: Text)])
-- @
module LLMonad.Tools
  ( Tool (..)
  , tool
  , tool'
  , toolSync
  , mkTool
  , ToolResult
  , AgentOpts (..)
  , defaultAgentOpts
  , useTools
  , useToolsWith
  ) where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON, ToJSON (..), Value (..), eitherDecode', encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Effectful
import LLMonad.Core
  ( LLM
  , chatRound
  , pushMessage
  )
import LLMonad.Error (LLMError (..))
import LLMonad.Schema (ToSchema (..))
import LLMonad.Types

-- | Result of running a tool: either a human- or JSON-formatted error string
-- that will be passed back to the model as feedback, or a JSON payload.
type ToolResult = Either Text Value

-- | A callable tool exposed to the model.
data Tool = Tool
  { toolSpec :: ToolSpec
  , toolRun :: Value -> IO ToolResult
  }

-- | Define a tool whose handler returns a 'ToJSON' result.
tool ::
  forall a r.
  (FromJSON a, ToSchema a, ToJSON r) =>
  -- | Tool name (e.g. @"fetch_url"@)
  Text ->
  -- | Description given to the model
  Text ->
  -- | Implementation
  (a -> IO r) ->
  Tool
tool name desc run =
  tool' name desc (\a -> Right . toJSON <$> run a)

-- | Define a pure synchronous tool.
toolSync ::
  forall a r.
  (FromJSON a, ToSchema a, ToJSON r) =>
  Text ->
  Text ->
  (a -> r) ->
  Tool
toolSync name desc run = tool name desc (pure . run)

-- | Alias for 'tool'.
mkTool ::
  forall a r.
  (FromJSON a, ToSchema a, ToJSON r) =>
  Text ->
  Text ->
  (a -> IO r) ->
  Tool
mkTool = tool

-- | Define a tool whose handler returns a raw 'ToolResult' (allows returning
-- explicit error text back to the model).
tool' ::
  forall a.
  (FromJSON a, ToSchema a) =>
  Text ->
  Text ->
  (a -> IO ToolResult) ->
  Tool
tool' name desc run =
  Tool
    { toolSpec =
        ToolSpec
          { toolSpecName = name
          , toolSpecDescription = desc
          , toolSpecParameters = toSchema @a
          }
    , toolRun = \val ->
        let parsed = case eitherDecode' (encode val) of
              Right parsed' -> Right parsed'
              Left err -> case val of
                Object _ -> case eitherDecode' "[]" of
                  Right p -> Right p
                  Left _ -> Left err
                Array _ -> case eitherDecode' "{}" of
                  Right p -> Right p
                  Left _ -> Left err
                Null -> case eitherDecode' "[]" of
                  Right p -> Right p
                  Left _ -> case eitherDecode' "{}" of
                    Right p -> Right p
                    Left _ -> Left err
                _ -> Left err
         in case parsed of
              Left err -> pure (Left ("invalid arguments: " <> decodeUtf8With lenientDecode (LBS.toStrict (encode err))))
              Right parsed' -> run parsed'
    }

-- | Knobs for the agent loop.
data AgentOpts = AgentOpts
  { -- | Maximum model round-trips before giving up.
    agentMaxRounds :: Int
  , -- | Sampling parameters for every round.
    agentParams :: Params
  }

defaultAgentOpts :: AgentOpts
defaultAgentOpts =
  AgentOpts
    { agentMaxRounds = 8
    , agentParams = defaultParams
    }

-- | Give the model tools and a task; let it call them until it produces a
-- final answer. Throws 'AgentRoundsExhausted' if it never settles.
useTools :: (LLM :> es, IOE :> es) => [Tool] -> Text -> Eff es Text
useTools = useToolsWith defaultAgentOpts

-- | 'useTools' with explicit options.
useToolsWith :: (LLM :> es, IOE :> es) => AgentOpts -> [Tool] -> Text -> Eff es Text
useToolsWith opts tools instruction = do
  pushMessage (UserMsg instruction)
  loop (agentMaxRounds opts)
  where
    specs = map toolSpec tools

    loop roundsLeft = do
      resp <- chatRound (agentParams opts) RfText specs ToolAuto
      case crspToolCalls resp of
        [] -> pure (crspText resp)
        calls
          | roundsLeft <= 1 -> liftIO (throwIO (AgentRoundsExhausted (agentMaxRounds opts)))
          | otherwise -> do
              mapM_ executeAndRecord calls
              loop (roundsLeft - 1)

    executeAndRecord call = do
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
