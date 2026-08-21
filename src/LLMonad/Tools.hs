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
  , ToolIO
  , tool
  , tool'
  , toolSync
  , mkTool
  , liftTool
  , hoistTool
  , (.:?|)
  , (.:|)
  , ToolResult
  , AgentOpts (..)
  , defaultAgentOpts
  , useTools
  , useToolsWith
  ) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Aeson (FromJSON, Key, Object, ToJSON (..), Value (..), eitherDecode', encode, object, (.=))
import Data.Aeson.Types (Parser, (.:?))
import qualified Data.ByteString.Lazy as LBS
import Data.List (find, intercalate)
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

--------------------------------------------------------------------------------
-- Alternative-key argument parsing
--------------------------------------------------------------------------------

infixr 9 .:?|
infixr 9 .:|

-- | '(.:?)' over several accepted spellings of an optional field: the first
-- key present in the object wins, and later spellings are consulted only
-- while no value has been found yet.
--
-- > o .:?| ["searchDirectory", "search_directory", "directory"]
(.:?|) :: FromJSON a => Object -> [Key] -> Parser (Maybe a)
o .:?| keys = go keys
  where
    go []       = pure Nothing
    go (k : ks) = o .:? k >>= maybe (go ks) (pure . Just)

-- | '(.:)' over several accepted spellings of a required field: the first key
-- present in the object wins; fails when none are present.
--
-- > o .:| ["filePath", "file_path", "path"]
(.:|) :: FromJSON a => Object -> [Key] -> Parser a
o .:| keys = o .:?| keys >>= maybe missing pure
  where
    missing = fail ("missing required argument; expected one of: " <> intercalate ", " (map show keys))

-- | A callable tool exposed to the model, parameterized by its effect monad.
data Tool m = Tool
  { toolSpec :: ToolSpec
  , toolRun  :: Value -> m ToolResult
  }

-- | Type alias for IO-bound tools.
type ToolIO = Tool IO

-- | Define a tool whose handler returns a 'ToJSON' result.
tool ::
  forall a r m.
  (Monad m, FromJSON a, ToSchema a, ToJSON r) =>
  -- | Tool name (e.g. @"fetch_url"@)
  Text ->
  -- | Description given to the model
  Text ->
  -- | Implementation
  (a -> m r) ->
  Tool m
tool name desc run =
  tool' name desc (\a -> Right . toJSON <$> run a)

-- | Define a pure synchronous tool.
toolSync ::
  forall a r m.
  (Monad m, FromJSON a, ToSchema a, ToJSON r) =>
  Text ->
  Text ->
  (a -> r) ->
  Tool m
toolSync name desc run = tool name desc (pure . run)

-- | Alias for 'tool'.
mkTool ::
  forall a r m.
  (Monad m, FromJSON a, ToSchema a, ToJSON r) =>
  Text ->
  Text ->
  (a -> m r) ->
  Tool m
mkTool = tool

-- | Define a tool whose handler returns a raw 'ToolResult' (allows returning
-- explicit error text back to the model).
tool' ::
  forall a m.
  (Monad m, FromJSON a, ToSchema a) =>
  Text ->
  Text ->
  (a -> m ToolResult) ->
  Tool m
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

-- | Lift an IO-based tool into an Effectful environment.
liftTool :: (IOE :> es) => Tool IO -> Tool (Eff es)
liftTool (Tool spec run) = Tool spec (\val -> liftIO (run val))

-- | Hoist a natural transformation over a tool's effect monad.
hoistTool :: (forall x. m x -> n x) -> Tool m -> Tool n
hoistTool nat (Tool spec run) = Tool spec (nat . run)

-- | Knobs for the agent loop.
data AgentOpts = AgentOpts
  { -- | Maximum model round-trips before giving up.
    agentMaxRounds :: Int
  , -- | Sampling parameters for every round.
    agentParams    :: Params
  }

defaultAgentOpts :: AgentOpts
defaultAgentOpts =
  AgentOpts
    { agentMaxRounds = 8
    , agentParams = defaultParams
    }

-- | Give the model tools and a task; let it call them until it produces a
-- final answer. Throws 'AgentRoundsExhausted' if it never settles.
useTools :: (LLM :> es, IOE :> es) => [Tool (Eff es)] -> Text -> Eff es Text
useTools = useToolsWith defaultAgentOpts

-- | 'useTools' with explicit options.
useToolsWith :: (LLM :> es, IOE :> es) => AgentOpts -> [Tool (Eff es)] -> Text -> Eff es Text
useToolsWith opts tools instruction = do
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
                  mapM_ executeAndRecord calls
                  loop (roundsLeft - 1) currentSignatures

    executeAndRecord call = do
      let payload = case find ((== toolCallName call) . toolSpecName) specs of
            Nothing -> Left ("unknown tool: " <> toolCallName call)
            Just _ -> case find ((== toolCallName call) . toolSpecName . toolSpec) tools of
              Nothing -> Left ("unknown tool implementation: " <> toolCallName call)
              Just t -> Right t
      result <- case payload of
        Left errMsg -> pure (Left errMsg)
        Right t -> toolRun t (toolCallArguments call)
      let value = case result of
            Right v -> v
            Left errMsg -> object ["error" .= errMsg]
          content = decodeUtf8With lenientDecode . LBS.toStrict . encode $ value
      pushMessage (ToolMsg (toolCallId call) content)
