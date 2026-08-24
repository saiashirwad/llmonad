{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- | Types and combinators for LLM tool-calling (function calling).

Define tools by wrapping Haskell functions whose argument types implement
'Data.Aeson.FromJSON' and 'LLMonad.Schema.ToSchema':

@
data LookupUser = LookupUser { userId :: Text }
  deriving (Generic, FromJSON, ToSchema)

lookupUserTool :: Tool
lookupUserTool =
  tool "lookup_user" "Find a user by id" $ \(LookupUser uid) -> do
    -- do something
    pure (object ["name" .= ("Alice" :: Text)])
@
-}
module LLMonad.Tools (
    Tool (..),
    ToolIO,
    tool,
    tool',
    toolSync,
    mkTool,
    hoistTool,
    Toolset,
    tools,
    noTools,
    toolsetTools,
    duplicateToolNamesIn,
    (.:?|),
    (.:|),
    ToolResult,
) where

import Control.Exception (throw)
import Data.Aeson (FromJSON, Key, Object, ToJSON (..), Value, parseJSON)
import Data.Aeson.Types (Parser, parseEither, (.:?))
import Data.List (group, intercalate, sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import LLMonad.Error (LLMError (..))
import LLMonad.Schema (ToSchema (..))
import LLMonad.Types (ToolSpec (..))

{- | Result of running a tool: either a human- or JSON-formatted error string
that will be passed back to the model as feedback, or a JSON payload.
-}
type ToolResult = Either Text Value

--------------------------------------------------------------------------------
-- Alternative-key argument parsing
--------------------------------------------------------------------------------

infixr 9 .:?|
infixr 9 .:|

{- | '(.:?)' over several accepted spellings of an optional field: the first
key present in the object wins, and later spellings are consulted only
while no value has been found yet.

> o .:?| ["searchDirectory", "search_directory", "directory"]
-}
(.:?|) :: (FromJSON a) => Object -> [Key] -> Parser (Maybe a)
o .:?| keys = go keys
  where
    go [] = pure Nothing
    go (k : ks) = o .:? k >>= maybe (go ks) (pure . Just)

{- | '(.:)' over several accepted spellings of a required field: the first key
present in the object wins; fails when none are present.

> o .:| ["filePath", "file_path", "path"]
-}
(.:|) :: (FromJSON a) => Object -> [Key] -> Parser a
o .:| keys = o .:?| keys >>= maybe missing pure
  where
    missing = fail ("missing required argument; expected one of: " <> intercalate ", " (map show keys))

-- | A callable tool exposed to the model, parameterized by its effect monad.
data Tool m = Tool
    { toolSpec :: ToolSpec
    , toolRun :: Value -> m ToolResult
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

{- | Define a tool whose handler returns a raw 'ToolResult' (allows returning
explicit error text back to the model).
-}
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
            case parseEither parseJSON val of
                Left err -> pure (Left ("invalid arguments: " <> T.pack err))
                Right parsed' -> run parsed'
        }

-- | Hoist a natural transformation over a tool's effect monad.
hoistTool :: (forall x. m x -> n x) -> Tool m -> Tool n
hoistTool nat (Tool spec run) = Tool spec (nat . run)

-- | A composable set of tools for one configured agent.
newtype Toolset es = Toolset {toolsetTools :: [Tool (Eff es)]}

instance Semigroup (Toolset es) where
    -- Merged sets revalidate: two individually valid toolsets can collide.
    Toolset left <> Toolset right = tools (left <> right)

instance Monoid (Toolset es) where
    mempty = Toolset []

{- | Build a toolset, rejecting duplicate tool names immediately so invalid
assemblies fail before any tokens are spent.
-}
tools :: [Tool (Eff es)] -> Toolset es
tools toolList = case duplicateToolNamesIn (Toolset toolList) of
    [] -> Toolset toolList
    names -> throw (AgentConfigurationError ("duplicate tool names: " <> T.intercalate ", " names))

{- | Names that appear more than once in a toolset (empty when it is valid).
Exposed so callers like 'mount' can surface the failure eagerly instead of
whenever the tool list happens to be forced.
-}
duplicateToolNamesIn :: Toolset es -> [Text]
duplicateToolNamesIn =
    mapMaybe headOfDuplicate
        . group
        . sort
        . map (toolSpecName . toolSpec)
        . toolsetTools
  where
    headOfDuplicate (name : _ : _) = Just name
    headOfDuplicate _ = Nothing

-- | An agent with no tools.
noTools :: Toolset es
noTools = mempty
