{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Derive real JSON Schema documents from ordinary Haskell types.

'ToSchema' is to structured LLM output what aeson's 'Data.Aeson.FromJSON'
is to parsing: derive both, and @'LLMonad.Core.ask' \@MyType "..."@ hands
you back a decoded @MyType@ with the schema enforced server-side where
possible.

The generated schemas are shaped for maximum provider compatibility
(OpenAI strict mode \/ Groq \/ Anthropic tool-use):

* Objects carry @\"additionalProperties\": false@ and list every property
  in @\"required\"@.

* Optionality is expressed by widening the type: @Maybe Text@ becomes
  @\"type\": [\"string\", \"null\"]@ rather than being dropped from
  @required@.

* Enumerations (constructors with no fields) become JSON @enum@s.

* Multi-constructor types become @anyOf@ over tagged variants. Each
  variant object carries a @\"tag\"@ discriminator matching aeson's
  default encoding, so a derived 'Data.Aeson.FromJSON' instance round-trips.

* Single-constructor types with one unnamed field (e.g. newtypes) collapse
  to their inner schema.
-}
module LLMonad.Schema (
    -- * The class
    ToSchema (..),
    HasSchema,
    schema,
    schemaName,

    -- * Refining derived schemas
    withDescription,
    describeProperties,

    -- * Building blocks (for hand-written instances)
    typedSchema,
    arrayOf,
    arraySchema,
    objSchema,
    enumOf,
    nullable,
) where

import Data.Aeson (Value (Array, Object, String), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty)
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable, tyConName, typeRep, typeRepTyCon)
import Data.Vector qualified as V
import GHC.Generics (
    C1,
    Constructor (..),
    D1,
    Generic (..),
    K1 (..),
    M1 (..),
    S,
    Selector (..),
    U1 (..),
    V1,
    (:*:) (..),
    (:+:) (..),
 )
import Numeric.Natural (Natural)

--------------------------------------------------------------------------------
-- The class
--------------------------------------------------------------------------------

{- | Types whose shape can be described to an LLM as JSON Schema.

Derive it alongside aeson's classes:

> data Person = Person { name :: Text, age :: Maybe Int }
>   deriving (Show, Generic)
>   deriving anyclass (FromJSON, ToJSON, ToSchema)
-}
class ToSchema a where
    -- | The JSON Schema describing values of type @a@.
    toSchema :: Value
    default toSchema :: (Generic a, GToSchema (Rep a)) => Value
    toSchema = gToSchema (from (undefined :: a))

    {- | A short name for the type; used as the schema name in providers that
    require one (OpenAI's @json_schema.name@). Defaults to the Haskell type
    name.
    -}
    schemaTypeName :: Text
    default schemaTypeName :: (Typeable a) => Text
    schemaTypeName = T.pack (tyConName (typeRepTyCon (typeRep (Proxy :: Proxy a))))

    {- | Free-form description attached to the top-level schema. Empty by
    default; override or use 'withDescription'.
    -}
    schemaDescription :: Text
    schemaDescription = ""

-- | Typeclass synonym matching project specification.
type HasSchema = ToSchema

-- | Retrieve the JSON Schema for a type with HasSchema.
schema :: forall a. (HasSchema a) => Value
schema = toSchema @a

-- | Retrieve the schema name for a type with HasSchema.
schemaName :: forall a. (HasSchema a) => Text
schemaName = schemaTypeName @a

--------------------------------------------------------------------------------
-- Refinements
--------------------------------------------------------------------------------

-- | Attach a description to a schema (top level).
withDescription :: Text -> Value -> Value
withDescription d (Object o) = Object (KM.insert (Key.fromText "description") (String d) o)
withDescription _ v = v

{- | Attach descriptions to named properties of an object schema. Great for
nudging models about field semantics without changing your types.
-}
describeProperties :: [(Text, Text)] -> Value -> Value
describeProperties descs (Object o) = case KM.lookup (Key.fromText "properties") o of
    Just (Object props) ->
        let props' = foldl' step props descs
         in Object (KM.insert (Key.fromText "properties") (Object props') o)
    _ -> Object o
  where
    step acc (k, d) = case KM.lookup (Key.fromText k) acc of
        Just (Object po) ->
            KM.insert (Key.fromText k) (Object (KM.insert (Key.fromText "description") (String d) po)) acc
        _ -> acc
describeProperties _ v = v

--------------------------------------------------------------------------------
-- Building blocks
--------------------------------------------------------------------------------

-- | @{\"type\": t}@
typedSchema :: Text -> Value
typedSchema t = object ["type" .= t]

-- | Array whose elements all match one schema.
arrayOf :: Value -> Value
arrayOf itemSchema =
    object ["type" .= ("array" :: Text), "items" .= itemSchema]

-- | Tuple-style array (draft-07 positional @items@).
arraySchema :: [Value] -> Value
arraySchema items =
    object ["type" .= ("array" :: Text), "items" .= Array (V.fromList items)]

{- | Object schema from properties: every property required,
@additionalProperties: false@.
-}
objSchema :: [(Text, Value)] -> Value
objSchema props =
    object
        [ "type" .= ("object" :: Text)
        , "properties" .= object [(Key.fromText k) .= v | (k, v) <- props]
        , "required" .= map fst props
        , "additionalProperties" .= False
        ]

-- | @{\"enum\": [...]}@
enumOf :: [Text] -> Value
enumOf xs = object ["enum" .= xs]

{- | Make a schema also accept @null@, in the most provider-compatible way:
widen a simple @type@ into a two-element type array when possible, else
wrap in @anyOf@.
-}
nullable :: Value -> Value
nullable inner = case inner of
    Object o
        | Just (String t) <- KM.lookup (Key.fromText "type") o
        , t `elem` ["string", "integer", "number", "boolean", "array", "object"] ->
            Object (KM.insert (Key.fromText "type") (Array (V.fromList [String t, String "null"])) o)
    Object o
        | Just (Array ts) <- KM.lookup (Key.fromText "type") o ->
            Object (KM.insert (Key.fromText "type") (Array (V.snoc ts (String "null"))) o)
    _ -> object ["anyOf" .= [inner, object ["type" .= ("null" :: Text)]]]

--------------------------------------------------------------------------------
-- Concrete instances
--------------------------------------------------------------------------------

instance ToSchema Bool where toSchema = typedSchema "boolean"
instance ToSchema Char where toSchema = typedSchema "string"
instance ToSchema Text where toSchema = typedSchema "string"

-- | @String@ is @[Char]@; pin it before the generic list instance.
instance {-# OVERLAPPING #-} ToSchema [Char] where toSchema = typedSchema "string"

instance ToSchema Int where toSchema = typedSchema "integer"
instance ToSchema Integer where toSchema = typedSchema "integer"
instance ToSchema Word where toSchema = typedSchema "integer"
instance ToSchema Natural where toSchema = typedSchema "integer"
instance ToSchema Double where toSchema = typedSchema "number"
instance ToSchema Float where toSchema = typedSchema "number"
instance ToSchema Scientific where toSchema = typedSchema "number"
instance ToSchema () where toSchema = typedSchema "null"

-- | Any JSON value at all.
instance ToSchema Value where toSchema = object []

instance {-# OVERLAPPABLE #-} (ToSchema a, Typeable a) => ToSchema [a] where
    toSchema = arrayOf (toSchema @a)
    schemaTypeName = "Array"

instance (ToSchema a, Typeable a) => ToSchema (Maybe a) where
    toSchema = nullable (toSchema @a)
    schemaTypeName = "Optional"

instance (ToSchema a, Typeable a) => ToSchema (NonEmpty a) where
    toSchema = object ["type" .= ("array" :: Text), "items" .= toSchema @a, "minItems" .= (1 :: Int)]
    schemaTypeName = "NonEmptyArray"

instance (ToSchema a, Ord a, Typeable a) => ToSchema (Set.Set a) where
    toSchema = object ["type" .= ("array" :: Text), "items" .= toSchema @a, "uniqueItems" .= True]
    schemaTypeName = "Set"

instance (ToSchema v, Typeable v) => ToSchema (Map.Map Text v) where
    toSchema = object ["type" .= ("object" :: Text), "additionalProperties" .= toSchema @v]
    schemaTypeName = "Map"

instance (ToSchema a, ToSchema b, Typeable a, Typeable b) => ToSchema (a, b) where
    toSchema = arraySchema [toSchema @a, toSchema @b]
    schemaTypeName = "Tuple2"

instance (ToSchema a, ToSchema b, ToSchema c, Typeable a, Typeable b, Typeable c) => ToSchema (a, b, c) where
    toSchema = arraySchema [toSchema @a, toSchema @b, toSchema @c]
    schemaTypeName = "Tuple3"

instance (ToSchema a, ToSchema b, ToSchema c, ToSchema d, Typeable a, Typeable b, Typeable c, Typeable d) => ToSchema (a, b, c, d) where
    toSchema = arraySchema [toSchema @a, toSchema @b, toSchema @c, toSchema @d]
    schemaTypeName = "Tuple4"

--------------------------------------------------------------------------------
-- Generic machinery
--------------------------------------------------------------------------------

class GToSchema f where
    gToSchema :: f p -> Value

{- | Datatype wrapper: pick between single-constructor, enum, and anyOf
renderings.
-}
instance (GSum f) => GToSchema (D1 d f) where
    gToSchema _ =
        let n = sumCount (undefined :: f p)
         in if n == 1
                then maybe (object []) id (sumSingle (undefined :: f p))
                else
                    if sumAllNullary (undefined :: f p)
                        then enumOf (sumNames (undefined :: f p))
                        else object ["anyOf" .= sumVariants (undefined :: f p)]

-- | Sum structure: aggregates constructors.
class GSum f where
    sumVariants :: f p -> [Value]
    sumNames :: f p -> [Text]
    sumAllNullary :: f p -> Bool
    sumCount :: f p -> Int
    sumSingle :: f p -> Maybe Value

instance (GSum a, GSum b) => GSum (a :+: b) where
    sumVariants _ = sumVariants (undefined :: a p) ++ sumVariants (undefined :: b p)
    sumNames _ = sumNames (undefined :: a p) ++ sumNames (undefined :: b p)
    sumAllNullary _ = sumAllNullary (undefined :: a p) && sumAllNullary (undefined :: b p)
    sumCount _ = sumCount (undefined :: a p) + sumCount (undefined :: b p)
    sumSingle _ = Nothing

instance (Constructor c, GProduct f) => GSum (C1 c f) where
    sumVariants _ =
        let name = T.pack (conName (undefined :: C1 c f p))
            isRec = conIsRecord (undefined :: C1 c f p)
         in [ctorVariant isRec name (undefined :: f p)]
    sumNames _ = [T.pack (conName (undefined :: C1 c f p))]
    sumAllNullary _ = prodIsNullary (undefined :: f p)
    sumCount _ = 1
    sumSingle _ =
        let isRec = conIsRecord (undefined :: C1 c f p)
         in Just (ctorSingle isRec (undefined :: f p))

-- | Product structure: fields of one constructor.
class GProduct f where
    prodFields :: f p -> [(Text, Value)]
    prodPositional :: f p -> [Value]
    prodIsNullary :: f p -> Bool

instance GProduct V1 where
    prodFields _ = []
    prodPositional _ = []
    prodIsNullary _ = True

instance GProduct U1 where
    prodFields _ = []
    prodPositional _ = []
    prodIsNullary _ = True

instance (GProduct a, GProduct b) => GProduct (a :*: b) where
    prodFields _ = prodFields (undefined :: a p) ++ prodFields (undefined :: b p)
    prodPositional _ = prodPositional (undefined :: a p) ++ prodPositional (undefined :: b p)
    prodIsNullary _ = False

instance (Selector s, ToSchema a) => GProduct (M1 S s (K1 i a)) where
    prodFields _ =
        let nm = selName (undefined :: M1 S s (K1 i a) p)
         in [(if null nm then fallbackFieldName else T.pack nm, toSchema @a)]
    prodPositional _ = [toSchema @a]
    prodIsNullary _ = False

fallbackFieldName :: Text
fallbackFieldName = "value"

--------------------------------------------------------------------------------
-- Variant rendering (mirrors aeson's default encoding)
--------------------------------------------------------------------------------

-- | Tagged variant, used inside @anyOf@ for multi-constructor types.
ctorVariant :: forall f p. (GProduct f) => Bool -> Text -> f p -> Value
ctorVariant isRec name fp
    | prodIsNullary fp = objSchema [("tag", enumOf [name])]
    | isRec = objSchema (("tag", enumOf [name]) : prodFields fp)
    | otherwise = case prodPositional fp of
        [inner] -> objSchema [("tag", enumOf [name]), ("contents", inner)]
        inners -> objSchema [("tag", enumOf [name]), ("contents", arraySchema inners)]

-- | Untagged rendering, used when there is exactly one constructor.
ctorSingle :: forall f p. (GProduct f) => Bool -> f p -> Value
ctorSingle isRec fp
    | prodIsNullary fp = object ["type" .= ("array" :: Text), "maxItems" .= (0 :: Int)]
    | isRec = objSchema (prodFields fp)
    | otherwise = case prodPositional fp of
        [inner] -> inner
        inners -> arraySchema inners
