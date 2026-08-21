{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module LLMonad.Schema
  ( -- * JSON Schema Types
    JSONSchema (..),
    schemaToValue,
    schemaToOpenAISchema,

    -- * HasSchema Typeclass
    HasSchema (..),

    -- * Generic Schema Helpers
    GHasSchema (..),
    GEnumSchema (..),
  )
where

import Data.Aeson (ToJSON (..), Value (..), object, (.=))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Map.Strict (Map)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics

-- | Representation of a JSON Schema
data JSONSchema
  = SchemaString
  | SchemaInteger
  | SchemaNumber
  | SchemaBoolean
  | SchemaNull
  | SchemaArray JSONSchema
  | SchemaObject [(Text, JSONSchema, Bool)] -- (Field name, Sub-schema, Required)
  | SchemaEnum [Text]
  | SchemaOneOf [JSONSchema]
  | SchemaAny
  deriving (Show, Eq, Generic)

instance ToJSON JSONSchema where
  toJSON = schemaToValue

-- | Convert a JSONSchema into an Aeson JSON Schema Value
schemaToValue :: JSONSchema -> Value
schemaToValue = \case
  SchemaString ->
    object ["type" .= ("string" :: Text)]
  SchemaInteger ->
    object ["type" .= ("integer" :: Text)]
  SchemaNumber ->
    object ["type" .= ("number" :: Text)]
  SchemaBoolean ->
    object ["type" .= ("boolean" :: Text)]
  SchemaNull ->
    object ["type" .= ("null" :: Text)]
  SchemaArray item ->
    object
      [ "type" .= ("array" :: Text),
        "items" .= schemaToValue item
      ]
  SchemaObject fields ->
    let props = KM.fromList [(K.fromText name, schemaToValue s) | (name, s, _) <- fields]
        requiredList = [name | (name, _, isReq) <- fields, isReq]
     in object
          [ "type" .= ("object" :: Text),
            "properties" .= Object props,
            "required" .= requiredList,
            "additionalProperties" .= False
          ]
  SchemaEnum values ->
    object
      [ "type" .= ("string" :: Text),
        "enum" .= values
      ]
  SchemaOneOf schemas ->
    object ["anyOf" .= map schemaToValue schemas]
  SchemaAny ->
    object []

-- | Convert a schema to standard OpenAI structured output format
schemaToOpenAISchema :: Text -> JSONSchema -> Value
schemaToOpenAISchema name sc =
  object
    [ "type" .= ("json_schema" :: Text),
      "json_schema"
        .= object
          [ "name" .= name,
            "strict" .= True,
            "schema" .= schemaToValue sc
          ]
    ]

-- | Type class for Haskell types that have a JSON Schema representation
class HasSchema a where
  schema :: JSONSchema
  default schema :: (GHasSchema (Rep a)) => JSONSchema
  schema = gschema @(Rep a)

-- ============================================================================
-- Generic Derivation for HasSchema
-- ============================================================================

class GHasSchema f where
  gschema :: JSONSchema

class GRecordFields f where
  gRecordFields :: [(Text, JSONSchema, Bool)]

class GEnumSchema f where
  gEnumValues :: [Text]

-- Meta-information wrapper (Data type metadata)
instance (GHasSchema f) => GHasSchema (M1 D c f) where
  gschema = gschema @f

-- Meta-information wrapper (Constructor metadata)
instance (GRecordFields f) => GHasSchema (M1 C c f) where
  gschema = SchemaObject (gRecordFields @f)

-- Meta-information wrapper for empty constructor
instance GHasSchema (M1 C c U1) where
  gschema = SchemaObject []

-- Sum types: Check if all constructors are enum constants
instance (GEnumSchema (a :+: b)) => GHasSchema (a :+: b) where
  gschema = SchemaEnum (gEnumValues @(a :+: b))

-- Enum extraction
instance (GEnumSchema a, GEnumSchema b) => GEnumSchema (a :+: b) where
  gEnumValues = gEnumValues @a ++ gEnumValues @b

instance (Constructor c) => GEnumSchema (M1 C c U1) where
  gEnumValues = [T.pack (conName (undefined :: M1 C c U1 p))]

instance (Constructor c) => GEnumSchema (M1 C c (M1 S s U1)) where
  gEnumValues = [T.pack (conName (undefined :: M1 C c (M1 S s U1) p))]

-- Product types for record fields
instance (GRecordFields a, GRecordFields b) => GRecordFields (a :*: b) where
  gRecordFields = gRecordFields @a ++ gRecordFields @b

-- Selector (Record Field)
instance (Selector s, HasSchema a) => GRecordFields (M1 S s (K1 i a)) where
  gRecordFields =
    let fieldName = T.pack (selName (undefined :: M1 S s (K1 i a) p))
        s = schema @a
        isReq = case s of
          SchemaNull -> False
          _ -> not (isOptional s)
     in if T.null fieldName
          then []
          else [(fieldName, s, isReq)]

instance GRecordFields U1 where
  gRecordFields = []

-- Check if schema represents an optional/nullable type
isOptional :: JSONSchema -> Bool
isOptional SchemaNull = True
isOptional (SchemaOneOf xs) = any isOptional xs
isOptional _ = False

-- ============================================================================
-- Primitive and Base Type Instances
-- ============================================================================

instance HasSchema Text where
  schema = SchemaString

instance HasSchema String where
  schema = SchemaString

instance HasSchema Int where
  schema = SchemaInteger

instance HasSchema Int8 where
  schema = SchemaInteger

instance HasSchema Int16 where
  schema = SchemaInteger

instance HasSchema Int32 where
  schema = SchemaInteger

instance HasSchema Int64 where
  schema = SchemaInteger

instance HasSchema Integer where
  schema = SchemaInteger

instance HasSchema Word where
  schema = SchemaInteger

instance HasSchema Word8 where
  schema = SchemaInteger

instance HasSchema Word16 where
  schema = SchemaInteger

instance HasSchema Word32 where
  schema = SchemaInteger

instance HasSchema Word64 where
  schema = SchemaInteger

instance HasSchema Double where
  schema = SchemaNumber

instance HasSchema Float where
  schema = SchemaNumber

instance HasSchema Scientific where
  schema = SchemaNumber

instance HasSchema Bool where
  schema = SchemaBoolean

instance HasSchema () where
  schema = SchemaNull

instance HasSchema Value where
  schema = SchemaAny

instance (HasSchema a) => HasSchema (Maybe a) where
  schema = SchemaOneOf [schema @a, SchemaNull]

instance (HasSchema a) => HasSchema [a] where
  schema = SchemaArray (schema @a)

instance (HasSchema a) => HasSchema (Vector a) where
  schema = SchemaArray (schema @a)

instance HasSchema (Map Text a) where
  schema =
    SchemaObject [] -- Object with dynamic keys

instance (HasSchema a, HasSchema b) => HasSchema (a, b) where
  schema =
    SchemaObject
      [ ("first", schema @a, True),
        ("second", schema @b, True)
      ]

instance (HasSchema a, HasSchema b, HasSchema c) => HasSchema (a, b, c) where
  schema =
    SchemaObject
      [ ("first", schema @a, True),
        ("second", schema @b, True),
        ("third", schema @c, True)
      ]
