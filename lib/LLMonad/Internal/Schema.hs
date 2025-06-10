{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module LLMonad.Internal.Schema
  ( Schema (..),
    GSchema (..),
  )
where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Generics
  ( C,
    D,
    Generic (Rep, from),
    K1,
    M1,
    S,
    Selector (selName),
    U1,
    type (:*:),
    type (:+:),
  )

-- | Schema class for generating JSON schema examples
class Schema a where
  genericSchema :: Value
  default genericSchema :: (Generic a, GSchema (Rep a)) => Value
  genericSchema = gschema (from (undefined :: a))

-- | Generic schema derivation
class GSchema f where
  gschema :: f p -> Value

instance (GSchema a, GSchema b) => GSchema (a :*: b) where
  gschema _ = case (gschema (undefined :: a p), gschema (undefined :: b p)) of
    (Object o1, Object o2) -> Object (o1 <> o2)
    (v1, v2) -> Array (V.fromList [v1, v2])

instance (GSchema a, GSchema b) => GSchema (a :+: b) where
  gschema _ = gschema (undefined :: a p)

instance (GSchema a) => GSchema (M1 D c a) where
  gschema _ = gschema (undefined :: a p)

instance (GSchema a) => GSchema (M1 C c a) where
  gschema _ = gschema (undefined :: a p)

instance GSchema U1 where
  gschema _ = Object mempty

instance (Selector s, Schema a) => GSchema (M1 S s (K1 i a)) where
  gschema _ =
    let fieldName = T.pack (selName (undefined :: M1 S s (K1 i a) p))
        fieldValue = genericSchema @a
     in Object (KM.singleton (K.fromText fieldName) fieldValue)

-- Basic type instances
instance Schema Bool where
  genericSchema = Bool True

instance Schema Text where
  genericSchema = String "example"

instance Schema Int where
  genericSchema = Number 42

instance (Schema a) => Schema (Maybe a) where
  genericSchema = case genericSchema @a of
    Null -> Null
    v -> v

instance (Schema a) => Schema [a] where
  genericSchema = Array (V.singleton (genericSchema @a))
