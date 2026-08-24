{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module LLMonad.SchemaSpec (spec) where

import Data.Aeson (
    Value (..),
    object,
    (.=),
 )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.Scientific (Scientific)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import LLMonad.Schema
import Test.Hspec

data Person = Person
    { name :: Text
    , age :: Maybe Int
    }
    deriving (Generic)

instance ToSchema Person

data Color = Red | Green | Blue
    deriving (Generic)

instance ToSchema Color

data Shape
    = Circle {radius :: Double}
    | Rect {width :: Double, height :: Double}
    deriving (Generic)

instance ToSchema Shape

newtype Age = Age Int
    deriving (Generic)

instance ToSchema Age

data Wrapper = Wrapper {inner :: Age}
    deriving (Generic)

instance ToSchema Wrapper

data NestedA = NestedA {fieldB :: NestedB} deriving (Generic)
instance ToSchema NestedA

data NestedB = NestedB {leaf :: Text} deriving (Generic)
instance ToSchema NestedB

data MixedSum
    = NullaryCase
    | PayloadCase {payloadValue :: Int}
    deriving (Generic)
instance ToSchema MixedSum

lookupV :: Text -> Value -> Maybe Value
lookupV k (Object o) = KM.lookup (Key.fromText k) o
lookupV _ _ = Nothing

spec :: Spec
spec = do
    describe "Schema Engine (Tier 1: Feature Coverage)" $ do
        it "derives strict object schemas for records" $ do
            toSchema @Person
                `shouldBe` object
                    [ "type" .= ("object" :: Text)
                    , "properties"
                        .= object
                            [ Key.fromText "name" .= object ["type" .= ("string" :: Text)]
                            , Key.fromText "age" .= object ["type" .= ["integer" :: Text, "null" :: Text]]
                            ]
                    , "required" .= ["name" :: Text, "age" :: Text]
                    , "additionalProperties" .= False
                    ]

        it "turns nullary enumerations into JSON enums" $ do
            toSchema @Color `shouldBe` object ["enum" .= ["Red" :: Text, "Green" :: Text, "Blue" :: Text]]

        it "names schemas after their Haskell types" $ do
            schemaTypeName @Person `shouldBe` "Person"
            schemaTypeName @Int `shouldBe` "Int"
            schemaTypeName @Color `shouldBe` "Color"

        it "collapses single-field newtypes to their inner schema" $ do
            toSchema @Age `shouldBe` object ["type" .= ("integer" :: Text)]

        it "wraps newtype fields inside records normally" $ do
            toSchema @Wrapper
                `shouldBe` object
                    [ "type" .= ("object" :: Text)
                    , "properties" .= object [Key.fromText "inner" .= object ["type" .= ("integer" :: Text)]]
                    , "required" .= ["inner" :: Text]
                    , "additionalProperties" .= False
                    ]

        it "renders sum types as tagged anyOf variants" $ do
            case lookupV "anyOf" (toSchema @Shape) of
                Just (Array vs) -> length vs `shouldBe` 2
                other -> expectationFailure ("expected anyOf array, got: " <> show other)

        it "tags sum variants with a discriminator" $ do
            case lookupV "anyOf" (toSchema @Shape) of
                Just (Array vs) -> do
                    let circle = V.head vs
                    (lookupV "properties" circle >>= lookupV "tag") `shouldBe` Just (object ["enum" .= ["Circle" :: Text]])
                other -> expectationFailure ("expected anyOf array, got: " <> show other)

        it "describes lists with items schema" $ do
            toSchema @[Int]
                `shouldBe` object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("integer" :: Text)]]

        it "widens Maybe into nullable type arrays" $ do
            toSchema @(Maybe Text) `shouldBe` object ["type" .= ["string" :: Text, "null" :: Text]]

        it "supports attaching descriptions to properties" $ do
            let s = describeProperties [("name", "Full name")] (toSchema @Person)
            case s of
                Object o
                    | Just props <- KM.lookup (Key.fromText "properties") o
                    , Object po <- props
                    , Just nameSchema <- KM.lookup (Key.fromText "name") po
                    , Object ns <- nameSchema ->
                        KM.lookup (Key.fromText "description") ns `shouldBe` Just (String "Full name")
                _ -> expectationFailure "expected object schema"

        it "supports top-level descriptions" $ do
            lookupV "description" (withDescription "A person" (toSchema @Person))
                `shouldBe` Just (String "A person")

    describe "Schema Engine (Tier 2: Boundary & Corner Cases)" $ do
        it "derives null schema for unit type ()" $ do
            toSchema @() `shouldBe` object ["type" .= ("null" :: Text)]

        it "derives open schema for raw Value" $ do
            toSchema @Value `shouldBe` object []

        it "derives positional tuple schemas" $ do
            toSchema @(Int, Text, Bool)
                `shouldBe` object
                    [ "type" .= ("array" :: Text)
                    , "items" .= Array (V.fromList [object ["type" .= ("integer" :: Text)], object ["type" .= ("string" :: Text)], object ["type" .= ("boolean" :: Text)]])
                    ]

        it "derives 4-element tuple schema" $ do
            toSchema @(Int, Double, Text, Scientific)
                `shouldBe` object
                    [ "type" .= ("array" :: Text)
                    , "items" .= Array (V.fromList [object ["type" .= ("integer" :: Text)], object ["type" .= ("number" :: Text)], object ["type" .= ("string" :: Text)], object ["type" .= ("number" :: Text)]])
                    ]

        it "derives minItems: 1 for NonEmpty lists" $ do
            toSchema @(NonEmpty Text)
                `shouldBe` object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("string" :: Text)], "minItems" .= (1 :: Int)]

        it "derives uniqueItems: true for Set containers" $ do
            toSchema @(Set.Set Text)
                `shouldBe` object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("string" :: Text)], "uniqueItems" .= True]

        it "derives dictionary object schema for Map Text v" $ do
            toSchema @(Map.Map Text Int)
                `shouldBe` object ["type" .= ("object" :: Text), "additionalProperties" .= object ["type" .= ("integer" :: Text)]]

        it "enforces additionalProperties: false on deeply nested records" $ do
            let schemaA = toSchema @NestedA
            lookupV "additionalProperties" schemaA `shouldBe` Just (Bool False)
            let schemaB = toSchema @NestedB
            lookupV "additionalProperties" schemaB `shouldBe` Just (Bool False)

        it "correctly formats mixed nullary and payload sum variants" $ do
            case lookupV "anyOf" (toSchema @MixedSum) of
                Just (Array vs) -> do
                    length vs `shouldBe` 2
                    let v1 = vs V.! 0
                        v2 = vs V.! 1
                    (lookupV "properties" v1 >>= lookupV "tag") `shouldBe` Just (object ["enum" .= ["NullaryCase" :: Text]])
                    (lookupV "properties" v2 >>= lookupV "tag") `shouldBe` Just (object ["enum" .= ["PayloadCase" :: Text]])
                other -> expectationFailure ("Expected anyOf array for MixedSum, got: " <> show other)
