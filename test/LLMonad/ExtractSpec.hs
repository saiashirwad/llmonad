{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLMonad.ExtractSpec (spec) where

import Data.Aeson (Value (..), object)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import Data.Vector qualified as V
import LLMonad.Internal.Extract
import Test.Hspec

spec :: Spec
spec = do
  it "parses bare JSON" $ do
    extractJSON "{\"a\": 1}" `shouldBe` Right (object [(Key.fromText "a", Number 1)])

  it "strips markdown fences" $ do
    extractJSON "```json\n{\"a\": 1}\n```" `shouldBe` Right (object [(Key.fromText "a", Number 1)])

  it "tolerates prose around the JSON" $ do
    extractJSON "Here is your answer:\n{\"a\": 1}\nHope that helps!" `shouldBe`
      Right (object [(Key.fromText "a", Number 1)])

  it "handles braces inside string literals" $ do
    extractJSON "{\"text\": \"a } tricky { value\"}" `shouldBe`
      Right (object [(Key.fromText "text", String "a } tricky { value")])

  it "finds top-level arrays" $ do
    extractJSON "[1, 2, 3]" `shouldBe` Right (Array (V.fromList [Number 1, Number 2, Number 3]))

  it "fails cleanly when there is no JSON" $ do
    case extractJSON "no json here at all" of
      Left _ -> pure ()
      Right v -> expectationFailure ("unexpectedly parsed: " <> show v)

  it "decodes into Haskell types leniently" $ do
    decodeViaJSON @Text "\"hello\"" `shouldBe` Right "hello"
    decodeViaJSON @Int "answer: 42." `shouldBe` Right 42

  describe "Delimiter stack tracking and mismatched bracket rejection" $ do
    it "rejects mismatched closing bracket on top-level array: [1, 2}" $ do
      case extractJSON "[1, 2}" of
        Left _ -> pure ()
        Right v -> expectationFailure ("unexpectedly parsed mismatched array: " <> show v)

    it "rejects mismatched closing bracket on top-level object: {\"a\": 1]" $ do
      case extractJSON "{\"a\": 1]" of
        Left _ -> pure ()
        Right v -> expectationFailure ("unexpectedly parsed mismatched object: " <> show v)

    it "rejects nested mismatched brackets: {\"arr\": [1, 2}}" $ do
      case extractJSON "{\"arr\": [1, 2}}" of
        Left _ -> pure ()
        Right v -> expectationFailure ("unexpectedly parsed nested mismatched brackets: " <> show v)

    it "rejects nested mismatched braces: [{\"a\": 1]" $ do
      case extractJSON "[{\"a\": 1]" of
        Left _ -> pure ()
        Right v -> expectationFailure ("unexpectedly parsed nested mismatched braces: " <> show v)

    it "correctly parses valid nested structures with mixed braces and brackets" $ do
      extractJSON "{\"data\": [1, {\"nested\": [true, false]}], \"ok\": true}" `shouldSatisfy` (\case
        Right (Object _) -> True
        _ -> False)

    it "ignores brackets and braces inside string literals" $ do
      let payload = "{\"msg\": \"[brackets and {braces} inside strings]\", \"count\": 3}"
      case extractJSON payload of
        Right (Object o) -> KM.lookup (Key.fromText "count") o `shouldBe` Just (Number 3)
        other -> expectationFailure ("expected object, got: " <> show other)

    it "handles escaped quotes and special characters within strings correctly" $ do
      let payload = "{\"escaped\": \"quote \\\" [ ] { } test\", \"val\": 42}"
      case extractJSON payload of
        Right (Object o) -> KM.lookup (Key.fromText "val") o `shouldBe` Just (Number 42)
        other -> expectationFailure ("expected object, got: " <> show other)
