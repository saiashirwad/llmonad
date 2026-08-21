{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLMonad.ExtractSpec (spec) where

import Data.Aeson (Value (..), object)
import qualified Data.Aeson.Key as Key
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
