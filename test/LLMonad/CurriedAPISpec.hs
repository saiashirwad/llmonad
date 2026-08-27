{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LLMonad.CurriedAPISpec (spec) where

import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Text (Text)
import Effectful
import GHC.Generics (Generic)
import LLMonad
import Test.Hspec

data Person = Person
    { name :: Text
    , age :: Int
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data Sentiment = Positive | Negative | Neutral
    deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data Analysis = Analysis
    { sentiment :: Sentiment
    , score :: Double
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

runMock :: [Either LLMError CompletionResponse] -> Eff '[LLM, IOE] a -> IO (a, [CompletionRequest])
runMock script act = runEff (runLLMMock script act)

spec :: Spec
spec = do
    describe "Curried Functional API (R2 / F3.1, F3.2, F3.3)" $ do
        describe "0-argument ask" $ do
            it "evaluates 0-argument ask directly returning a primitive value" $ do
                let script = [Right (structuredResp (toJSON (42 :: Int)))]
                    action :: Eff '[LLM, IOE] Int
                    action = ask "What is the answer to everything?"
                (val, reqs) <- runMock script action
                val `shouldBe` 42
                case reqs of
                    (req : _) -> crMessages req `shouldBe` [UserMsg "What is the answer to everything?"]
                    [] -> expectationFailure "Expected at least one request"

            it "evaluates 0-argument ask returning a record" $ do
                let script = [Right (structuredResp (toJSON (Person "Alice" 30)))]
                    action :: Eff '[LLM, IOE] Person
                    action = ask "Extract Alice's details"
                (person, _) <- runMock script action
                person `shouldBe` Person "Alice" 30

        describe "1-argument curried ask" $ do
            it "evaluates a single-argument curried function" $ do
                let summarize :: Text -> Eff '[LLM, IOE] Text
                    summarize = ask "Summarize this text in one sentence"
                    script = [Right (structuredResp (toJSON ("Short summary" :: Text)))]
                (result, reqs) <- runMock script (summarize "Long input article about Haskell")
                result `shouldBe` "Short summary"
                case reqs of
                    (req : _) -> crMessages req `shouldBe` [UserMsg "Summarize this text in one sentence:\nLong input article about Haskell"]
                    [] -> expectationFailure "Expected at least one request"

            it "evaluates a 1-argument function returning a record" $ do
                let extractPerson :: Text -> Eff '[LLM, IOE] Person
                    extractPerson = ask "Extract person information"
                    script = [Right (structuredResp (toJSON (Person "Bob" 25)))]
                (person, _) <- runMock script (extractPerson "Bob is 25 years old")
                person `shouldBe` Person "Bob" 25

            it "evaluates a 1-argument function returning an enum" $ do
                let classifySentiment :: Text -> Eff '[LLM, IOE] Sentiment
                    classifySentiment = ask "Classify sentiment"
                    script = [Right (structuredResp (toJSON Positive))]
                (s, _) <- runMock script (classifySentiment "I love this library!")
                s `shouldBe` Positive

        describe "2-argument curried ask'" $ do
            it "evaluates a 2-argument curried function returning a Bool" $ do
                let compareDates :: Text -> Text -> Eff '[LLM, IOE] Bool
                    compareDates = ask' "Is the first date before the second date?"
                    script = [Right (structuredResp (toJSON True))]
                (isBefore, reqs) <- runMock script (compareDates "2020-01-01" "2025-01-01")
                isBefore `shouldBe` True
                case reqs of
                    (req : _) -> crMessages req `shouldBe` [UserMsg "Is the first date before the second date?:\n2020-01-01 2025-01-01"]
                    [] -> expectationFailure "Expected at least one request"

            it "evaluates a 2-argument function returning a tuple" $ do
                let pairSummary :: Text -> Text -> Eff '[LLM, IOE] (Int, Text)
                    pairSummary = ask' "Extract pair"
                    script = [Right (structuredResp (toJSON (10 :: Int, "ten" :: Text)))]
                (res, _) <- runMock script (pairSummary "arg1" "arg2")
                res `shouldBe` (10, "ten")

        describe "3-argument curried ask" $ do
            it "evaluates a 3-argument function correctly formatting the prompt" $ do
                let combineThree :: Text -> Text -> Text -> Eff '[LLM, IOE] Text
                    combineThree = ask "Combine three terms"
                    script = [Right (structuredResp (toJSON ("Combined output" :: Text)))]
                (res, reqs) <- runMock script (combineThree "foo" "bar" "baz")
                res `shouldBe` "Combined output"
                case reqs of
                    (req : _) -> crMessages req `shouldBe` [UserMsg "Combine three terms:\nfoo bar baz"]
                    [] -> expectationFailure "Expected at least one request"

        describe "Monadic chaining and Applicative composition" $ do
            it "supports monadic do-notation sequencing across multiple ask calls" $ do
                let script =
                        [ Right (structuredResp (toJSON (Person "Charlie" 40)))
                        , Right (structuredResp (toJSON (Analysis Positive 0.95)))
                        ]
                    action :: Eff '[LLM, IOE] (Person, Analysis)
                    action = do
                        p <- ask @Person "Extract person" ("Charlie is 40" :: Text)
                        a <- ask @Analysis "Analyze sentiment" ("Charlie is very happy" :: Text)
                        pure (p, a)
                ((p, a), reqs) <- runMock script action
                p `shouldBe` Person "Charlie" 40
                a `shouldBe` Analysis Positive 0.95
                length reqs `shouldBe` 2

            it "supports Applicative (<*>) composition for parallel style evaluation" $ do
                let script =
                        [ Right (structuredResp (toJSON ("Summary text" :: Text)))
                        , Right (structuredResp (toJSON (100 :: Int)))
                        ]
                    action :: Eff '[LLM, IOE] (Text, Int)
                    action = (,) <$> ask @Text "Get summary" ("text" :: Text) <*> ask @Int "Count words" ("text" :: Text)
                ((s, c), reqs) <- runMock script action
                s `shouldBe` "Summary text"
                c `shouldBe` 100
                length reqs `shouldBe` 2
