{-# LANGUAGE OverloadedStrings #-}

module LLMonad.PromptSpec (spec) where

import Data.Text (Text)
import LLMonad
import Test.Hspec

spec :: Spec
spec = do
    describe "Prompt & Message Algebra (R5 / F5.1, F5.2)" $ do
        describe "Prompt Monoid & IsString" $ do
            it "satisfies identity law with mempty" $ do
                let p = Prompt "Hello"
                p <> mempty `shouldBe` p
                mempty <> p `shouldBe` p
                unPrompt mempty `shouldBe` ""

            it "satisfies associativity law with (<>)" $ do
                let p1 = Prompt "A "
                    p2 = Prompt "B "
                    p3 = Prompt "C"
                (p1 <> p2) <> p3 `shouldBe` p1 <> (p2 <> p3)
                unPrompt (p1 <> p2 <> p3) `shouldBe` "A B C"

            it "concatenates list of prompts with mconcat" $ do
                let ps = [Prompt "One", Prompt "Two", Prompt "Three"]
                unPrompt (mconcat ps) `shouldBe` "OneTwoThree"

            it "supports IsString literal overloading" $ do
                let p :: Prompt
                    p = "Literal prompt"
                p `shouldBe` Prompt "Literal prompt"

        describe "Message Smart Constructors" $ do
            it "creates user message" $ do
                user "Hello" `shouldBe` UserMsg "Hello"

            it "creates assistant message" $ do
                assistant "Hi there" `shouldBe` AssistantMsg "Hi there" []

            it "creates system message" $ do
                system "System prompt" `shouldBe` SystemMsg "System prompt"

            it "creates tool result message" $ do
                toolResult "call-123" "{\"success\":true}" `shouldBe` ToolMsg "call-123" "{\"success\":true}"

        describe "fewShot combinator" $ do
            it "constructs alternating user/assistant messages ending in query" $ do
                let examples = [("What is 1+1?", "2"), ("What is 2+2?", "4")]
                    query = "What is 3+3?"
                    msgs = fewShot examples query
                msgs
                    `shouldBe` [ UserMsg "What is 1+1?"
                               , AssistantMsg "2" []
                               , UserMsg "What is 2+2?"
                               , AssistantMsg "4" []
                               , UserMsg "What is 3+3?"
                               ]

            it "handles empty examples gracefully" $ do
                let msgs = fewShot [] "Solo query"
                msgs `shouldBe` [UserMsg "Solo query"]

        describe "embed and embedShow helpers" $ do
            it "embeds JSON serializable value into prompt" $ do
                let name :: Text
                    name = "Haskell"
                    p = "Welcome to " <> embed name <> "!"
                p `shouldBe` "Welcome to \"Haskell\"!"

            it "embedShow formats Show instances into prompt Text" $ do
                let count :: Int
                    count = 42
                    p = "Count is " <> embedShow count
                p `shouldBe` "Count is 42"
