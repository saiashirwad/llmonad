{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module LLMonad.THSpec (spec) where

import Data.Aeson (FromJSON, ToJSON, object, toJSON, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import LLMonad
import LLMonad.TH.QuasiQuoter (PromptChunk (..), parsePromptChunks)
import Test.Hspec

data PersonQuery = PersonQuery
    { personName :: Text
    , personAge :: Int
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

sampleRecordTool :: PersonQuery -> IO Text
sampleRecordTool q = pure ("Hello " <> personName q <> ", age " <> T.pack (show (personAge q)))

samplePureAdd :: (Int, Int) -> Int
samplePureAdd (x, y) = x + y

sampleIOEcho :: Text -> IO Text
sampleIOEcho t = pure ("Echo: " <> t)

-- Close the declaration group so Template Haskell can reify the preceding definitions
$(return [])

-- Splice tools using makeTool and makeToolNamed
thRecordTool :: Tool IO
thRecordTool = $(makeTool 'sampleRecordTool)

thAddTool :: Tool IO
thAddTool = $(makeTool 'samplePureAdd)

thEchoTool :: Tool IO
thEchoTool = $(makeToolNamed "custom_echo" 'sampleIOEcho)

spec :: Spec
spec = do
    describe "Template Haskell QuasiQuoter & Tool Generation (R6 / F6.1, F6.2)" $ do
        describe "Prompt QuasiQuoter [prompt| ... |]" $ do
            it "interpolates simple literal text" $ do
                let p :: Text
                    p = [prompt|Hello world!|]
                p `shouldBe` "Hello world!"

            it "interpolates a single Text variable" $ do
                let name = "Alice" :: Text
                    p = [prompt|Hello #{name}!|]
                p `shouldBe` "Hello Alice!"

            it "interpolates multiple types (Text, Int, Double, Bool)" $ do
                let username = "Bob" :: Text
                    count = 42 :: Int
                    ratio = 3.14 :: Double
                    flag = True
                    p = [prompt|User #{username} has #{count} items with ratio #{ratio} and status #{flag}.|]
                p `shouldBe` "User Bob has 42 items with ratio 3.14 and status True."

            it "interpolates adjacent variables without separator" $ do
                let a = "foo" :: Text
                    b = "bar" :: Text
                    p = [prompt|#{a}#{b}|]
                p `shouldBe` "foobar"

            it "handles escaped interpolations" $ do
                let p = [prompt|The cost is \#{50} dollars.|]
                p `shouldBe` "The cost is #{50} dollars."

            it "parses prompt chunks directly with parser unit tests" $ do
                parsePromptChunks "Hello #{name}, score #{score}!"
                    `shouldBe` Right [ChunkLit "Hello ", ChunkVar "name", ChunkLit ", score ", ChunkVar "score", ChunkLit "!"]
                parsePromptChunks "No vars" `shouldBe` Right [ChunkLit "No vars"]
                parsePromptChunks "\\#{escaped}" `shouldBe` Right [ChunkLit "#{escaped}"]
                parsePromptChunks "#{}" `shouldBe` Left "Empty variable interpolation in #{}"
                parsePromptChunks "Unclosed #{" `shouldBe` Left "Unclosed #{ in prompt template"

        describe "makeTool and makeToolNamed Splices" $ do
            it "generates Tool from record-based IO function" $ do
                let spec' = toolSpec thRecordTool
                toolSpecName spec' `shouldBe` "sampleRecordTool"
                toolSpecDescription spec' `shouldBe` "Execute sampleRecordTool"
                toolSpecParameters spec' `shouldBe` toSchema @PersonQuery

                res <- toolRun thRecordTool (object ["personName" .= ("Alice" :: Text), "personAge" .= (30 :: Int)])
                res `shouldBe` Right (toJSON ("Hello Alice, age 30" :: Text))

            it "generates Tool from pure tuple function using makeTool" $ do
                let spec' = toolSpec thAddTool
                toolSpecName spec' `shouldBe` "samplePureAdd"
                toolSpecDescription spec' `shouldBe` "Execute samplePureAdd"

                res <- toolRun thAddTool (toJSON [10 :: Int, 20 :: Int])
                res `shouldBe` Right (toJSON (30 :: Int))

            it "generates Tool with custom name using makeToolNamed" $ do
                let spec' = toolSpec thEchoTool
                toolSpecName spec' `shouldBe` "custom_echo"
                toolSpecDescription spec' `shouldBe` "Execute sampleIOEcho"

                res <- toolRun thEchoTool (toJSON ("Test message" :: Text))
                res `shouldBe` Right (toJSON ("Echo: Test message" :: Text))
