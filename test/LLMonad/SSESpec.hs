{-# LANGUAGE OverloadedStrings #-}

module LLMonad.SSESpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import LLMonad.Internal.SSE
import Test.Hspec

-- Feed chunks through a fresh parser and flush at the end.
runParser :: [ByteString] -> [Text]
runParser chunks = go newSSEParser chunks id
  where
    go p [] acc = acc (finishSSE p)
    go p (c : cs) acc =
      let (p', evts) = stepSSE p c
       in go p' cs (acc . (evts ++))

spec :: Spec
spec = do
  it "parses a complete event" $ do
    runParser ["data: hello\n\n"] `shouldBe` ["hello"]

  it "handles CRLF line endings" $ do
    runParser ["data: hello\r\n\r\n"] `shouldBe` ["hello"]

  it "reassembles events split across arbitrary chunk boundaries" $ do
    let whole = "event: message\ndata: {\"a\": 1}\n\n"
        expected = ["{\"a\": 1}"]
        splits = [runParser [BC.take i whole, BS.drop i whole] | i <- [0 .. BC.length whole]]
    splits `shouldSatisfy` all (== expected)

  it "joins multi-line data fields with newlines" $ do
    runParser ["data: line1\ndata: line2\n\n"] `shouldBe` ["line1\nline2"]

  it "ignores comments and non-data fields" $ do
    runParser [": keep-alive\nevent: delta\nid: 7\ndata: x\n\n"] `shouldBe` ["x"]

  it "buffers a trailing partial event until finish" $ do
    let (p1, e1) = stepSSE newSSEParser "data: partial"
    e1 `shouldBe` []
    finishSSE p1 `shouldBe` ["partial"]

  it "passes [DONE] through as data" $ do
    runParser ["data: [DONE]\n\n"] `shouldBe` ["[DONE]"]

  it "emits multiple events from one chunk" $ do
    runParser ["data: a\n\ndata: b\n\n"] `shouldBe` ["a", "b"]

  it "handles empty input" $ do
    runParser [] `shouldBe` []
