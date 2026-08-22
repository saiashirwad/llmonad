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

  it "reassembles multi-line data events split across arbitrary chunk boundaries" $ do
    let whole = "data: line1\ndata: line2\ndata: line3\n\n"
        expected = ["line1\nline2\nline3"]
        splits = [runParser [BC.take i whole, BS.drop i whole] | i <- [0 .. BC.length whole]]
    splits `shouldSatisfy` all (== expected)

  it "does not flush pending multiline data until double newline" $ do
    let (p1, e1) = stepSSE newSSEParser "data: line1\n"
    e1 `shouldBe` []
    let (p2, e2) = stepSSE p1 "data: line2\n"
    e2 `shouldBe` []
    let (p3, e3) = stepSSE p2 "\n"
    e3 `shouldBe` ["line1\nline2"]
    finishSSE p3 `shouldBe` []

  it "handles multi-line data with interleaved comments across chunks" $ do
    let (p1, e1) = stepSSE newSSEParser "data: start\n: ping\n"
    e1 `shouldBe` []
    let (p2, e2) = stepSSE p1 "data: middle\n: pong\n"
    e2 `shouldBe` []
    let (p3, e3) = stepSSE p2 "data: end\n\n"
    e3 `shouldBe` ["start\nmiddle\nend"]
    finishSSE p3 `shouldBe` []

  it "ignores comments and non-data fields" $ do
    runParser [": keep-alive\nevent: delta\nid: 7\ndata: x\n\n"] `shouldBe` ["x"]

  it "buffers a trailing partial event until finish" $ do
    let (p1, e1) = stepSSE newSSEParser "data: partial"
    e1 `shouldBe` []
    finishSSE p1 `shouldBe` ["partial"]

  it "buffers trailing multi-line data without terminal double newline until finish" $ do
    let (p1, e1) = stepSSE newSSEParser "data: line1\ndata: line2\n"
    e1 `shouldBe` []
    finishSSE p1 `shouldBe` ["line1\nline2"]

  it "passes [DONE] through as data" $ do
    runParser ["data: [DONE]\n\n"] `shouldBe` ["[DONE]"]

  it "emits multiple events from one chunk" $ do
    runParser ["data: a\n\ndata: b\n\n"] `shouldBe` ["a", "b"]

  it "handles empty input" $ do
    runParser [] `shouldBe` []
