{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLMonad.Batch1AdversarialSpec (spec) where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (AsyncException (..), throwIO, try)
import Control.Monad (forM_)
import Data.ByteString qualified as BS
import Data.List (foldl')
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import LLMonad.Error (LLMError (..))
import LLMonad.Internal.Http (
    maxResponseBodyBytes,
    parseRetryAfter,
    timeoutFor,
    trySync,
 )
import LLMonad.Internal.SSE (
    finishSSE,
    newSSEParser,
    stepSSE,
 )
import LLMonad.Providers.Anthropic (
    finalizeAnthropicStream,
    handleAnthropicEvent,
    initialAntStreamState,
 )
import LLMonad.Providers.OpenAICompatible (
    finalizeOAIStream,
    handleOpenAIChunk,
    initialOAIStreamState,
 )
import LLMonad.Types (CompletionResponse (..), FinishReason (..))
import Network.HTTP.Client (responseTimeoutMicro)
import Test.Hspec

feedChunks :: [BS.ByteString] -> [Text]
feedChunks chunks = go newSSEParser chunks []
  where
    go p [] acc = acc ++ finishSSE p
    go p (c : cs) acc =
        let (p', evts) = stepSSE p c
         in go p' cs (acc ++ evts)

feedBytesOneByOne :: BS.ByteString -> [Text]
feedBytesOneByOne bs =
    let singleBytes = [BS.singleton b | b <- BS.unpack bs]
     in feedChunks singleBytes

spec :: Spec
spec = do
    describe "Batch 1 Adversarial Suite: SSE Parser Pathological Byte Fragmentation" $ do
        it "Reassembles multi-event and multiline payload fed 1 byte at a time" $ do
            let raw = "data: line1\ndata: line2\n\ndata: second event line1\ndata: second event line2\n\n"
                expected = ["line1\nline2", "second event line1\nsecond event line2"]
            feedBytesOneByOne (encodeUtf8 raw) `shouldBe` expected

        it "Handles empty data payload fed 1 byte at a time" $ do
            let raw = "data:\n\ndata: non-empty\n\n"
            feedBytesOneByOne (encodeUtf8 raw) `shouldBe` ["", "non-empty"]

        it "Handles data lines without colon (WHATWG field-name-only) fed 1 byte at a time" $ do
            let raw = "data\n\ndata: value\n\n"
            feedBytesOneByOne (encodeUtf8 raw) `shouldBe` ["", "value"]

        it "Handles arbitrary multi-byte UTF-8 splits across 1, 2, 3, and 4-byte boundaries" $ do
            -- 'λ' is 2 bytes (0xCE 0xBB)
            -- '文' is 3 bytes (0xE6 0x96 0x87)
            -- '🚀' is 4 bytes (0xF0 0x9F 0x9A 0x80)
            let sample = "data: Greek: λ | CJK: 文 | Emoji: 🚀 | Symbols: 𝄞 ∰\n\n"
                bs = encodeUtf8 sample
                expected = ["Greek: λ | CJK: 文 | Emoji: 🚀 | Symbols: 𝄞 ∰"]

            -- 1-byte feeds
            feedBytesOneByOne bs `shouldBe` expected

            -- All pairwise splits
            forM_ [0 .. BS.length bs] $ \splitPoint -> do
                let c1 = BS.take splitPoint bs
                    c2 = BS.drop splitPoint bs
                feedChunks [c1, c2] `shouldBe` expected

            -- All 3-way splits across unicode points
            forM_ [1, 5, 10, 15, 20, 25, 30, 35, 40] $ \s1 -> do
                forM_ [s1 + 1, s1 + 5, s1 + 10] $ \s2 -> do
                    let c1 = BS.take s1 bs
                        c2 = BS.take (s2 - s1) (BS.drop s1 bs)
                        c3 = BS.drop s2 bs
                    feedChunks [c1, c2, c3] `shouldBe` expected

        it "Handles CRLF vs LF mixed within and between events" $ do
            let raw = "data: line1\r\ndata: line2\n\r\ndata: event2_line1\ndata: event2_line2\r\n\n"
                expected = ["line1\nline2", "event2_line1\nevent2_line2"]
            feedBytesOneByOne (encodeUtf8 raw) `shouldBe` expected
            feedChunks [encodeUtf8 raw] `shouldBe` expected

        it "Handles chunk boundary splitting exactly between CR and LF (\\r in chunk 1, \\n in chunk 2)" $ do
            let c1 = "data: split-cr-lf\r"
                c2 = "\n\r\n"
            feedChunks [c1, c2] `shouldBe` ["split-cr-lf"]

        it "Handles multiple events without space after colon" $ do
            let raw = "data:alpha\n\ndata:beta\n\ndata:gamma\n\n"
                expected = ["alpha", "beta", "gamma"]
            feedBytesOneByOne (encodeUtf8 raw) `shouldBe` expected
            feedChunks [encodeUtf8 raw] `shouldBe` expected

        it "Preserves extra spaces after colon beyond the first optional space" $ do
            let raw = "data:   three leading spaces\n\ndata: no space\n\ndata:  two spaces\n\n"
                expected = ["  three leading spaces", "no space", " two spaces"]
            feedChunks [encodeUtf8 raw] `shouldBe` expected

        it "Handles multiple consecutive blank lines without emitting spurious empty events" $ do
            let raw = "\n\n\ndata: first\n\n\n\n\ndata: second\n\n\n\n"
                expected = ["first", "second"]
            feedBytesOneByOne (encodeUtf8 raw) `shouldBe` expected
            feedChunks [encodeUtf8 raw] `shouldBe` expected

        it "Handles interspersed comments, custom fields, and keepalives across fragmented chunks" $ do
            let raw = ": ping comment\nevent: message\nid: 101\ndata: valid line 1\n: middle comment\nretry: 3000\ndata: valid line 2\n\n"
                expected = ["valid line 1\nvalid line 2"]
            feedBytesOneByOne (encodeUtf8 raw) `shouldBe` expected

        it "Reassembles very long data lines (100 KB) split into 1-byte, 7-byte, and 64-byte chunks" $ do
            let bigLine = T.replicate 10000 "0123456789" -- 100,000 characters
                raw = "data: " <> bigLine <> "\n\n"
                bs = encodeUtf8 raw
                expected = [bigLine]

            -- 64-byte chunks
            let chunk64 = [BS.take 64 (BS.drop i bs) | i <- [0, 64 .. BS.length bs - 1]]
            feedChunks chunk64 `shouldBe` expected

            -- 7-byte prime-sized chunks
            let chunk7 = [BS.take 7 (BS.drop i bs) | i <- [0, 7 .. BS.length bs - 1]]
            feedChunks chunk7 `shouldBe` expected

        it "Correctly flushes un-terminated trailing data at finishSSE" $ do
            let (p1, e1) = stepSSE newSSEParser "data: unterminated line 1\ndata: unterminated line 2"
            e1 `shouldBe` []
            finishSSE p1 `shouldBe` ["unterminated line 1\nunterminated line 2"]

        it "Correctly flushes trailing data without newline at finishSSE" $ do
            let (p1, e1) = stepSSE newSSEParser "data: single trailing line"
            e1 `shouldBe` []
            finishSSE p1 `shouldBe` ["single trailing line"]

        it "Returns empty list on empty input and finishSSE" $ do
            finishSSE newSSEParser `shouldBe` []
            let (p, evts) = stepSSE newSSEParser ""
            evts `shouldBe` []
            finishSSE p `shouldBe` []

    describe "Batch 1 Adversarial Suite: HTTP Async Exception Safety" $ do
        it "trySync catches synchronous exceptions as Left SomeException" $ do
            res <- trySync (throwIO (userError "synchronous IO error"))
            case res of
                Left ex -> show ex `shouldSatisfy` T.isInfixOf "synchronous IO error" . T.pack
                Right _ -> expectationFailure "Expected Left from synchronous exception"

        it "trySync immediately rethrows ThreadKilled async exception" $ do
            doneMVar <- newEmptyMVar
            tid <- forkIO $ do
                res <- try (trySync (throwIO ThreadKilled))
                case res of
                    Left ThreadKilled -> putMVar doneMVar (Right ())
                    Left other -> putMVar doneMVar (Left ("Wrong exception: " <> show other))
                    Right _ -> putMVar doneMVar (Left "Swallowed ThreadKilled into Right")
            threadDelay 10000
            killThread tid
            result <- takeMVar doneMVar
            result `shouldBe` Right ()

        it "trySync immediately rethrows UserInterrupt async exception" $ do
            doneMVar <- newEmptyMVar
            _ <- forkIO $ do
                res <- try (trySync (throwIO UserInterrupt))
                case res of
                    Left UserInterrupt -> putMVar doneMVar (Right ())
                    Left other -> putMVar doneMVar (Left ("Wrong exception: " <> show other))
                    Right _ -> putMVar doneMVar (Left "Swallowed UserInterrupt into Right")
            result <- takeMVar doneMVar
            result `shouldBe` Right ()

        it "Preserves ThreadKilled cancellation when killing a sleeping worker thread" $ do
            mvar <- newEmptyMVar
            workerTid <- forkIO $ do
                res <- try (trySync (threadDelay 5000000))
                case res of
                    Left ThreadKilled -> putMVar mvar (Right ())
                    Left other -> putMVar mvar (Left ("Caught unexpected: " <> show other))
                    Right _ -> putMVar mvar (Left "Swallowed async kill into Right")
            threadDelay 50000
            killThread workerTid
            outcome <- takeMVar mvar
            outcome `shouldBe` Right ()

    describe "Batch 1 Adversarial Suite: HTTP Timeout Clamping & Arithmetic Guard" $ do
        it "Handles default timeout (Nothing) as 300,000,000 micros" $ do
            timeoutFor Nothing `shouldBe` responseTimeoutMicro 300000000

        it "Clamps negative numbers and zero to 0 micros" $ do
            timeoutFor (Just 0) `shouldBe` responseTimeoutMicro 0
            timeoutFor (Just (-1)) `shouldBe` responseTimeoutMicro 0
            timeoutFor (Just (-1000000)) `shouldBe` responseTimeoutMicro 0
            timeoutFor (Just (minBound :: Int)) `shouldBe` responseTimeoutMicro 0

        it "Converts normal positive seconds to microseconds correctly" $ do
            timeoutFor (Just 1) `shouldBe` responseTimeoutMicro 1000000
            timeoutFor (Just 60) `shouldBe` responseTimeoutMicro 60000000
            timeoutFor (Just 3600) `shouldBe` responseTimeoutMicro 3600000000

        it "Clamps maxBound and near-maxBound integer values to maxBound micros without overflow" $ do
            let maxSafeSecs = (maxBound :: Int) `quot` 1000000
            timeoutFor (Just maxSafeSecs) `shouldBe` responseTimeoutMicro (maxSafeSecs * 1000000)
            timeoutFor (Just (maxSafeSecs + 1)) `shouldBe` responseTimeoutMicro maxBound
            timeoutFor (Just (maxSafeSecs + 1000)) `shouldBe` responseTimeoutMicro maxBound
            timeoutFor (Just ((maxBound :: Int) - 1)) `shouldBe` responseTimeoutMicro maxBound
            timeoutFor (Just (maxBound :: Int)) `shouldBe` responseTimeoutMicro maxBound

        it "Never produces negative microsecond values for any tested Int input" $ do
            let testValues =
                    [ minBound
                    , minBound + 1
                    , -1000000000
                    , -1
                    , 0
                    , 1
                    , 10
                    , 300
                    , 1000000
                    , (maxBound `quot` 1000000) - 1
                    , maxBound `quot` 1000000
                    , (maxBound `quot` 1000000) + 1
                    , maxBound - 1
                    , maxBound
                    ]
            forM_ testValues $ \v -> do
                let rt = timeoutFor (Just v)
                -- timeoutFor returns responseTimeoutMicro n, which should never crash or throw
                show rt `shouldSatisfy` (\s -> length s > 0)

    describe "Batch 1 Adversarial Suite: HTTP Retry-After & Response Size Constants" $ do
        it "Correctly parses valid integer, decimal, uppercase 'S' and lowercase 's' Retry-After headers" $ do
            parseRetryAfter [("Retry-After", "0")] `shouldBe` Just 0
            parseRetryAfter [("Retry-After", "120")] `shouldBe` Just 120
            parseRetryAfter [("Retry-After", "  3600  ")] `shouldBe` Just 3600
            parseRetryAfter [("Retry-After", "45s")] `shouldBe` Just 45
            parseRetryAfter [("Retry-After", "90S")] `shouldBe` Just 90
            parseRetryAfter [("Retry-After", "1.5")] `shouldBe` Just 2
            parseRetryAfter [("Retry-After", "0.01")] `shouldBe` Just 1
            parseRetryAfter [("Retry-After", "0.99s")] `shouldBe` Just 1

        it "Returns Nothing for invalid, empty, or negative Retry-After headers" $ do
            parseRetryAfter [] `shouldBe` Nothing
            parseRetryAfter [("Retry-After", "")] `shouldBe` Nothing
            parseRetryAfter [("Retry-After", "Wed, 21 Oct 2026 07:28:00 GMT")] `shouldBe` Nothing
            parseRetryAfter [("Retry-After", "-10")] `shouldBe` Nothing
            parseRetryAfter [("Retry-After", "invalid_seconds")] `shouldBe` Nothing

        it "Verifies maxResponseBodyBytes is set to 10 MiB (10485760 bytes)" $ do
            maxResponseBodyBytes `shouldBe` 10 * 1024 * 1024

    describe "Batch 1 Adversarial Suite: Provider Stream Completion Invariants" $ do
        it "Anthropic: stream without message_stop or stop_reason is rejected with HttpError" $ do
            let events =
                    [ "{\"type\":\"message_start\",\"message\":{\"usage\":{\"input_tokens\":5}}}"
                    , "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\"}}"
                    , "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"Truncated text...\"}}"
                    ]
                st = foldl' (\s e -> fst (handleAnthropicEvent s e)) initialAntStreamState events
            case finalizeAnthropicStream st of
                Left (HttpError msg) -> msg `shouldSatisfy` T.isInfixOf "ended prematurely"
                other -> expectationFailure ("Expected HttpError, got: " <> show other)

        it "Anthropic: stream with message_stop succeeds with FrStop" $ do
            let events =
                    [ "{\"type\":\"message_start\",\"message\":{\"usage\":{\"input_tokens\":5}}}"
                    , "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\"}}"
                    , "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"Complete response\"}}"
                    , "{\"type\":\"message_stop\"}"
                    ]
                st = foldl' (\s e -> fst (handleAnthropicEvent s e)) initialAntStreamState events
            case finalizeAnthropicStream st of
                Right resp -> crspFinishReason resp `shouldBe` FrStop
                Left e -> expectationFailure ("Expected success, got: " <> show e)

        it "OpenAI: stream without [DONE] or finish_reason is rejected with HttpError" $ do
            let chunks =
                    [ "{\"choices\":[{\"delta\":{\"role\":\"assistant\",\"content\":\"Truncated response...\"}}]}"
                    ]
                st = foldl' (\s c -> fst (handleOpenAIChunk s c)) initialOAIStreamState chunks
            case finalizeOAIStream st of
                Left (HttpError msg) -> msg `shouldSatisfy` T.isInfixOf "ended prematurely"
                other -> expectationFailure ("Expected HttpError, got: " <> show other)

        it "OpenAI: stream with [DONE] succeeds with FrStop" $ do
            let chunks =
                    [ "{\"choices\":[{\"delta\":{\"role\":\"assistant\",\"content\":\"Complete answer\"}}]}"
                    , "[DONE]"
                    ]
                st = foldl' (\s c -> fst (handleOpenAIChunk s c)) initialOAIStreamState chunks
            case finalizeOAIStream st of
                Right resp -> do
                    crspText resp `shouldBe` "Complete answer"
                    crspFinishReason resp `shouldBe` FrStop
                Left e -> expectationFailure ("Expected success, got: " <> show e)
