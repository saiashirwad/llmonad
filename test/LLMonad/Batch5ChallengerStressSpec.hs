{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Comprehensive Empirical Stress and Corruption Suite for Batch 5:
Journal Corruption Safety, Deserialization Fail-Closed Guarantees,
and High-Throughput O(N) Event Logging.
-}
module LLMonad.Batch5ChallengerStressSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Effectful
import LLMonad.Journal
import LLMonad.Journal.File
import LLMonad.Providers.Anthropic (encodeAnthropicMessages)
import LLMonad.Types qualified as CoreTypes
import LLMonad.World.Memory (initMemoryWorld, runWorldMemory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "Batch 5 Empirical Challenger Stress Suite" $ do
    describe "1. Corrupted Journal Files & Fail-Closed Deserialization Stress Tests" $ do
        it "loadJournalText fails closed on truncated JSON lines at various cutoffs" $ do
            let truncatedLines =
                    [ "{\"type\":\"TurnStarted\""
                    , "{\"type\":\"UserMsg\",\"content\":"
                    , "{\"type\":\"UserMsg\",\"content\":\"unterminated string"
                    , "{\"type\":\"ModelTurn\",\"content\":\"ok\",\"toolCalls\":[{\"id\":\"c1\""
                    , "{\"type\":\"ToolInvoked\",\"toolName\":\"grep\",\"arguments\":{\"pattern\":"
                    , "{\"type\":\"ToolCompleted\",\"toolCallId\":\"c1\",\"result\":"
                    , "{\"type\":\"MetricsReported\",\"metrics\":{\"mmPromptTokens\":100"
                    ]
            forM_ (zip [1 :: Int ..] truncatedLines) $ \(idx, line) -> do
                case loadJournalText line of
                    Left err -> err `shouldSatisfy` ("Line 1:" `T.isInfixOf`)
                    Right _ -> expectationFailure ("Expected truncated line " ++ show idx ++ " to fail decoding: " ++ T.unpack line)

        it "loadJournalText fails closed on invalid syntax, malformed JSON, and trailing garbage" $ do
            let invalidLines =
                    [ "NOT_A_VALID_JSON_OBJECT"
                    , "{key_without_quotes: 123}"
                    , "{\"type\":\"TurnStarted\",\"turnId\":\"t1\"} trailing garbage"
                    , "{\"type\":\"UserMsg\",\"content\":\"hello\"} 123"
                    , "{\"type\":\"ToolInvoked\",\"toolName\":\"calc\",\"arguments\":{nested_bad}}"
                    ]
            forM_ invalidLines $ \line -> do
                let fullText = "{\"type\":\"TurnStarted\",\"turnId\":\"t1\"}\n" <> line <> "\n{\"type\":\"TurnFinished\",\"turnId\":\"t1\"}"
                case loadJournalText fullText of
                    Left err -> err `shouldSatisfy` ("Line 2:" `T.isInfixOf`)
                    Right _ -> expectationFailure ("Expected invalid JSON line to fail on Line 2: " ++ T.unpack line)

        it "loadJournalText fails closed on missing mandatory fields and invalid type tags" $ do
            let missingFieldLines =
                    [ ("{\"type\":\"UserMsg\"}", "UserMsg missing content")
                    , ("{\"type\":\"TurnStarted\"}", "TurnStarted missing turnId")
                    , ("{\"type\":\"TurnFinished\"}", "TurnFinished missing turnId")
                    , ("{\"type\":\"MetricsReported\"}", "MetricsReported missing metrics")
                    , ("{\"type\":\"MetricsReported\",\"metrics\":{}}", "MetricsReported empty metrics")
                    , ("{\"type\":\"UnknownCustomType\",\"foo\":\"bar\"}", "Unknown type tag")
                    , ("{\"turnId\":\"t1\"}", "Missing type tag")
                    ]
            forM_ missingFieldLines $ \(line, desc) -> do
                case loadJournalText line of
                    Left _ -> pure ()
                    Right _ -> expectationFailure ("Expected fail closed for " ++ desc ++ ": " ++ T.unpack line)

        it "loadJournalFile fails closed on corrupted files and reports line number" $ do
            withSystemTempDirectory "b5_challenger_load" $ \tmpDir -> do
                let corruptFile = tmpDir </> "corrupt.jsonl"
                TIO.writeFile corruptFile "{\"type\":\"TurnStarted\",\"turnId\":\"t1\"}\n{\"type\":\"UserMsg\",\"content\":\"valid\"}\nMALFORMED_JSON_LINE\n"
                res <- runEff (loadJournalFile corruptFile)
                case res of
                    Left err -> err `shouldSatisfy` ("Line 3:" `T.isInfixOf`)
                    Right _ -> expectationFailure "Expected loadJournalFile to fail with error on Line 3"

        it "loadJournalFileWorld fails closed on corrupted files in virtual filesystem" $ do
            let corruptContent = "{\"type\":\"TurnStarted\",\"turnId\":\"t1\"}\n{\"type\":\"UserMsg\",\"content\":12345}\n"
            let virtualFs = initMemoryWorld [("journal/session.jsonl", corruptContent)]
            (res, _) <- runEff $ runWorldMemory virtualFs (loadJournalFileWorld "journal/session.jsonl")
            case res of
                Left err -> err `shouldSatisfy` ("Line 2:" `T.isInfixOf`)
                Right _ -> expectationFailure "Expected loadJournalFileWorld to fail on Line 2 type mismatch"

        it "resumeSession fails closed with IOException on corrupted file" $ do
            withSystemTempDirectory "b5_challenger_resume_corrupt" $ \tmpDir -> do
                let p = tmpDir </> "broken.jsonl"
                TIO.writeFile p "{\"type\":\"TurnStarted\",\"turnId\":\"t1\"}\n{INCOMPLETE_JSON"
                let action = runEff (resumeSession p)
                action `shouldThrow` anyIOException

        it "resumeSessionWorld fails closed with exception on corrupted virtual file" $ do
            let virtualFs = initMemoryWorld [("corrupt.jsonl", "{\"type\":\"TurnStarted\"}\n")]
            let action = runEff $ runWorldMemory virtualFs (resumeSessionWorld "corrupt.jsonl")
            action `shouldThrow` anyException

        it "runJournalFile fails closed immediately before executing user computation when file is corrupt" $ do
            withSystemTempDirectory "b5_challenger_run_corrupt" $ \tmpDir -> do
                let p = tmpDir </> "bad_journal.jsonl"
                TIO.writeFile p "GARBAGE_DATA\n"
                let action = runEff $ runJournalFile p $ do
                        recordUserMsg "This must not be executed or written"
                        pure (99 :: Int)
                action `shouldThrow` anyIOException
                -- Verify the corrupt file was NOT overwritten
                content <- TIO.readFile p
                content `shouldBe` "GARBAGE_DATA\n"

        it "runJournalFileWorld fails closed before executing user computation when virtual file is corrupt" $ do
            let virtualFs = initMemoryWorld [("sess.jsonl", "CORRUPTED_WORLD_DATA\n")]
            let action = runEff $ runWorldMemory virtualFs $ do
                    runJournalFileWorld "sess.jsonl" $ do
                        recordUserMsg "Should not run"
                        pure (100 :: Int)
            action `shouldThrow` anyException

        it "correctly handles empty and whitespace-only files as valid empty sessions" $ do
            withSystemTempDirectory "b5_challenger_empty" $ \tmpDir -> do
                let emptyFile = tmpDir </> "empty.jsonl"
                let wsFile = tmpDir </> "whitespace.jsonl"
                TIO.writeFile emptyFile ""
                TIO.writeFile wsFile "   \n\n\t  \n"

                resEmpty <- runEff (loadJournalFile emptyFile)
                resEmpty `shouldBe` Right []

                resWs <- runEff (loadJournalFile wsFile)
                resWs `shouldBe` Right []

                resumedEmpty <- runEff (resumeSession emptyFile)
                resumedEmpty `shouldBe` []

                resumedWs <- runEff (resumeSession wsFile)
                resumedWs `shouldBe` []

        it "correctly returns [] when resuming a non-existent file path" $ do
            resumed <- runEff (resumeSession "/path/to/completely/non_existent_journal_file.jsonl")
            resumed `shouldBe` []

            let virtualFs = initMemoryWorld []
            (resumedWorld, _) <- runEff $ runWorldMemory virtualFs (resumeSessionWorld "non_existent.jsonl")
            resumedWorld `shouldBe` []

    describe "2. High-Volume Event Logging (1,000+ Events) Throughput & O(N) Verification" $ do
        it "appends and recovers 2,000 events with linear performance and zero data corruption" $ do
            withSystemTempDirectory "b5_challenger_high_volume" $ \tmpDir -> do
                let p = tmpDir </> "high_volume_2000.jsonl"
                let totalTurns = 400 -- 400 turns * 5 events/turn = 2,000 events

                t0 <- getCurrentTime

                -- Phase 1: Run 400 turns (2,000 events) via runJournalFile
                (val, evs) <- runEff $ runJournalFileWithEvents p $ do
                    forM_ [1 .. totalTurns] $ \(i :: Int) -> do
                        let tid = "turn-" <> T.pack (show i)
                        let cid = "call-" <> T.pack (show i)
                        recordTurnStart tid
                        recordUserMsg ("Instruction " <> T.pack (show i))
                        recordToolCallWithId cid "db_query" (object ["query_id" .= i, "table" .= ("events" :: Text)])
                        recordToolResult cid (Right (object ["rows_affected" .= (1 :: Int), "status" .= ("ok" :: Text)]))
                        recordTurnFinish tid
                    pure ("high_volume_done" :: Text)

                t1 <- getCurrentTime
                let durationSec = realToFrac (diffUTCTime t1 t0) :: Double

                val `shouldBe` "high_volume_done"
                length evs `shouldBe` (totalTurns * 5)
                -- Must complete 2,000 disk-flushed events within reasonable execution time (< 15 seconds)
                durationSec `shouldSatisfy` (< 15.0)

                -- Phase 2: Resume session from disk and verify exact event count and sequence
                resumed <- runEff (resumeSession p)
                length resumed `shouldBe` (totalTurns * 5)
                resumed `shouldBe` evs

                -- Phase 3: Audit Replay verification
                case replayAudit resumed of
                    Left err -> expectationFailure ("Replay audit failed on 2,000 events: " ++ T.unpack err)
                    Right summary -> do
                        rsTotalTurns summary `shouldBe` totalTurns
                        rsUserMessages summary `shouldBe` totalTurns
                        rsToolInvocations summary `shouldBe` totalTurns
                        rsToolCompletions summary `shouldBe` totalTurns
                        rsIsValidSequence summary `shouldBe` True
                        rsValidationErrors summary `shouldBe` []

                -- Phase 4: Chat History reconstruction fidelity
                let chatHistory = reconstructChatHistory resumed
                -- Each turn produces: UserMsg, ToolMsg (from ToolCompleted) -> 2 messages * 400 turns = 800 messages
                length chatHistory `shouldBe` (totalTurns * 2)

        it "verifies linear append scaling across consecutive event batches without latency spikes" $ do
            withSystemTempDirectory "b5_challenger_scaling" $ \tmpDir -> do
                let p = tmpDir </> "scaling_bench.jsonl"
                let batchSize = 500

                -- Batch 1: Events 1..500
                t0 <- getCurrentTime
                runEff $ runJournalFile p $ do
                    forM_ [1 .. batchSize] $ \(i :: Int) -> do
                        recordUserMsg ("Batch 1 msg " <> T.pack (show i))
                t1 <- getCurrentTime
                let timeBatch1 = realToFrac (diffUTCTime t1 t0) :: Double

                -- Batch 2: Events 501..1000 (appended to existing 500 events on disk)
                t2 <- getCurrentTime
                runEff $ runJournalFile p $ do
                    forM_ [batchSize + 1 .. batchSize * 2] $ \(i :: Int) -> do
                        recordUserMsg ("Batch 2 msg " <> T.pack (show i))
                t3 <- getCurrentTime
                let timeBatch2 = realToFrac (diffUTCTime t3 t2) :: Double

                -- Verify total events on disk
                resumed <- runEff (resumeSession p)
                length resumed `shouldBe` (batchSize * 2)

                -- In O(N) append mode, Batch 2 time should not exceed 3.5x of Batch 1 time (allowing for initial 500 lines read)
                timeBatch2 `shouldSatisfy` (< (timeBatch1 * 3.5 + 2.0))

    describe "3. Provider Tool Call ID & Sequence Replay Invariants" $ do
        it "reconstructs parallel multi-tool assistant turns and matches results by toolCallId" $ do
            let tc1 = ToolCall "tc_fetch_1" "fetch_url" (object ["url" .= ("https://api.a.com" :: Text)])
                tc2 = ToolCall "tc_fetch_2" "fetch_url" (object ["url" .= ("https://api.b.com" :: Text)])
                tc3 = ToolCall "tc_calc_3" "compute" (object ["x" .= (42 :: Int)])
                events =
                    [ TurnStarted "turn-parallel"
                    , UserMsg "Fetch data from A and B and compute 42"
                    , ModelTurn "Running 3 operations in parallel" [tc1, tc2, tc3]
                    , ToolInvoked "tc_fetch_1" "fetch_url" (object ["url" .= ("https://api.a.com" :: Text)])
                    , ToolInvoked "tc_fetch_2" "fetch_url" (object ["url" .= ("https://api.b.com" :: Text)])
                    , ToolInvoked "tc_calc_3" "compute" (object ["x" .= (42 :: Int)])
                    , -- Out of order completions
                      ToolCompleted "tc_fetch_2" (Right (object ["status" .= (200 :: Int), "data" .= ("B" :: Text)]))
                    , ToolCompleted "tc_calc_3" (Right (object ["result" .= (84 :: Int)]))
                    , ToolCompleted "tc_fetch_1" (Right (object ["status" .= (200 :: Int), "data" .= ("A" :: Text)]))
                    , ModelTurn "All 3 operations finished successfully." []
                    , TurnFinished "turn-parallel"
                    ]

            -- Audit verification
            case replayAudit events of
                Left err -> expectationFailure ("Parallel replay audit failed: " ++ T.unpack err)
                Right summary -> do
                    rsTotalTurns summary `shouldBe` 1
                    rsToolInvocations summary `shouldBe` 3
                    rsToolCompletions summary `shouldBe` 3
                    rsIsValidSequence summary `shouldBe` True

            -- Reconstruct chat history
            let history = reconstructChatHistory events
            length history `shouldBe` 6
            history
                `shouldBe` [ CoreTypes.UserMsg "Fetch data from A and B and compute 42"
                           , CoreTypes.AssistantMsg "Running 3 operations in parallel" [tc1, tc2, tc3]
                           , CoreTypes.ToolMsg "tc_fetch_2" "{\"data\":\"B\",\"status\":200}"
                           , CoreTypes.ToolMsg "tc_calc_3" "{\"result\":84}"
                           , CoreTypes.ToolMsg "tc_fetch_1" "{\"data\":\"A\",\"status\":200}"
                           , CoreTypes.AssistantMsg "All 3 operations finished successfully." []
                           ]

            -- Wire format serialization check
            let anthropicWire = encodeAnthropicMessages history
            LBS.null (encode anthropicWire) `shouldBe` False

        it "flags audit errors on uncompleted tool calls and mismatched turn IDs" $ do
            let uncompletedCallEvents =
                    [ TurnStarted "t1"
                    , ToolInvoked "dangling_id" "my_tool" (object [])
                    , TurnFinished "t1"
                    ]
            case replayAudit uncompletedCallEvents of
                Left err -> err `shouldSatisfy` ("dangling_id" `T.isInfixOf`)
                Right _ -> expectationFailure "Expected dangling tool invocation to fail replayAudit"

            let uninvokedCompletionEvents =
                    [ TurnStarted "t1"
                    , ToolCompleted "phantom_id" (Right (object []))
                    , TurnFinished "t1"
                    ]
            case replayAudit uninvokedCompletionEvents of
                Left err -> err `shouldSatisfy` ("phantom_id" `T.isInfixOf`)
                Right _ -> expectationFailure "Expected uninvoked tool completion to fail replayAudit"
