{-# LANGUAGE OverloadedStrings #-}

module LLMonad.Batch5AdversarialSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Effectful
import LLMonad.Journal
import LLMonad.Journal.File
import LLMonad.Journal.Memory (runJournalMemory)
import LLMonad.Types qualified as CoreTypes
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = do
    describe "Batch 5 Adversarial & Fidelity Suite" $ do
        describe "1. Provider Tool Call ID & Turn Reconstruction Adversarial Tests" $ do
            it "reconstructs multi-turn dialogue with alternating tool calls and prose" $ do
                let call1 = ToolCall "call_p1" "calc" (object ["expr" .= ("10+20" :: Text)])
                    call2 = ToolCall "call_p2" "calc" (object ["expr" .= ("30*2" :: Text)])
                    events =
                        [ TurnStarted "turn-1"
                        , UserMsg "Calculate step 1"
                        , ModelTurn "Computing step 1" [call1]
                        , ToolInvoked "call_p1" "calc" (object ["expr" .= ("10+20" :: Text)])
                        , ToolCompleted "call_p1" (Right (object ["result" .= (30 :: Int)]))
                        , ModelTurn "Step 1 is 30. Now calculating step 2." [call2]
                        , ToolInvoked "call_p2" "calc" (object ["expr" .= ("30*2" :: Text)])
                        , ToolCompleted "call_p2" (Right (object ["result" .= (60 :: Int)]))
                        , ModelTurn "Final result is 60." []
                        , TurnFinished "turn-1"
                        ]
                let history = reconstructChatHistory events
                length history `shouldBe` 6
                history
                    `shouldBe` [ CoreTypes.UserMsg "Calculate step 1"
                               , CoreTypes.AssistantMsg "Computing step 1" [call1]
                               , CoreTypes.ToolMsg "call_p1" "{\"result\":30}"
                               , CoreTypes.AssistantMsg "Step 1 is 30. Now calculating step 2." [call2]
                               , CoreTypes.ToolMsg "call_p2" "{\"result\":60}"
                               , CoreTypes.AssistantMsg "Final result is 60." []
                               ]

            it "preserves toolCallId when multiple identical tool names run with different IDs" $ do
                let callA = ToolCall "id_aaa" "read" (object ["file" .= ("a.txt" :: Text)])
                    callB = ToolCall "id_bbb" "read" (object ["file" .= ("b.txt" :: Text)])
                    events =
                        [ TurnStarted "t1"
                        , ModelTurn "Reading both files" [callA, callB]
                        , ToolInvoked "id_aaa" "read" (object ["file" .= ("a.txt" :: Text)])
                        , ToolInvoked "id_bbb" "read" (object ["file" .= ("b.txt" :: Text)])
                        , ToolCompleted "id_bbb" (Right (object ["content" .= ("B" :: Text)]))
                        , ToolCompleted "id_aaa" (Right (object ["content" .= ("A" :: Text)]))
                        , TurnFinished "t1"
                        ]
                case replayAudit events of
                    Left err -> expectationFailure ("Expected valid replay with out-of-order completions: " ++ T.unpack err)
                    Right summary -> do
                        rsIsValidSequence summary `shouldBe` True
                        rsToolInvocations summary `shouldBe` 2
                        rsToolCompletions summary `shouldBe` 2

            it "reconstructs tool error results with Error prefix in ToolMsg" $ do
                let call = ToolCall "call_err" "fetch" (object ["url" .= ("https://invalid" :: Text)])
                    events =
                        [ UserMsg "Fetch page"
                        , ModelTurn "Fetching..." [call]
                        , ToolInvoked "call_err" "fetch" (object ["url" .= ("https://invalid" :: Text)])
                        , ToolCompleted "call_err" (Left "Connection refused (404)")
                        , ModelTurn "Page could not be fetched." []
                        ]
                    history = reconstructChatHistory events
                history
                    `shouldBe` [ CoreTypes.UserMsg "Fetch page"
                               , CoreTypes.AssistantMsg "Fetching..." [call]
                               , CoreTypes.ToolMsg "call_err" "Error: Connection refused (404)"
                               , CoreTypes.AssistantMsg "Page could not be fetched." []
                               ]

        describe "2. Fail-Closed Deserialization Adversarial Boundary Cases" $ do
            it "rejects corrupted JSON line in middle of long journal file" $ do
                withSystemTempDirectory "journal_middle_corrupt" $ \tmpDir -> do
                    let p = tmpDir ++ "/corrupted_middle.jsonl"
                    let linesContent =
                            [ "{\"type\":\"TurnStarted\",\"turnId\":\"t1\"}"
                            , "{\"type\":\"UserMsg\",\"content\":\"hello\"}"
                            , "THIS IS CORRUPTED DATA ON LINE 3"
                            , "{\"type\":\"TurnFinished\",\"turnId\":\"t1\"}"
                            ]
                    TIO.writeFile p (T.unlines linesContent)
                    res <- runEff (loadJournalFile p)
                    case res of
                        Left err -> err `shouldSatisfy` ("Line 3:" `T.isInfixOf`)
                        Right _ -> expectationFailure "Expected loadJournalFile to fail on line 3"

            it "rejects unknown event type in journal line" $ do
                let unknownTypeLine = "{\"type\":\"HackedEventType\",\"payload\":123}"
                case loadJournalText unknownTypeLine of
                    Left err -> err `shouldSatisfy` ("unknown event type" `T.isInfixOf`)
                    Right _ -> expectationFailure "Expected loadJournalText to fail on unknown event type"

            it "rejects truncated JSON line" $ do
                let truncatedLine = "{\"type\":\"TurnStarted\",\"turnId\":\"t1\""
                case loadJournalText truncatedLine of
                    Left err -> err `shouldSatisfy` ("Line 1:" `T.isInfixOf`)
                    Right _ -> expectationFailure "Expected loadJournalText to fail on truncated JSON"

            it "runJournalFile fails closed immediately before executing user action on corrupted file" $ do
                withSystemTempDirectory "journal_fail_closed_test" $ \tmpDir -> do
                    let p = tmpDir ++ "/broken.jsonl"
                    TIO.writeFile p "{\"type\":\"UserMsg\",\"content\":CORRUPT}\n"
                    runEff (runJournalFile p (pure (42 :: Int))) `shouldThrow` anyIOException

        describe "3. High-Volume & Stress Replay Auditing" $ do
            it "handles 1,000 events with rapid multi-turn tool execution" $ do
                let numTurns = 200
                let program = do
                        forM_ [1 .. numTurns] $ \(i :: Int) -> do
                            let tid = "turn-" <> T.pack (show i)
                            let cid = "call-" <> T.pack (show i)
                            recordTurnStart tid
                            recordUserMsg ("Instruction " <> T.pack (show i))
                            recordToolCallWithId cid "worker" (object ["id" .= i])
                            recordToolResult cid (Right (object ["status" .= ("done" :: Text)]))
                            recordTurnFinish tid
                        getEvents

                (events, _) <- runEff (runJournalMemory program)
                length events `shouldBe` (numTurns * 5)
                case replayAudit events of
                    Left err -> expectationFailure ("Replay audit failed on 1000 events: " ++ T.unpack err)
                    Right summary -> do
                        rsTotalTurns summary `shouldBe` numTurns
                        rsUserMessages summary `shouldBe` numTurns
                        rsToolInvocations summary `shouldBe` numTurns
                        rsToolCompletions summary `shouldBe` numTurns
                        rsIsValidSequence summary `shouldBe` True

            it "handles multi-line JSON values and special characters in tool arguments and results" $ do
                let complexArgs =
                        object
                            [ "code" .= ("\nfunction test() {\n  return \"hello\\nworld\";\n}\n" :: Text)
                            , "unicode" .= ("🚀 🌟 日本語 \t\r\n" :: Text)
                            ]
                let complexRes = Right (object ["output" .= ("Success: \n  line 1\n  line 2\n" :: Text)])
                let evInvoked = ToolInvoked "c_complex" "run_code" complexArgs
                let evCompleted = ToolCompleted "c_complex" complexRes

                withSystemTempDirectory "journal_complex_json" $ \tmpDir -> do
                    let p = tmpDir ++ "/complex.jsonl"
                    runEff $ runJournalFile p $ do
                        recordEvent evInvoked
                        recordEvent evCompleted

                    resumed <- runEff (resumeSession p)
                    resumed `shouldBe` [evInvoked, evCompleted]
