{-# LANGUAGE OverloadedStrings #-}

module LLMonad.JournalSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (eitherDecode, encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Effectful
import LLMonad.Journal
import LLMonad.Journal.File
import LLMonad.Journal.Memory
import qualified LLMonad.Types as CoreTypes
import LLMonad.World.Memory (initMemoryWorld, runWorldMemory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = do
  describe "LLMonad.Journal" $ do

    describe "JSON Serialization & Deserialization" $ do
      it "roundtrips TurnStarted event" $ do
        let ev = TurnStarted "turn-001"
        eitherDecode (encode ev) `shouldBe` Right ev

      it "roundtrips UserMsg event" $ do
        let ev = UserMsg "Hello, please list the directory files."
        eitherDecode (encode ev) `shouldBe` Right ev

      it "roundtrips ModelTurn event" $ do
        let ev = ModelTurn "I will check the files for you."
        eitherDecode (encode ev) `shouldBe` Right ev

      it "roundtrips ToolInvoked event with JSON arguments" $ do
        let args = object ["path" .= ("src/Main.hs" :: Text), "lines" .= ([1, 50] :: [Int])]
            ev = ToolInvoked "view_file" args
        eitherDecode (encode ev) `shouldBe` Right ev

      it "roundtrips ToolCompleted event with success payload" $ do
        let res = Right (object ["content" .= ("module Main where" :: Text)])
            ev = ToolCompleted "view_file" res
        eitherDecode (encode ev) `shouldBe` Right ev

      it "roundtrips ToolCompleted event with error string" $ do
        let res = Left "File not found: missing.txt"
            ev = ToolCompleted "view_file" res
        eitherDecode (encode ev) `shouldBe` Right ev

      it "roundtrips MetricsReported event" $ do
        let metrics = ModelMetrics
              { mmPromptTokens     = 512
              , mmCompletionTokens = 128
              , mmTotalTokens      = 640
              , mmLatencyMs        = 342.5
              , mmModel            = "gpt-4o"
              }
            ev = MetricsReported metrics
        eitherDecode (encode ev) `shouldBe` Right ev

      it "roundtrips TurnFinished event" $ do
        let ev = TurnFinished "turn-001"
        eitherDecode (encode ev) `shouldBe` Right ev

      it "roundtrips ModelMetrics data type" $ do
        let mm = ModelMetrics 100 50 150 85.4 "claude-3-5-sonnet"
        eitherDecode (encode mm) `shouldBe` Right mm
        promptTokens mm `shouldBe` 100
        completionTokens mm `shouldBe` 50
        totalTokens mm `shouldBe` 150
        latencyMs mm `shouldBe` 85.4
        metricModel mm `shouldBe` "claude-3-5-sonnet"

      it "roundtrips ReplaySummary data type" $ do
        let summary = ReplaySummary 2 2 2 3 3 500 200 700 450.0 True []
        eitherDecode (encode summary) `shouldBe` Right summary

      it "roundtrips JournalState data type" $ do
        let st = JournalState [TurnStarted "t1", UserMsg "hi", TurnFinished "t1"] (Just "sess-1")
        eitherDecode (encode st) `shouldBe` Right st

      it "fails gracefully on malformed JSON for JournalEvent" $ do
        let badJson = "{\"type\":\"UnknownType\",\"invalidField\":true}"
        let res :: Either String JournalEvent = eitherDecode badJson
        res `shouldSatisfy` (\case Left _ -> True; Right _ -> False)

    describe "In-Memory Journal Interpreter (runJournalMemory)" $ do
      it "records events in chronological order" $ do
        let program = do
              recordEvent (TurnStarted "turn-1")
              recordEvent (UserMsg "Create a new file.")
              recordEvent (ToolInvoked "write_file" (object ["path" .= ("foo.txt" :: Text)]))
              recordEvent (ToolCompleted "write_file" (Right (object ["bytes" .= (12 :: Int)])))
              recordEvent (ModelTurn "File created.")
              recordEvent (TurnFinished "turn-1")
              getEvents

        (events, captured) <- runEff (runJournalMemory program)
        length events `shouldBe` 6
        captured `shouldBe` events
        case events of
          (firstEv:_) -> firstEv `shouldBe` TurnStarted "turn-1"
          []          -> expectationFailure "events list was empty"
        case reverse events of
          (lastEv:_) -> lastEv `shouldBe` TurnFinished "turn-1"
          []         -> expectationFailure "events list was empty"

      it "supports convenience smart constructors" $ do
        let program = do
              recordTurnStart "turn-2"
              recordUserMsg "What is 2+2?"
              recordToolCall "calc" (object ["expr" .= ("2+2" :: Text)])
              recordToolResult "calc" (Right (object ["result" .= (4 :: Int)]))
              recordModelTurn "The answer is 4."
              recordMetrics (ModelMetrics 50 10 60 45.0 "test-model")
              recordTurnFinish "turn-2"
              getEvents

        (events, _) <- runEff (runJournalMemory program)
        events `shouldBe`
          [ TurnStarted "turn-2"
          , UserMsg "What is 2+2?"
          , ToolInvoked "calc" (object ["expr" .= ("2+2" :: Text)])
          , ToolCompleted "calc" (Right (object ["result" .= (4 :: Int)]))
          , ModelTurn "The answer is 4."
          , MetricsReported (ModelMetrics 50 10 60 45.0 "test-model")
          , TurnFinished "turn-2"
          ]

      it "clears recorded events on clearEvents" $ do
        let program = do
              recordUserMsg "msg1"
              recordUserMsg "msg2"
              clearEvents
              recordUserMsg "msg3"
              getEvents

        (events, _) <- runEff (runJournalMemory program)
        events `shouldBe` [UserMsg "msg3"]

      it "supports runJournalMemoryWithState with pre-existing events" $ do
        let initial = JournalState [TurnStarted "t0", TurnFinished "t0"] (Just "s0")
        let program = do
              recordUserMsg "new message"
              getEvents

        (events, finalState) <- runEff (runJournalMemoryWithState initial program)
        length events `shouldBe` 3
        jsEvents finalState `shouldBe` events
        jsSessionId finalState `shouldBe` Just "s0"

      it "supports runJournalMemorySimple returning computation value" $ do
        let program = do
              recordUserMsg "compute 42"
              pure (42 :: Int)
        res <- runEff (runJournalMemorySimple program)
        res `shouldBe` 42

    describe "File-based Journal Persistence (runJournalFile)" $ do
      it "writes valid JSONL records to disk line-by-line" $ do
        withSystemTempDirectory "journal_file_test" $ \tmpDir -> do
          let journalPath = tmpDir ++ "/session.jsonl"
          let program = do
                recordTurnStart "t-1"
                recordUserMsg "Hello disk journal"
                recordModelTurn "Hello user"
                recordTurnFinish "t-1"

          runEff $ runJournalFile journalPath program

          fileContent <- TIO.readFile journalPath
          let fileLines = filter (not . T.null) (T.lines fileContent)
          length fileLines `shouldBe` 4

          let parsedEvents = map (eitherDecode . LBS.fromStrict . TE.encodeUtf8) fileLines
          parsedEvents `shouldBe`
            [ Right (TurnStarted "t-1")
            , Right (UserMsg "Hello disk journal")
            , Right (ModelTurn "Hello user")
            , Right (TurnFinished "t-1")
            ]

      it "flushes records immediately so they can be observed during execution" $ do
        withSystemTempDirectory "journal_flush_test" $ \tmpDir -> do
          let journalPath = tmpDir ++ "/live.jsonl"
          let program = do
                recordTurnStart "turn-live"
                recordUserMsg "Check disk immediately"
                -- Read file directly via IO while session is still active
                raw <- liftIO $ TIO.readFile journalPath
                pure (T.lines raw)

          linesDuringRun <- runEff $ runJournalFile journalPath program
          length linesDuringRun `shouldBe` 2

      it "persists and appends across multiple sequential runs on the same file" $ do
        withSystemTempDirectory "journal_append_test" $ \tmpDir -> do
          let journalPath = tmpDir ++ "/multi_run.jsonl"

          -- First run: Turn 1
          runEff $ runJournalFile journalPath $ do
            recordTurnStart "turn-1"
            recordUserMsg "First question"
            recordTurnFinish "turn-1"

          -- Second run: Turn 2 (resuming and appending)
          evs2 <- runEff $ runJournalFile journalPath $ do
            recordTurnStart "turn-2"
            recordUserMsg "Second question"
            recordTurnFinish "turn-2"
            getEvents

          length evs2 `shouldBe` 6
          evs2 `shouldBe`
            [ TurnStarted "turn-1"
            , UserMsg "First question"
            , TurnFinished "turn-1"
            , TurnStarted "turn-2"
            , UserMsg "Second question"
            , TurnFinished "turn-2"
            ]

      it "clears file content on clearEvents" $ do
        withSystemTempDirectory "journal_clear_test" $ \tmpDir -> do
          let journalPath = tmpDir ++ "/clear.jsonl"
          runEff $ runJournalFile journalPath $ do
            recordUserMsg "to be deleted"
            clearEvents
            recordUserMsg "kept"

          content <- TIO.readFile journalPath
          let cleanLines = filter (not . T.null) (T.lines content)
          length cleanLines `shouldBe` 1

      it "supports runJournalFileWithEvents" $ do
        withSystemTempDirectory "journal_with_events_test" $ \tmpDir -> do
          let journalPath = tmpDir ++ "/with_events.jsonl"
          (val, evs) <- runEff $ runJournalFileWithEvents journalPath $ do
            recordUserMsg "hello with events"
            pure ("done" :: Text)

          val `shouldBe` "done"
          evs `shouldBe` [UserMsg "hello with events"]

    describe "World-based Journal Persistence (runJournalFileWorld)" $ do
      it "writes and reads journal records in MemoryWorld virtual filesystem" $ do
        let virtualFs = initMemoryWorld []
        let program = do
              runJournalFileWorld "/workspace/.journal/events.jsonl" $ do
                recordTurnStart "vw-1"
                recordUserMsg "Virtual world event"
                recordModelTurn "Virtual world answer"
                recordTurnFinish "vw-1"
                getEvents

        (events, _) <- runEff (runWorldMemory virtualFs program)
        length events `shouldBe` 4
        events `shouldBe`
          [ TurnStarted "vw-1"
          , UserMsg "Virtual world event"
          , ModelTurn "Virtual world answer"
          , TurnFinished "vw-1"
          ]

    describe "Session Resume (resumeSession & reconstructChatHistory)" $ do
      it "resumes events from a JSONL file and reconstructs ChatMessage history" $ do
        withSystemTempDirectory "journal_resume_test" $ \tmpDir -> do
          let journalPath = tmpDir ++ "/resume.jsonl"
          runEff $ runJournalFile journalPath $ do
            recordTurnStart "t1"
            recordUserMsg "List files in directory"
            recordToolCall "list_files" (object ["dir" .= ("." :: Text)])
            recordToolResult "list_files" (Right (object ["files" .= (["a.txt", "b.txt"] :: [Text])]))
            recordModelTurn "I see a.txt and b.txt."
            recordTurnFinish "t1"

          resumedEvents <- runEff (resumeSession journalPath)
          length resumedEvents `shouldBe` 6

          let chatHistory = reconstructChatHistory resumedEvents
          length chatHistory `shouldBe` 3
          chatHistory `shouldBe`
            [ CoreTypes.UserMsg "List files in directory"
            , CoreTypes.ToolMsg "list_files" "{\"files\":[\"a.txt\",\"b.txt\"]}"
            , CoreTypes.AssistantMsg "I see a.txt and b.txt." []
            ]

      it "returns empty event list when resuming non-existent file" $ do
        resumed <- runEff (resumeSession "/non/existent/path/never_existed.jsonl")
        resumed `shouldBe` []

      it "returns Left error when loadJournalFile encounters malformed lines" $ do
        withSystemTempDirectory "journal_bad_file" $ \tmpDir -> do
          let badPath = tmpDir ++ "/corrupted.jsonl"
          TIO.writeFile badPath "{\"type\":\"TurnStarted\",\"turnId\":\"1\"}\nNOT_VALID_JSON\n"
          res <- runEff (loadJournalFile badPath)
          res `shouldSatisfy` (\case Left _ -> True; Right _ -> False)

      it "resumes session via World effect with resumeSessionWorld" $ do
        let jsonlText = "{\"type\":\"TurnStarted\",\"turnId\":\"t1\"}\n{\"type\":\"UserMsg\",\"content\":\"hi\"}\n{\"type\":\"TurnFinished\",\"turnId\":\"t1\"}\n"
        let virtualFs = initMemoryWorld [("workspace/session.jsonl", jsonlText)]
        let program = resumeSessionWorld "workspace/session.jsonl"

        (events, _) <- runEff (runWorldMemory virtualFs program)
        events `shouldBe` [TurnStarted "t1", UserMsg "hi", TurnFinished "t1"]

    describe "Audit Replay Verification (replayAudit & replayAuditSummary)" $ do
      it "validates a correct multi-turn session and aggregates metrics" $ do
        let events =
              [ TurnStarted "turn-1"
              , UserMsg "Question 1"
              , ToolInvoked "search" (object ["q" .= ("cats" :: Text)])
              , ToolCompleted "search" (Right (object ["count" .= (3 :: Int)]))
              , ModelTurn "Found 3 cats."
              , MetricsReported (ModelMetrics 100 30 130 120.0 "gpt-4o")
              , TurnFinished "turn-1"
              , TurnStarted "turn-2"
              , UserMsg "Question 2"
              , ModelTurn "Answer 2."
              , MetricsReported (ModelMetrics 200 50 250 210.5 "gpt-4o")
              , TurnFinished "turn-2"
              ]

        case replayAudit events of
          Left err -> expectationFailure ("Expected valid replay, got error: " ++ T.unpack err)
          Right summary -> do
            rsTotalTurns summary `shouldBe` 2
            rsUserMessages summary `shouldBe` 2
            rsModelTurns summary `shouldBe` 2
            rsToolInvocations summary `shouldBe` 1
            rsToolCompletions summary `shouldBe` 1
            rsPromptTokens summary `shouldBe` 300
            rsCompletionTokens summary `shouldBe` 80
            rsTotalTokens summary `shouldBe` 380
            rsTotalLatencyMs summary `shouldBe` 330.5
            rsIsValidSequence summary `shouldBe` True
            rsValidationErrors summary `shouldBe` []

      it "flags validation error on ToolCompleted without ToolInvoked" $ do
        let events =
              [ TurnStarted "t1"
              , ToolCompleted "unknown_tool" (Right (object []))
              , TurnFinished "t1"
              ]
        case replayAudit events of
          Left err -> err `shouldSatisfy` ("ToolCompleted without matching ToolInvoked" `T.isInfixOf`)
          Right _ -> expectationFailure "Expected replayAudit to fail on uninvoked tool completion"

      it "flags validation error on uncompleted ToolInvoked" $ do
        let events =
              [ TurnStarted "t1"
              , ToolInvoked "dangling_tool" (object [])
              , TurnFinished "t1"
              ]
        case replayAudit events of
          Left err -> err `shouldSatisfy` ("Tool invocation was never completed" `T.isInfixOf`)
          Right _ -> expectationFailure "Expected replayAudit to fail on dangling tool call"

      it "flags validation error on unclosed TurnStarted" $ do
        let events =
              [ TurnStarted "t1"
              , UserMsg "hello"
              ]
        case replayAudit events of
          Left err -> err `shouldSatisfy` ("Turn was started but never finished" `T.isInfixOf`)
          Right _ -> expectationFailure "Expected replayAudit to fail on unclosed turn"

      it "flags validation error on mismatched TurnFinished" $ do
        let events =
              [ TurnStarted "t1"
              , TurnFinished "t2"
              ]
        case replayAudit events of
          Left err -> err `shouldSatisfy` ("does not match active turn" `T.isInfixOf`)
          Right _ -> expectationFailure "Expected replayAudit to fail on mismatched turn ID"

      it "flags validation error on TurnFinished with no open turn" $ do
        let events =
              [ TurnFinished "t1"
              ]
        case replayAudit events of
          Left err -> err `shouldSatisfy` ("TurnFinished with no open turn" `T.isInfixOf`)
          Right _ -> expectationFailure "Expected replayAudit to fail on orphaned TurnFinished"

      it "flags validation error on nested TurnStarted without closing previous turn" $ do
        let events =
              [ TurnStarted "t1"
              , TurnStarted "t2"
              , TurnFinished "t2"
              , TurnFinished "t1"
              ]
        let summary = replayAuditSummary events
        rsIsValidSequence summary `shouldBe` False
        rsValidationErrors summary `shouldSatisfy` (\errs -> any ("Nested TurnStarted" `T.isInfixOf`) errs)

      it "handles empty event list as a valid zero-count session" $ do
        case replayAudit [] of
          Left err -> expectationFailure ("Empty session should be valid, got: " ++ T.unpack err)
          Right summary -> do
            rsTotalTurns summary `shouldBe` 0
            rsUserMessages summary `shouldBe` 0
            rsModelTurns summary `shouldBe` 0
            rsToolInvocations summary `shouldBe` 0
            rsToolCompletions summary `shouldBe` 0
            rsPromptTokens summary `shouldBe` 0
            rsCompletionTokens summary `shouldBe` 0
            rsTotalTokens summary `shouldBe` 0
            rsTotalLatencyMs summary `shouldBe` 0.0
            rsIsValidSequence summary `shouldBe` True

    describe "Stress & Resilience" $ do
      it "handles large event streams (500 events) in memory without leakage" $ do
        let n = 100
        let program = do
              forM_ [1 .. n] $ \(i :: Int) -> do
                let tid = "t-" <> T.pack (show i)
                recordTurnStart tid
                recordUserMsg ("Message " <> T.pack (show i))
                recordToolCall "tool" (object ["idx" .= i])
                recordToolResult "tool" (Right (object ["status" .= ("ok" :: Text)]))
                recordTurnFinish tid
              getEvents

        (events, _) <- runEff (runJournalMemory program)
        length events `shouldBe` (n * 5)
        case replayAudit events of
          Left err -> expectationFailure ("500 events audit failed: " ++ T.unpack err)
          Right summary -> do
            rsTotalTurns summary `shouldBe` n
            rsUserMessages summary `shouldBe` n
            rsToolInvocations summary `shouldBe` n
            rsToolCompletions summary `shouldBe` n
            rsIsValidSequence summary `shouldBe` True

      it "handles multi-line text and unicode in event payloads" $ do
        let multiLineText = "Line 1\nLine 2 with \t tabs\nLine 3 with 🚀 emoji and 日本語 text."
        let ev = UserMsg multiLineText
        let encoded = encode ev
        eitherDecode encoded `shouldBe` Right ev

        withSystemTempDirectory "journal_unicode_test" $ \tmpDir -> do
          let p = tmpDir ++ "/unicode.jsonl"
          runEff $ runJournalFile p $ do
            recordUserMsg multiLineText
            recordModelTurn "Response with 🌟 and \n newlines"

          resumed <- runEff (resumeSession p)
          length resumed `shouldBe` 2
          case resumed of
            (firstResumed:_) -> firstResumed `shouldBe` UserMsg multiLineText
            []               -> expectationFailure "resumed list was empty"
