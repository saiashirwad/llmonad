{-# LANGUAGE OverloadedStrings #-}

module LLMonad.ReplaySpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (eitherDecode, encode, object, (.=), Value)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Effectful
import LLMonad.Journal
import LLMonad.Journal.File
import LLMonad.Journal.Memory
import LLMonad.Providers.Anthropic (encodeAnthropicMessages)
import LLMonad.Providers.OpenAICompatible (buildChatCompletionsBody, defaultOpenAICompatConfig, StructuredTier (..))
import LLMonad.Types hiding (UserMsg)
import qualified LLMonad.Types as CoreTypes
import LLMonad.World.Memory (initMemoryWorld, runWorldMemory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = do
  describe "LLMonad.Journal.Replay (Batch 5 Fidelity)" $ do

    describe "1. Tool Call ID Preservation & Serialization Fidelity" $ do
      it "preserves distinct toolCallId and toolName across serialization" $ do
        let ev = ToolInvoked "call_abc123" "grep_search" (object ["query" .= ("pattern" :: Text)])
        let encoded = encode ev
        eitherDecode encoded `shouldBe` Right ev

      it "preserves toolCallId in ToolCompleted across serialization" $ do
        let res = Right (object ["matches" .= ([1, 2, 3] :: [Int])])
            ev = ToolCompleted "call_abc123" res
        let encoded = encode ev
        eitherDecode encoded `shouldBe` Right ev

      it "preserves [ToolCall] in ModelTurn across serialization" $ do
        let calls =
              [ ToolCall "call_1" "view_file" (object ["path" .= ("src/A.hs" :: Text)])
              , ToolCall "call_2" "edit_file" (object ["path" .= ("src/B.hs" :: Text)])
              ]
            ev = ModelTurn "I will inspect and edit the modules." calls
        let encoded = encode ev
        eitherDecode encoded `shouldBe` Right ev

      it "parses legacy JSONL lacking toolCallId by defaulting to toolName" $ do
        let legacyInvoked = "{\"type\":\"ToolInvoked\",\"toolName\":\"view_file\",\"arguments\":{\"path\":\"foo.hs\"}}"
        case eitherDecode legacyInvoked of
          Left err -> expectationFailure ("Legacy ToolInvoked decode failed: " ++ err)
          Right ev -> ev `shouldBe` ToolInvoked "view_file" "view_file" (object ["path" .= ("foo.hs" :: Text)])

      it "parses legacy JSONL ToolCompleted lacking toolCallId by defaulting to toolName" $ do
        let legacyCompleted = "{\"type\":\"ToolCompleted\",\"toolName\":\"view_file\",\"result\":{\"status\":\"ok\"}}"
        case eitherDecode legacyCompleted of
          Left err -> expectationFailure ("Legacy ToolCompleted decode failed: " ++ err)
          Right ev -> ev `shouldBe` ToolCompleted "view_file" (Right (object ["status" .= ("ok" :: Text)]))

      it "parses legacy JSONL ModelTurn lacking toolCalls by defaulting to empty list" $ do
        let legacyTurn = "{\"type\":\"ModelTurn\",\"content\":\"Hello world\"}"
        case eitherDecode legacyTurn of
          Left err -> expectationFailure ("Legacy ModelTurn decode failed: " ++ err)
          Right ev -> ev `shouldBe` ModelTurn "Hello world" []

    describe "2. Reconstruct Chat History Fidelity" $ do
      it "reconstructs AssistantMsg with [ToolCall] and matching ToolMsg with toolCallId" $ do
        let call1 = ToolCall "call_01" "view_file" (object ["path" .= ("src/A.hs" :: Text)])
            call2 = ToolCall "call_02" "view_file" (object ["path" .= ("src/B.hs" :: Text)])
            events =
              [ TurnStarted "turn-1"
              , UserMsg "Please inspect src/A.hs and src/B.hs."
              , ModelTurn "Reading both files in parallel." [call1, call2]
              , ToolInvoked "call_01" "view_file" (object ["path" .= ("src/A.hs" :: Text)])
              , ToolInvoked "call_02" "view_file" (object ["path" .= ("src/B.hs" :: Text)])
              , ToolCompleted "call_01" (Right (object ["content" .= ("module A where" :: Text)]))
              , ToolCompleted "call_02" (Right (object ["content" .= ("module B where" :: Text)]))
              , ModelTurn "Both files inspected successfully." []
              , TurnFinished "turn-1"
              ]

        let chatHistory = reconstructChatHistory events
        length chatHistory `shouldBe` 5
        chatHistory `shouldBe`
          [ CoreTypes.UserMsg "Please inspect src/A.hs and src/B.hs."
          , CoreTypes.AssistantMsg "Reading both files in parallel." [call1, call2]
          , CoreTypes.ToolMsg "call_01" "{\"content\":\"module A where\"}"
          , CoreTypes.ToolMsg "call_02" "{\"content\":\"module B where\"}"
          , CoreTypes.AssistantMsg "Both files inspected successfully." []
          ]

      it "produces valid provider wire request bodies with reconstructed chat history" $ do
        let call = ToolCall "call_x99" "list_dir" (object ["directoryPath" .= ("." :: Text)])
            events =
              [ UserMsg "List root directory"
              , ModelTurn "" [call]
              , ToolCompleted "call_x99" (Right (object ["entries" .= (["src", "test"] :: [Text])]))
              , ModelTurn "I see src and test." []
              ]
            history = reconstructChatHistory events
            req = CompletionRequest
              { crModel = Model "gpt-4o"
              , crSystem = Just "System instruction"
              , crMessages = history
              , crParams = defaultParams
              , crTools = []
              , crToolChoice = ToolAuto
              , crResponseFormat = RfText
              }
        -- OpenAICompatible request serialization
        let oaiBody = buildChatCompletionsBody (defaultOpenAICompatConfig "https://api.openai.com/v1") TierPromptOnly req
        let oaiEncoded = encode oaiBody
        LBS.null oaiEncoded `shouldBe` False

        -- Anthropic request serialization
        let anthropicMsgs = encodeAnthropicMessages history
        let anthropicEncoded = encode anthropicMsgs
        LBS.null anthropicEncoded `shouldBe` False

    describe "3. Fail-Closed Deserialization & Resilience" $ do
      it "loadJournalText returns Left error on invalid JSON lines with line numbers" $ do
        let textWithBadLine = "{\"type\":\"TurnStarted\",\"turnId\":\"1\"}\nNOT_VALID_JSON\n{\"type\":\"TurnFinished\",\"turnId\":\"1\"}"
        case loadJournalText textWithBadLine of
          Left err -> err `shouldSatisfy` ("Line 2:" `T.isInfixOf`)
          Right _ -> expectationFailure "Expected loadJournalText to fail on malformed line"

      it "loadJournalFile returns Left when file does not exist" $ do
        res <- runEff (loadJournalFile "/non/existent/path/never_existed.jsonl")
        case res of
          Left err -> err `shouldSatisfy` ("does not exist" `T.isInfixOf`)
          Right _ -> expectationFailure "Expected loadJournalFile to fail on missing file"

      it "loadJournalFileWorld returns Left when file does not exist" $ do
        let virtualFs = initMemoryWorld []
        (res, _) <- runEff $ runWorldMemory virtualFs (loadJournalFileWorld "missing.jsonl")
        case res of
          Left err -> err `shouldSatisfy` ("does not exist" `T.isInfixOf`)
          Right _ -> expectationFailure "Expected loadJournalFileWorld to fail on missing file"

      it "resumeSession fails closed with IO exception when file is corrupted" $ do
        withSystemTempDirectory "journal_replay_corrupt" $ \tmpDir -> do
          let p = tmpDir ++ "/corrupted.jsonl"
          TIO.writeFile p "{\"type\":\"TurnStarted\",\"turnId\":\"1\"}\n{\"type\":\"BROKEN\n"
          runEff (resumeSession p) `shouldThrow` anyIOException

      it "resumeSessionWorld fails closed with exception when file is corrupted" $ do
        let badContent = "{\"type\":\"TurnStarted\",\"turnId\":\"1\"}\nBROKEN_JSON_DATA\n"
        let virtualFs = initMemoryWorld [("session.jsonl", badContent)]
        runEff (runWorldMemory virtualFs (resumeSessionWorld "session.jsonl")) `shouldThrow` anyException

      it "runJournalFile fails closed with IO exception when initial file is corrupted" $ do
        withSystemTempDirectory "journal_run_corrupt" $ \tmpDir -> do
          let p = tmpDir ++ "/corrupted.jsonl"
          TIO.writeFile p "MALFORMED_INITIAL_DATA\n"
          runEff (runJournalFile p (recordUserMsg "hi")) `shouldThrow` anyIOException

      it "runJournalFileWorld fails closed with exception when initial file is corrupted" $ do
        let badContent = "MALFORMED_INITIAL_WORLD_DATA\n"
        let virtualFs = initMemoryWorld [("workspace/session.jsonl", badContent)]
        runEff (runWorldMemory virtualFs (runJournalFileWorld "workspace/session.jsonl" (recordUserMsg "hi"))) `shouldThrow` anyException

    describe "4. Multi-Turn Tool Replay Audit Validation" $ do
      it "tracks parallel tool invocations by toolCallId correctly in replayAudit" $ do
        let events =
              [ TurnStarted "turn-1"
              , UserMsg "Perform parallel tasks"
              , ToolInvoked "call_101" "task_a" (object ["arg" .= (1 :: Int)])
              , ToolInvoked "call_102" "task_b" (object ["arg" .= (2 :: Int)])
              , ToolCompleted "call_102" (Right (object ["res" .= (20 :: Int)]))
              , ToolCompleted "call_101" (Right (object ["res" .= (10 :: Int)]))
              , ModelTurn "All tasks completed." []
              , TurnFinished "turn-1"
              ]
        case replayAudit events of
          Left err -> expectationFailure ("Expected valid parallel replay, got: " ++ T.unpack err)
          Right summary -> do
            rsTotalTurns summary `shouldBe` 1
            rsToolInvocations summary `shouldBe` 2
            rsToolCompletions summary `shouldBe` 2
            rsIsValidSequence summary `shouldBe` True

      it "flags validation error when a specific toolCallId is never completed" $ do
        let events =
              [ TurnStarted "turn-1"
              , UserMsg "Perform parallel tasks"
              , ToolInvoked "call_101" "task_a" (object ["arg" .= (1 :: Int)])
              , ToolInvoked "call_102" "task_b" (object ["arg" .= (2 :: Int)])
              , ToolCompleted "call_101" (Right (object ["res" .= (10 :: Int)]))
              -- call_102 is never completed
              , ModelTurn "Done" []
              , TurnFinished "turn-1"
              ]
        case replayAudit events of
          Left err -> err `shouldSatisfy` ("call_102" `T.isInfixOf`)
          Right _ -> expectationFailure "Expected replayAudit to fail on uncompleted call_102"
