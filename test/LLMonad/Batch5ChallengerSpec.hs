{-# LANGUAGE OverloadedStrings #-}

module LLMonad.Batch5ChallengerSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), eitherDecode, encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Effectful
import LLMonad.Journal
import LLMonad.Journal.File
import LLMonad.Journal.Memory
import LLMonad.Providers.Anthropic (encodeAnthropicMessages)
import LLMonad.Providers.OpenAICompatible (StructuredTier (..), buildChatCompletionsBody, defaultOpenAICompatConfig)
import LLMonad.Types hiding (UserMsg)
import qualified LLMonad.Types as CoreTypes
import LLMonad.World.Memory (initMemoryWorld, runWorldMemory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = do
  describe "Batch 5 Challenger Empirical Stress Suite" $ do

    describe "1. Multi-Turn Complex Journal Replay & History Reconstruction" $ do
      it "reconstructs complex multi-turn conversation with parallel, sequential, and error tool calls" $ do
        let -- Turn 1 tool calls (parallel: 3 calls)
            tc1 = ToolCall "call_p1" "read_file" (object ["path" .= ("src/A.hs" :: Text)])
            tc2 = ToolCall "call_p2" "fetch_api" (object ["url" .= ("https://example.com/api" :: Text)])
            tc3 = ToolCall "call_p3" "calc_hash" (object ["algorithm" .= ("sha256" :: Text), "salt" .= Null])
            -- Turn 2 tool calls (sequential retry after error)
            tc4 = ToolCall "call_s1" "search_db" (object ["query" .= ("users" :: Text)])
            tc5 = ToolCall "call_s2" "search_db" (object ["query" .= ("users_v2" :: Text)])
            -- Turn 3 (large parallel batch: 4 calls)
            tc6 = ToolCall "call_b1" "tool1" (object ["k" .= (1 :: Int)])
            tc7 = ToolCall "call_b2" "tool2" (object ["k" .= (2 :: Int)])
            tc8 = ToolCall "call_b3" "tool3" (object ["k" .= (3 :: Int)])
            tc9 = ToolCall "call_b4" "tool4" (object ["k" .= (4 :: Int)])

            events =
              -- TURN 1: User prompt -> 3 parallel tools (1 ok, 1 error, 1 complex json)
              [ TurnStarted "turn-1"
              , UserMsg "Please analyze the files and hash."
              , ModelTurn "Starting parallel operations..." [tc1, tc2, tc3]
              , ToolInvoked "call_p1" "read_file" (object ["path" .= ("src/A.hs" :: Text)])
              , ToolInvoked "call_p2" "fetch_api" (object ["url" .= ("https://example.com/api" :: Text)])
              , ToolInvoked "call_p3" "calc_hash" (object ["algorithm" .= ("sha256" :: Text), "salt" .= Null])
              -- Complete out of order: tc2 (error), tc3 (ok), tc1 (ok)
              , ToolCompleted "call_p2" (Left "HTTP 503 Service Unavailable")
              , ToolCompleted "call_p3" (Right (object ["hash" .= ("a1b2c3d4" :: Text), "rounds" .= (1000 :: Int)]))
              , ToolCompleted "call_p1" (Right (object ["lines" .= ([1, 2, 3] :: [Int]), "content" .= ("module A where\nfoo = 1" :: Text)]))
              , ModelTurn "Parallel phase complete. File read and hash computed, but API failed." []
              , MetricsReported (ModelMetrics 400 120 520 150.0 "claude-3-5-sonnet")
              , TurnFinished "turn-1"

              -- TURN 2: User follow-up -> Sequential tool calls
              , TurnStarted "turn-2"
              , UserMsg "Try searching the database instead."
              , ModelTurn "Searching database query 1..." [tc4]
              , ToolInvoked "call_s1" "search_db" (object ["query" .= ("users" :: Text)])
              , ToolCompleted "call_s1" (Left "Table 'users' not found")
              , ModelTurn "Retrying with table 'users_v2'..." [tc5]
              , ToolInvoked "call_s2" "search_db" (object ["query" .= ("users_v2" :: Text)])
              , ToolCompleted "call_s2" (Right (object ["count" .= (42 :: Int)]))
              , ModelTurn "Found 42 users in users_v2." []
              , MetricsReported (ModelMetrics 250 80 330 110.0 "claude-3-5-sonnet")
              , TurnFinished "turn-2"

              -- TURN 3: Conversational turn with no tools
              , TurnStarted "turn-3"
              , UserMsg "Summarize our findings so far."
              , ModelTurn "Summary: src/A.hs is loaded, sha256 hash is computed, and 42 users exist." []
              , MetricsReported (ModelMetrics 150 40 190 60.0 "claude-3-5-sonnet")
              , TurnFinished "turn-3"

              -- TURN 4: 4 parallel tool calls completed in reverse order
              , TurnStarted "turn-4"
              , UserMsg "Run batch checks 1 to 4."
              , ModelTurn "Running batch tools..." [tc6, tc7, tc8, tc9]
              , ToolInvoked "call_b1" "tool1" (object ["k" .= (1 :: Int)])
              , ToolInvoked "call_b2" "tool2" (object ["k" .= (2 :: Int)])
              , ToolInvoked "call_b3" "tool3" (object ["k" .= (3 :: Int)])
              , ToolInvoked "call_b4" "tool4" (object ["k" .= (4 :: Int)])
              , ToolCompleted "call_b4" (Right (object ["v" .= (40 :: Int)]))
              , ToolCompleted "call_b3" (Right (object ["v" .= (30 :: Int)]))
              , ToolCompleted "call_b2" (Left "batch tool 2 timeout")
              , ToolCompleted "call_b1" (Right (object ["v" .= (10 :: Int)]))
              , ModelTurn "All batch checks finished." []
              , MetricsReported (ModelMetrics 600 150 750 200.0 "claude-3-5-sonnet")
              , TurnFinished "turn-4"
              ]

        -- 1. Validate audit replay summary
        case replayAudit events of
          Left err -> expectationFailure ("Replay audit failed on complex multi-turn: " ++ T.unpack err)
          Right summary -> do
            rsTotalTurns summary `shouldBe` 4
            rsUserMessages summary `shouldBe` 4
            rsModelTurns summary `shouldBe` 8
            rsToolInvocations summary `shouldBe` 9
            rsToolCompletions summary `shouldBe` 9
            rsPromptTokens summary `shouldBe` (400 + 250 + 150 + 600)
            rsCompletionTokens summary `shouldBe` (120 + 80 + 40 + 150)
            rsIsValidSequence summary `shouldBe` True

        -- 2. Reconstruct chat history
        let history = reconstructChatHistory events
        length history `shouldBe` 21

        -- 3. Verify exact message sequence and toolCallId correspondence
        history `shouldBe`
          [ -- Turn 1
            CoreTypes.UserMsg "Please analyze the files and hash."
          , CoreTypes.AssistantMsg "Starting parallel operations..." [tc1, tc2, tc3]
          , CoreTypes.ToolMsg "call_p2" "Error: HTTP 503 Service Unavailable"
          , CoreTypes.ToolMsg "call_p3" "{\"hash\":\"a1b2c3d4\",\"rounds\":1000}"
          , CoreTypes.ToolMsg "call_p1" "{\"content\":\"module A where\\nfoo = 1\",\"lines\":[1,2,3]}"
          , CoreTypes.AssistantMsg "Parallel phase complete. File read and hash computed, but API failed." []
            -- Turn 2
          , CoreTypes.UserMsg "Try searching the database instead."
          , CoreTypes.AssistantMsg "Searching database query 1..." [tc4]
          , CoreTypes.ToolMsg "call_s1" "Error: Table 'users' not found"
          , CoreTypes.AssistantMsg "Retrying with table 'users_v2'..." [tc5]
          , CoreTypes.ToolMsg "call_s2" "{\"count\":42}"
          , CoreTypes.AssistantMsg "Found 42 users in users_v2." []
            -- Turn 3
          , CoreTypes.UserMsg "Summarize our findings so far."
          , CoreTypes.AssistantMsg "Summary: src/A.hs is loaded, sha256 hash is computed, and 42 users exist." []
            -- Turn 4
          , CoreTypes.UserMsg "Run batch checks 1 to 4."
          , CoreTypes.AssistantMsg "Running batch tools..." [tc6, tc7, tc8, tc9]
          , CoreTypes.ToolMsg "call_b4" "{\"v\":40}"
          , CoreTypes.ToolMsg "call_b3" "{\"v\":30}"
          , CoreTypes.ToolMsg "call_b2" "Error: batch tool 2 timeout"
          , CoreTypes.ToolMsg "call_b1" "{\"v\":10}"
          , CoreTypes.AssistantMsg "All batch checks finished." []
          ]

    describe "2. Wire Format Serialization Stress Tests (OpenAI & Anthropic)" $ do
      it "serializes reconstructed history to valid OpenAI Chat Completions wire format" $ do
        let tc1 = ToolCall "call_101" "grep" (object ["query" .= ("main" :: Text)])
            tc2 = ToolCall "call_102" "edit" (object ["file" .= ("Main.hs" :: Text)])
            events =
              [ UserMsg "Find main and edit it"
              , ModelTurn "Running grep and edit" [tc1, tc2]
              , ToolCompleted "call_101" (Right (object ["count" .= (1 :: Int)]))
              , ToolCompleted "call_102" (Left "Permission denied")
              , ModelTurn "Done" []
              ]
            history = reconstructChatHistory events
            req = CompletionRequest
              { crModel = Model "gpt-4o"
              , crSystem = Just "System prompt"
              , crMessages = history
              , crParams = defaultParams
              , crTools = []
              , crToolChoice = ToolAuto
              , crResponseFormat = RfText
              }
            body = buildChatCompletionsBody (defaultOpenAICompatConfig "https://api.openai.com/v1") TierPromptOnly req

        -- Inspect JSON representation of the OpenAI request body
        case body of
          Object o -> do
            KM.lookup "model" o `shouldBe` Just (String "gpt-4o")
            case KM.lookup "messages" o of
              Just (Array msgs) -> do
                -- messages: system, user, assistant, tool 1, tool 2, assistant
                V.length msgs `shouldBe` 6
                -- msg 0: system
                case msgs V.! 0 of
                  Object m -> KM.lookup "role" m `shouldBe` Just (String "system")
                  _ -> expectationFailure "Expected system message"
                -- msg 1: user
                case msgs V.! 1 of
                  Object m -> do
                    KM.lookup "role" m `shouldBe` Just (String "user")
                    KM.lookup "content" m `shouldBe` Just (String "Find main and edit it")
                  _ -> expectationFailure "Expected user message"
                -- msg 2: assistant with tool calls
                case msgs V.! 2 of
                  Object m -> do
                    KM.lookup "role" m `shouldBe` Just (String "assistant")
                    case KM.lookup "tool_calls" m of
                      Just (Array calls) -> do
                        V.length calls `shouldBe` 2
                        -- Check tool call 1
                        case calls V.! 0 of
                          Object c -> do
                            KM.lookup "id" c `shouldBe` Just (String "call_101")
                            KM.lookup "type" c `shouldBe` Just (String "function")
                            case KM.lookup "function" c of
                              Just (Object f) -> do
                                KM.lookup "name" f `shouldBe` Just (String "grep")
                                KM.lookup "arguments" f `shouldBe` Just (String "{\"query\":\"main\"}")
                              _ -> expectationFailure "Expected function object"
                          _ -> expectationFailure "Expected tool call object"
                        -- Check tool call 2
                        case calls V.! 1 of
                          Object c -> do
                            KM.lookup "id" c `shouldBe` Just (String "call_102")
                            KM.lookup "type" c `shouldBe` Just (String "function")
                            case KM.lookup "function" c of
                              Just (Object f) -> do
                                KM.lookup "name" f `shouldBe` Just (String "edit")
                                KM.lookup "arguments" f `shouldBe` Just (String "{\"file\":\"Main.hs\"}")
                              _ -> expectationFailure "Expected function object"
                          _ -> expectationFailure "Expected tool call object"
                      _ -> expectationFailure "Expected tool_calls array"
                  _ -> expectationFailure "Expected assistant message"
                -- msg 3: tool 1
                case msgs V.! 3 of
                  Object m -> do
                    KM.lookup "role" m `shouldBe` Just (String "tool")
                    KM.lookup "tool_call_id" m `shouldBe` Just (String "call_101")
                    KM.lookup "content" m `shouldBe` Just (String "{\"count\":1}")
                  _ -> expectationFailure "Expected tool message"
                -- msg 4: tool 2 (error)
                case msgs V.! 4 of
                  Object m -> do
                    KM.lookup "role" m `shouldBe` Just (String "tool")
                    KM.lookup "tool_call_id" m `shouldBe` Just (String "call_102")
                    KM.lookup "content" m `shouldBe` Just (String "Error: Permission denied")
                  _ -> expectationFailure "Expected tool message"
                -- msg 5: assistant final
                case msgs V.! 5 of
                  Object m -> do
                    KM.lookup "role" m `shouldBe` Just (String "assistant")
                    KM.lookup "content" m `shouldBe` Just (String "Done")
                  _ -> expectationFailure "Expected assistant message"
              _ -> expectationFailure "Expected messages array"
          _ -> expectationFailure "Expected root JSON object"

      it "serializes reconstructed history to valid Anthropic Messages wire format" $ do
        let tc1 = ToolCall "call_ant1" "bash" (object ["command" .= ("ls -la" :: Text)])
            tc2 = ToolCall "call_ant2" "bash" (object ["command" .= ("whoami" :: Text)])
            events =
              [ UserMsg "Run system inspection commands"
              , ModelTurn "Running commands..." [tc1, tc2]
              , ToolCompleted "call_ant1" (Right (object ["stdout" .= ("total 0\n" :: Text)]))
              , ToolCompleted "call_ant2" (Right (object ["stdout" .= ("root\n" :: Text)]))
              , ModelTurn "Commands completed." []
              ]
            history = reconstructChatHistory events
            anthropicMsgs = encodeAnthropicMessages history

        -- In Anthropic format, consecutive tool responses must be grouped into a single user message
        length anthropicMsgs `shouldBe` 4
        -- msg 0: user message
        case anthropicMsgs !! 0 of
          Object m -> do
            KM.lookup "role" m `shouldBe` Just (String "user")
            case KM.lookup "content" m of
              Just (Array blocks) -> do
                V.length blocks `shouldBe` 1
                case blocks V.! 0 of
                  Object b -> do
                    KM.lookup "type" b `shouldBe` Just (String "text")
                    KM.lookup "text" b `shouldBe` Just (String "Run system inspection commands")
                  _ -> expectationFailure "Expected text block"
              _ -> expectationFailure "Expected content array"
          _ -> expectationFailure "Expected user object"

        -- msg 1: assistant message with tool_use blocks
        case anthropicMsgs !! 1 of
          Object m -> do
            KM.lookup "role" m `shouldBe` Just (String "assistant")
            case KM.lookup "content" m of
              Just (Array blocks) -> do
                V.length blocks `shouldBe` 3 -- 1 text block + 2 tool_use blocks
                case blocks V.! 0 of
                  Object b -> do
                    KM.lookup "type" b `shouldBe` Just (String "text")
                    KM.lookup "text" b `shouldBe` Just (String "Running commands...")
                  _ -> expectationFailure "Expected text block"
                case blocks V.! 1 of
                  Object b -> do
                    KM.lookup "type" b `shouldBe` Just (String "tool_use")
                    KM.lookup "id" b `shouldBe` Just (String "call_ant1")
                    KM.lookup "name" b `shouldBe` Just (String "bash")
                    KM.lookup "input" b `shouldBe` Just (object ["command" .= ("ls -la" :: Text)])
                  _ -> expectationFailure "Expected tool_use block"
                case blocks V.! 2 of
                  Object b -> do
                    KM.lookup "type" b `shouldBe` Just (String "tool_use")
                    KM.lookup "id" b `shouldBe` Just (String "call_ant2")
                    KM.lookup "name" b `shouldBe` Just (String "bash")
                    KM.lookup "input" b `shouldBe` Just (object ["command" .= ("whoami" :: Text)])
                  _ -> expectationFailure "Expected tool_use block"
              _ -> expectationFailure "Expected content array"
          _ -> expectationFailure "Expected assistant object"

        -- msg 2: grouped user message with tool_result blocks
        case anthropicMsgs !! 2 of
          Object m -> do
            KM.lookup "role" m `shouldBe` Just (String "user")
            case KM.lookup "content" m of
              Just (Array blocks) -> do
                V.length blocks `shouldBe` 2
                case blocks V.! 0 of
                  Object b -> do
                    KM.lookup "type" b `shouldBe` Just (String "tool_result")
                    KM.lookup "tool_use_id" b `shouldBe` Just (String "call_ant1")
                    KM.lookup "content" b `shouldBe` Just (String "{\"stdout\":\"total 0\\n\"}")
                  _ -> expectationFailure "Expected tool_result block"
                case blocks V.! 1 of
                  Object b -> do
                    KM.lookup "type" b `shouldBe` Just (String "tool_result")
                    KM.lookup "tool_use_id" b `shouldBe` Just (String "call_ant2")
                    KM.lookup "content" b `shouldBe` Just (String "{\"stdout\":\"root\\n\"}")
                  _ -> expectationFailure "Expected tool_result block"
              _ -> expectationFailure "Expected content array"
          _ -> expectationFailure "Expected user object"

        -- msg 3: assistant final message
        case anthropicMsgs !! 3 of
          Object m -> do
            KM.lookup "role" m `shouldBe` Just (String "assistant")
            case KM.lookup "content" m of
              Just (Array blocks) -> do
                V.length blocks `shouldBe` 1
                case blocks V.! 0 of
                  Object b -> do
                    KM.lookup "type" b `shouldBe` Just (String "text")
                    KM.lookup "text" b `shouldBe` Just (String "Commands completed.")
                  _ -> expectationFailure "Expected text block"
              _ -> expectationFailure "Expected content array"
          _ -> expectationFailure "Expected assistant object"

    describe "3. JSONL Roundtrip & Disk Persistence Fidelity" $ do
      it "roundtrips full multi-turn conversation with special characters through disk JSONL" $ do
        withSystemTempDirectory "journal_challenger_disk" $ \tmpDir -> do
          let p = tmpDir ++ "/full_session.jsonl"
          let tcSpecial = ToolCall "call_spec_1" "code_gen" (object ["code" .= ("alert(\"hello\\nworld\");\n// 🚀\n" :: Text)])
          let events =
                [ TurnStarted "turn-spec"
                , UserMsg "Generate code with emojis: 🌟 🚀 and special characters \t \r \n"
                , ModelTurn "Generating code snippet..." [tcSpecial]
                , ToolInvoked "call_spec_1" "code_gen" (object ["code" .= ("alert(\"hello\\nworld\");\n// 🚀\n" :: Text)])
                , ToolCompleted "call_spec_1" (Right (object ["status" .= ("success" :: Text), "lines" .= (3 :: Int)]))
                , ModelTurn "Code generated." []
                , TurnFinished "turn-spec"
                ]

          -- Write events
          runEff $ runJournalFile p $ do
            forM_ events recordEvent

          -- Read events back via resumeSession
          resumed <- runEff (resumeSession p)
          resumed `shouldBe` events

          -- Reconstruct history
          let hist = reconstructChatHistory resumed
          length hist `shouldBe` 4

          -- Verify wire format generation from resumed history
          let anthropicMsgs = encodeAnthropicMessages hist
          let oaiBody = buildChatCompletionsBody (defaultOpenAICompatConfig "https://api.openai.com/v1") TierPromptOnly
                          (CompletionRequest (Model "m") Nothing hist defaultParams [] ToolAuto RfText)

          LBS.null (encode anthropicMsgs) `shouldBe` False
          LBS.null (encode oaiBody) `shouldBe` False

    describe "4. Fail-Closed Boundary & Corruption Stress Tests" $ do
      it "fails closed on blank-line interleaved corruption in JSONL" $ do
        let badContent = "{\"type\":\"TurnStarted\",\"turnId\":\"1\"}\n\n   \n{\"type\":\"INVALID_TAG\"}\n"
        case loadJournalText badContent of
          Left err -> err `shouldSatisfy` ("unknown event type" `T.isInfixOf`)
          Right _ -> expectationFailure "Expected loadJournalText to fail closed"

      it "fails closed on unescaped JSON controls in tool arguments" $ do
        let unescapedJson = "{\"type\":\"ToolInvoked\",\"toolCallId\":\"c1\",\"toolName\":\"t\",\"arguments\":{\"bad\":\n}}"
        case loadJournalText unescapedJson of
          Left err -> err `shouldSatisfy` ("Line 1:" `T.isInfixOf`)
          Right _ -> expectationFailure "Expected loadJournalText to fail on malformed JSON"

    describe "5. High-Volume & Rapid Turn Stress Testing" $ do
      it "processes 200 turns (1,200 events) with parallel tools through replay, reconstruction, and provider serialization" $ do
        let numTurns = 200
        let program = do
              forM_ [1 .. numTurns] $ \(i :: Int) -> do
                let tid = "turn-" <> T.pack (show i)
                let c1 = "call_a_" <> T.pack (show i)
                let c2 = "call_b_" <> T.pack (show i)
                let tc1 = ToolCall c1 "worker_a" (object ["idx" .= i])
                let tc2 = ToolCall c2 "worker_b" (object ["idx" .= (i * 10)])
                recordTurnStart tid
                recordUserMsg ("Instruction " <> T.pack (show i))
                recordModelTurnWithCalls ("Model analyzing " <> T.pack (show i)) [tc1, tc2]
                recordToolCallWithId c1 "worker_a" (object ["idx" .= i])
                recordToolCallWithId c2 "worker_b" (object ["idx" .= (i * 10)])
                -- Alternate success / error across calls
                if even i
                  then do
                    recordToolResult c1 (Right (object ["status" .= ("ok" :: Text)]))
                    recordToolResult c2 (Left "Simulated network timeout")
                  else do
                    recordToolResult c1 (Left "Simulated auth error")
                    recordToolResult c2 (Right (object ["status" .= ("success" :: Text)]))
                recordModelTurn ("Turn " <> T.pack (show i) <> " completed.")
                recordMetrics (ModelMetrics (i * 10) (i * 2) (i * 12) 50.0 "claude-3-5-sonnet")
                recordTurnFinish tid
              getEvents

        (events, _) <- runEff (runJournalMemory program)
        length events `shouldBe` (numTurns * 10)

        -- 1. Replay audit check
        case replayAudit events of
          Left err -> expectationFailure ("Audit failed on 2000 events: " ++ T.unpack err)
          Right summary -> do
            rsTotalTurns summary `shouldBe` numTurns
            rsUserMessages summary `shouldBe` numTurns
            rsModelTurns summary `shouldBe` (numTurns * 2)
            rsToolInvocations summary `shouldBe` (numTurns * 2)
            rsToolCompletions summary `shouldBe` (numTurns * 2)
            rsIsValidSequence summary `shouldBe` True

        -- 2. Chat history reconstruction
        let history = reconstructChatHistory events
        length history `shouldBe` (numTurns * 5)

        -- 3. Anthropic wire encoding
        let antMsgs = encodeAnthropicMessages history
        -- In Anthropic format, each turn has UserMsg (1), AssistantMsg with tools (1), UserMsg grouping tool results (1), AssistantMsg final (1) -> 4 msgs per turn
        length antMsgs `shouldBe` (numTurns * 4)

        -- 4. OpenAI wire encoding
        let req = CompletionRequest (Model "gpt-4o") Nothing history defaultParams [] ToolAuto RfText
        let oaiBody = buildChatCompletionsBody (defaultOpenAICompatConfig "https://api.openai.com/v1") TierPromptOnly req
        case oaiBody of
          Object o -> case KM.lookup "messages" o of
            Just (Array msgs) -> V.length msgs `shouldBe` (numTurns * 5)
            _ -> expectationFailure "Expected messages array"
          _ -> expectationFailure "Expected root JSON object"

    describe "6. Deep Out-of-Order Parallel Tool Resolution Stress" $ do
      it "correctly matches 10 concurrent parallel tool calls completed in reverse order" $ do
        let toolIds = [ "call_id_" <> T.pack (show i) | i <- [1 .. 10 :: Int] ]
        let toolCalls = [ ToolCall tid ("tool_" <> T.pack (show i)) (object ["idx" .= i]) | (tid, i) <- zip toolIds [1 .. 10 :: Int] ]
        let invEvents = [ ToolInvoked tid ("tool_" <> T.pack (show i)) (object ["idx" .= i]) | (tid, i) <- zip toolIds [1 .. 10 :: Int] ]
        -- Reverse order completions
        let compEvents = [ ToolCompleted tid (Right (object ["result" .= (i * 100 :: Int)])) | (tid, i) <- reverse (zip toolIds [1 .. 10 :: Int]) ]
        let events =
              [ TurnStarted "turn-10x"
              , UserMsg "Run 10 parallel tools"
              , ModelTurn "Running 10 tools..." toolCalls
              ]
              ++ invEvents
              ++ compEvents
              ++
              [ ModelTurn "All 10 tools done." []
              , TurnFinished "turn-10x"
              ]

        case replayAudit events of
          Left err -> expectationFailure ("10x parallel replay failed: " ++ T.unpack err)
          Right summary -> do
            rsIsValidSequence summary `shouldBe` True
            rsToolInvocations summary `shouldBe` 10
            rsToolCompletions summary `shouldBe` 10

        let hist = reconstructChatHistory events
        -- Reconstructed: UserMsg, AssistantMsg (with 10 calls), 10 ToolMsgs (in completion order), AssistantMsg final
        length hist `shouldBe` 13
        case hist !! 1 of
          CoreTypes.AssistantMsg _ calls -> length calls `shouldBe` 10
          _ -> expectationFailure "Expected AssistantMsg with 10 calls"
