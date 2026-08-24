{-# LANGUAGE OverloadedStrings #-}

module LLMonad.AnthropicSpec (spec) where

import Data.Aeson (
    Value (..),
    object,
    (.=),
 )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector qualified as V
import LLMonad.Error (LLMError (..))
import LLMonad.Providers.Anthropic
import LLMonad.Types
import Test.Hspec

lookupV :: Text -> Value -> Maybe Value
lookupV k (Object o) = KM.lookup (Key.fromText k) o
lookupV _ _ = Nothing

at :: [Text] -> Value -> Maybe Value
at [] v = Just v
at (k : ks) v = lookupV k v >>= at ks

lbs :: Text -> ByteString
lbs = LBS.fromStrict . encodeUtf8

sampleSchema :: Value
sampleSchema =
    object
        [ "type" .= ("object" :: Text)
        , "properties" .= object [Key.fromText "q" .= object ["type" .= ("string" :: Text)]]
        , "required" .= ["q" :: Text]
        ]

schemaReq :: CompletionRequest
schemaReq =
    CompletionRequest
        { crModel = "test-model"
        , crSystem = Just "be helpful"
        , crMessages = [UserMsg "hi"]
        , crParams = defaultParams
        , crTools = []
        , crToolChoice = ToolAuto
        , crResponseFormat = RfJsonSchema "Answer" sampleSchema True
        }

cfg :: AnthropicConfig
cfg = defaultAnthropicConfig "sk-test"

spec :: Spec
spec = do
    describe "buildMessagesBody" $ do
        it "sends the system prompt top-level and requires max_tokens" $ do
            let body = buildMessagesBody cfg schemaReq
            lookupV "system" body `shouldBe` Just (String "be helpful")
            lookupV "max_tokens" body `shouldSatisfy` (\v -> case v of Just (Number _) -> True; _ -> False)

        it "forces the structured-output tool for json_schema requests" $ do
            let body = buildMessagesBody cfg schemaReq
            at ["tool_choice", "type"] body `shouldBe` Just (String "tool")
            at ["tool_choice", "name"] body `shouldBe` Just (String "__llmonad_structured_output")
            case at ["tools"] body of
                Just (Array ts) -> length ts `shouldBe` 1
                other -> expectationFailure ("expected one tool, got: " <> show other)

        it "groups consecutive tool results into one user message" $ do
            let req =
                    schemaReq
                        { crResponseFormat = RfText
                        , crMessages =
                            [ AssistantMsg "" [ToolCall "t1" "calc" (object ["a" .= (1 :: Int)])]
                            , ToolMsg "t1" "one"
                            , ToolMsg "t2" "two"
                            ]
                        }
                body = buildMessagesBody cfg req
            case at ["messages"] body of
                Just (Array msgs) -> do
                    length msgs `shouldBe` 2 -- assistant(tool_use), user([tool_result, tool_result])
                    let toolResultMsg = msgs V.! 1
                    case lookupV "content" toolResultMsg of
                        Just (Array blocks) -> length blocks `shouldBe` 2
                        other -> expectationFailure ("expected content blocks array, got: " <> show other)
                    lookupV "role" toolResultMsg `shouldBe` Just (String "user")
                other -> expectationFailure ("expected messages array, got: " <> show other)

        it "maps stop sequences to stop_sequences" $ do
            let req = schemaReq{crResponseFormat = RfText, crParams = defaultParams{paramStopSequences = ["END"]}}
                body = buildMessagesBody cfg req
            lookupV "stop_sequences" body `shouldBe` Just (Array (V.fromList [String "END"]))

    describe "parseMessagesResponse" $ do
        it "parses text responses" $ do
            let raw = "{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"hello\"}],\"stop_reason\":\"end_turn\",\"usage\":{\"input_tokens\":3,\"output_tokens\":4}}"
            case parseMessagesResponse (lbs raw) of
                Right resp -> do
                    crspText resp `shouldBe` "hello"
                    crspFinishReason resp `shouldBe` FrStop
                    crspUsage resp `shouldBe` Just (Usage 3 4)
                Left e -> expectationFailure (show e)

        it "surfaces synthetic tool_use input as the structured payload and isolates from crspToolCalls" $ do
            let raw = "{\"role\":\"assistant\",\"content\":[{\"type\":\"tool_use\",\"id\":\"tu_1\",\"name\":\"__llmonad_structured_output\",\"input\":{\"q\":\"hi\"}}],\"stop_reason\":\"tool_use\"}"
            case parseMessagesResponse (lbs raw) of
                Right resp -> do
                    crspToolCalls resp `shouldBe` []
                    crspStructuredPayload resp `shouldBe` Just (object [(Key.fromText "q", String "hi")])
                    crspFinishReason resp `shouldBe` FrStop
                Left e -> expectationFailure (show e)

        it "preserves user tool calls in crspToolCalls and leaves structuredPayload as Nothing" $ do
            let raw = "{\"role\":\"assistant\",\"content\":[{\"type\":\"tool_use\",\"id\":\"tu_2\",\"name\":\"calculator\",\"input\":{\"a\":1}}],\"stop_reason\":\"tool_use\"}"
            case parseMessagesResponse (lbs raw) of
                Right resp -> do
                    crspToolCalls resp `shouldBe` [ToolCall "tu_2" "calculator" (object [(Key.fromText "a", Number 1)])]
                    crspStructuredPayload resp `shouldBe` Nothing
                    crspFinishReason resp `shouldBe` FrToolUse
                Left e -> expectationFailure (show e)

    describe "streaming state machine" $ do
        it "assembles text deltas into a final response" $ do
            let events =
                    [ "{\"type\":\"message_start\",\"message\":{\"usage\":{\"input_tokens\":7}}}"
                    , "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\"}}"
                    , "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"Hel\"}}"
                    , "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"lo\"}}"
                    , "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":9}}"
                    , "{\"type\":\"message_stop\"}"
                    ]
                (st, events') = foldl step (initialAntStreamState, []) events
                step (s, acc) e =
                    let (s', evts) = handleAnthropicEvent s e
                     in (s', acc ++ evts)
            map streamEventText events' `shouldBe` ["Hel", "lo"]
            case finalizeAnt st of
                Right resp -> do
                    crspText resp `shouldBe` "Hello"
                    crspUsage resp `shouldBe` Just (Usage 7 9)
                    crspFinishReason resp `shouldBe` FrStop
                Left e -> expectationFailure (show e)

        it "reassembles streamed synthetic tool arguments from partial JSON without polluting crspToolCalls" $ do
            let events =
                    [ "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"tu_1\",\"name\":\"__llmonad_structured_output\"}}"
                    , "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"q\\\": \"}}"
                    , "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"\\\"hey\\\"}\"}}"
                    , "{\"type\":\"message_stop\"}"
                    ]
                st = foldl (\s e -> fst (handleAnthropicEvent s e)) initialAntStreamState events
            case finalizeAnt st of
                Right resp -> do
                    crspToolCalls resp `shouldBe` []
                    crspStructuredPayload resp `shouldBe` Just (object [(Key.fromText "q", String "hey")])
                    crspFinishReason resp `shouldBe` FrStop
                Left e -> expectationFailure (show e)

        it "reassembles streamed user tool arguments in crspToolCalls" $ do
            let events =
                    [ "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"tu_1\",\"name\":\"calc\"}}"
                    , "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"q\\\": \"}}"
                    , "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"\\\"hey\\\"}\"}}"
                    , "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\"}}"
                    , "{\"type\":\"message_stop\"}"
                    ]
                st = foldl (\s e -> fst (handleAnthropicEvent s e)) initialAntStreamState events
            case finalizeAnt st of
                Right resp -> do
                    crspToolCalls resp `shouldBe` [ToolCall "tu_1" "calc" (object [(Key.fromText "q", String "hey")])]
                    crspStructuredPayload resp `shouldBe` Nothing
                    crspFinishReason resp `shouldBe` FrToolUse
                Left e -> expectationFailure (show e)

        it "rejects truncated stream ending abruptly without message_stop or stop_reason" $ do
            let events =
                    [ "{\"type\":\"message_start\",\"message\":{\"usage\":{\"input_tokens\":5}}}"
                    , "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\"}}"
                    , "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"Incomplete\"}}"
                    ]
                st = foldl (\s e -> fst (handleAnthropicEvent s e)) initialAntStreamState events
            case finalizeAnt st of
                Left (HttpError _) -> pure ()
                other -> expectationFailure ("expected HttpError on truncated stream, got: " <> show other)

        it "rejects empty stream without events" $ do
            case finalizeAnt initialAntStreamState of
                Left (HttpError _) -> pure ()
                other -> expectationFailure ("expected HttpError on empty stream, got: " <> show other)

        it "verifies provider stream payload includes stream: true" $ do
            let baseBody = buildMessagesBody cfg schemaReq
                streamBody = case baseBody of
                    Object o -> Object (KM.insert "stream" (Bool True) o)
                    v -> v
            lookupV "stream" streamBody `shouldBe` Just (Bool True)
  where
    finalizeAnt = finalizeAnthropicStream
    streamEventText (SEText t) = t
    streamEventText _ = ""
