{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LLMonad.Batch3ChallengerSpec (spec) where

import Data.Aeson (
    FromJSON (..),
    Value (..),
    object,
    (.=),
 )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Vector qualified as V
import Effectful
import GHC.Generics (Generic)
import LLMonad
import LLMonad.Internal.Extract (decodeViaJSON, extractJSON)
import Test.Hspec

-- Types for rigorous fail-closed validation
data PersonRecord = PersonRecord
    { name :: Text
    , age :: Int
    , isMember :: Bool
    , tags :: [Text]
    }
    deriving (Show, Eq, Generic, FromJSON, ToSchema)

data NestedConfig = NestedConfig
    { serviceName :: Text
    , retryCount :: Int
    , extraDetails :: PersonRecord
    }
    deriving (Show, Eq, Generic, FromJSON, ToSchema)

data StructuredTarget = StructuredTarget
    { finalScore :: Int
    , outcomeSummary :: Text
    }
    deriving (Show, Eq, Generic, FromJSON, ToSchema)

lbs :: Text -> LBS.ByteString
lbs = LBS.fromStrict . encodeUtf8

personTool :: (Monad m) => Tool m
personTool = tool "person_tool" "Process person record" $ \(p :: PersonRecord) ->
    pure (object ["greeting" .= ("Hello " <> name p), "isAdult" .= (age p >= 18)])

nestedTool :: (Monad m) => Tool m
nestedTool = tool "nested_tool" "Process nested config" $ \(c :: NestedConfig) ->
    pure (object ["service" .= serviceName c, "targetPerson" .= name (extraDetails c)])

arrayTool :: (Monad m) => Tool m
arrayTool = tool "array_tool" "Process integer list" $ \(xs :: [Int]) ->
    pure (object ["totalSum" .= sum xs, "elementCount" .= length xs])

customErrorTool :: (Monad m) => Tool m
customErrorTool = tool' "custom_err_tool" "Tool with business rule errors" $ \(p :: PersonRecord) ->
    if age p < 0
        then pure (Left "Age cannot be negative")
        else pure (Right (object ["status" .= ("Accepted: " <> name p)]))

spec :: Spec
spec = do
    describe "Batch 3 Challenger Suite: Empirical Tool Argument Decoding Stress Tests" $ do
        it "rejects number when string is expected for record field without fallback" $ do
            let badVal =
                    object
                        [ "name" .= (12345 :: Int)
                        , "age" .= (30 :: Int)
                        , "isMember" .= True
                        , "tags" .= (["admin"] :: [Text])
                        ]
            res <- toolRun (personTool @IO) badVal
            case res of
                Left err -> do
                    err `shouldSatisfy` T.isPrefixOf "invalid arguments: "
                    err `shouldSatisfy` (\s -> "expected Text" `T.isInfixOf` s || "expected String" `T.isInfixOf` s || "parsing" `T.isInfixOf` s)
                Right _ -> expectationFailure "Expected fail-closed Left error on number-for-string field"

        it "rejects string when integer is expected for record field without fallback" $ do
            let badVal =
                    object
                        [ "name" .= ("Alice" :: Text)
                        , "age" .= ("thirty" :: Text)
                        , "isMember" .= True
                        , "tags" .= (["admin"] :: [Text])
                        ]
            res <- toolRun (personTool @IO) badVal
            case res of
                Left err -> err `shouldSatisfy` T.isPrefixOf "invalid arguments: "
                Right _ -> expectationFailure "Expected fail-closed Left error on string-for-int field"

        it "rejects string when boolean is expected for record field without fallback" $ do
            let badVal =
                    object
                        [ "name" .= ("Alice" :: Text)
                        , "age" .= (30 :: Int)
                        , "isMember" .= ("true" :: Text)
                        , "tags" .= (["admin"] :: [Text])
                        ]
            res <- toolRun (personTool @IO) badVal
            case res of
                Left err -> err `shouldSatisfy` T.isPrefixOf "invalid arguments: "
                Right _ -> expectationFailure "Expected fail-closed Left error on string-for-bool field"

        it "rejects missing required fields without fallback to empty default values" $ do
            let partialVal =
                    object
                        [ "name" .= ("Alice" :: Text)
                        , "age" .= (30 :: Int)
                        -- missing isMember and tags
                        ]
            res <- toolRun (personTool @IO) partialVal
            case res of
                Left err -> do
                    err `shouldSatisfy` T.isPrefixOf "invalid arguments: "
                    err `shouldSatisfy` (\s -> "key" `T.isInfixOf` s || "not found" `T.isInfixOf` s || "parsing" `T.isInfixOf` s)
                Right _ -> expectationFailure "Expected fail-closed Left error on missing required fields"

        it "rejects empty object {} when non-empty record is expected" $ do
            let emptyObj = object []
            res <- toolRun (personTool @IO) emptyObj
            case res of
                Left err -> err `shouldSatisfy` T.isPrefixOf "invalid arguments: "
                Right _ -> expectationFailure "Expected fail-closed Left error on empty object {}"

        it "rejects empty array [] when record is expected without silent [] fallback" $ do
            let emptyArr = Array mempty
            res <- toolRun (personTool @IO) emptyArr
            case res of
                Left err -> do
                    err `shouldSatisfy` T.isPrefixOf "invalid arguments: "
                    err `shouldSatisfy` (\s -> "expected Object" `T.isInfixOf` s || "encountered Array" `T.isInfixOf` s)
                Right _ -> expectationFailure "Expected fail-closed Left error on empty array [] passed to record tool"

        it "rejects non-empty array [1, 2, 3] when record is expected" $ do
            let arrVal = Array (V.fromList [Number 1, Number 2, Number 3])
            res <- toolRun (personTool @IO) arrVal
            case res of
                Left err -> err `shouldSatisfy` T.isPrefixOf "invalid arguments: "
                Right _ -> expectationFailure "Expected fail-closed Left error on non-empty array passed to record tool"

        it "rejects Null value when record is expected" $ do
            res <- toolRun (personTool @IO) Null
            case res of
                Left err -> do
                    err `shouldSatisfy` T.isPrefixOf "invalid arguments: "
                    err `shouldSatisfy` (\s -> "expected Object" `T.isInfixOf` s || "encountered Null" `T.isInfixOf` s)
                Right _ -> expectationFailure "Expected fail-closed Left error on Null passed to record tool"

        it "rejects object when array is expected without silent {} fallback" $ do
            let objVal = object ["totalSum" .= (100 :: Int)]
            res <- toolRun (arrayTool @IO) objVal
            case res of
                Left err -> do
                    err `shouldSatisfy` T.isPrefixOf "invalid arguments: "
                    err `shouldSatisfy` (\s -> "expected Array" `T.isInfixOf` s || "encountered Object" `T.isInfixOf` s)
                Right _ -> expectationFailure "Expected fail-closed Left error on object passed to array tool"

        it "rejects array containing incorrect element types" $ do
            let badArr = Array (V.fromList [Number 1, String "two", Number 3])
            res <- toolRun (arrayTool @IO) badArr
            case res of
                Left err -> err `shouldSatisfy` T.isPrefixOf "invalid arguments: "
                Right _ -> expectationFailure "Expected fail-closed Left error on mixed-type array"

        it "rejects deeply nested record type mismatches" $ do
            let badNested =
                    object
                        [ "serviceName" .= ("AuthService" :: Text)
                        , "retryCount" .= (3 :: Int)
                        , "extraDetails"
                            .= object
                                [ "name" .= ("Bob" :: Text)
                                , "age" .= ("invalid-age-string" :: Text)
                                , "isMember" .= False
                                , "tags" .= (["user"] :: [Text])
                                ]
                        ]
            res <- toolRun (nestedTool @IO) badNested
            case res of
                Left err -> err `shouldSatisfy` T.isPrefixOf "invalid arguments: "
                Right _ -> expectationFailure "Expected fail-closed Left error on nested record type mismatch"

        it "successfully parses and runs valid record inputs" $ do
            let goodVal =
                    object
                        [ "name" .= ("Charlie" :: Text)
                        , "age" .= (25 :: Int)
                        , "isMember" .= True
                        , "tags" .= (["developer", "haskell"] :: [Text])
                        ]
            res <- toolRun (personTool @IO) goodVal
            case res of
                Right (Object o) -> do
                    KM.lookup (Key.fromText "greeting") o `shouldBe` Just (String "Hello Charlie")
                    KM.lookup (Key.fromText "isAdult") o `shouldBe` Just (Bool True)
                other -> expectationFailure ("Expected Right object, got: " <> show other)

        it "preserves custom domain error messages without invalid-arguments wrapper" $ do
            let invalidDomainVal =
                    object
                        [ "name" .= ("Dave" :: Text)
                        , "age" .= (-5 :: Int)
                        , "isMember" .= False
                        , "tags" .= ([] :: [Text])
                        ]
            res <- toolRun (customErrorTool @IO) invalidDomainVal
            res `shouldBe` Left "Age cannot be negative"

    describe "Batch 3 Challenger Suite: Message Ordering On Repeated Tool Signatures" $ do
        it "ensures ToolMsg strictly precedes warning UserMsg across multiple repeat cycles in runTextLoopWith" $ do
            let callVal =
                    object
                        [ "name" .= ("Repeater" :: Text)
                        , "age" .= (20 :: Int)
                        , "isMember" .= True
                        , "tags" .= ([] :: [Text])
                        ]
                toolCall = Right (toolResp [ToolCall "tc_1" "person_tool" callVal])
                script =
                    [ toolCall -- round 1: initial call
                    , toolCall -- round 2: identical call (triggers warning)
                    , toolCall -- round 3: identical call again (triggers warning)
                    , Right (textResp "Settled final answer.")
                    ]
                opts = defaultAgentOpts{agentMaxRounds = 6}
            (res, _, hist, _) <- runEff $ runLLMMockFull script (runTextLoopWith opts [personTool] "Run repeated task")
            res `shouldBe` "Settled final answer."

            -- History verification:
            -- Check that in each round where a warning occurs, the ToolMsg for that round precedes the UserMsg warning.
            let indexedMsgs = zip [0 :: Int ..] hist
                toolMsgs = [(i, mid, content) | (i, ToolMsg mid content) <- indexedMsgs]
                warnMsgs = [(i, txt) | (i, UserMsg txt) <- indexedMsgs, "Repeated identical tool call" `T.isInfixOf` txt]

            length toolMsgs `shouldBe` 3
            length warnMsgs `shouldBe` 2

            -- Verify ordering:
            -- Round 1 tool call -> index of tool msg 1
            -- Round 2 tool call -> index of tool msg 2 < index of warning 1
            -- Round 3 tool call -> index of tool msg 3 < index of warning 2
            let toolIdx1 = (\(i, _, _) -> i) (toolMsgs !! 0)
                toolIdx2 = (\(i, _, _) -> i) (toolMsgs !! 1)
                toolIdx3 = (\(i, _, _) -> i) (toolMsgs !! 2)
                warnIdx1 = fst (warnMsgs !! 0)
                warnIdx2 = fst (warnMsgs !! 1)

            toolIdx1 `shouldSatisfy` (< toolIdx2)
            toolIdx2 `shouldSatisfy` (< warnIdx1)
            warnIdx1 `shouldSatisfy` (< toolIdx3)
            toolIdx3 `shouldSatisfy` (< warnIdx2)

        it "ensures ALL parallel ToolMsgs strictly precede warning UserMsg in runTextLoopWith" $ do
            let callVal1 = object ["name" .= ("P1" :: Text), "age" .= (21 :: Int), "isMember" .= True, "tags" .= ([] :: [Text])]
                callVal2 = object ["name" .= ("P2" :: Text), "age" .= (22 :: Int), "isMember" .= True, "tags" .= ([] :: [Text])]
                callVal3 = object ["name" .= ("P3" :: Text), "age" .= (23 :: Int), "isMember" .= True, "tags" .= ([] :: [Text])]
                parallelCalls =
                    Right
                        ( toolResp
                            [ ToolCall "call-p1" "person_tool" callVal1
                            , ToolCall "call-p2" "person_tool" callVal2
                            , ToolCall "call-p3" "person_tool" callVal3
                            ]
                        )
                script =
                    [ parallelCalls -- round 1
                    , parallelCalls -- round 2 (repeat)
                    , Right (textResp "Parallel execution complete.")
                    ]
                opts = defaultAgentOpts{agentMaxRounds = 5}
            (res, _, hist, _) <- runEff $ runLLMMockFull script (runTextLoopWith opts [personTool] "Execute parallel tasks")
            res `shouldBe` "Parallel execution complete."

            let indexedMsgs = zip [0 :: Int ..] hist
                toolMsgs = [(i, mid) | (i, ToolMsg mid _) <- indexedMsgs]
                warnMsgs = [(i, txt) | (i, UserMsg txt) <- indexedMsgs, "Repeated identical tool call" `T.isInfixOf` txt]

            length toolMsgs `shouldBe` 6 -- 3 calls in round 1 + 3 calls in round 2
            length warnMsgs `shouldBe` 1

            let round2ToolIndices = map fst (drop 3 toolMsgs)
            case warnMsgs of
                ((warnIdx, _) : _) -> all (< warnIdx) round2ToolIndices `shouldBe` True
                [] -> expectationFailure "Expected warning user message"

        it "ensures ToolMsg precedes cycle warning in runStructuredLoop multi-round flow" $ do
            let callVal = object ["name" .= ("StructuredWorker" :: Text), "age" .= (30 :: Int), "isMember" .= True, "tags" .= ([] :: [Text])]
                repeatCall = Right (toolResp [ToolCall "sc_1" "person_tool" callVal])
                finalAnswer = Right (structuredResp (object ["finalScore" .= (100 :: Int), "outcomeSummary" .= ("Completed successfully" :: Text)]))
                script =
                    [ repeatCall -- round 1
                    , repeatCall -- round 2 (repeated call with warning)
                    , finalAnswer -- round 3 (final structured output)
                    ]
                opts = defaultAgentOpts{agentMaxRounds = 5}
            (res, _, hist, _) <- runEff $ runLLMMockFull script (runStructuredLoopWith @StructuredTarget opts [personTool] "Compute structured target")
            res `shouldBe` StructuredTarget 100 "Completed successfully"

            let indexedMsgs = zip [0 :: Int ..] hist
                toolMsgs = [(i, mid) | (i, ToolMsg mid _) <- indexedMsgs]
                warnMsgs = [(i, txt) | (i, UserMsg txt) <- indexedMsgs, "Repeated identical tool call" `T.isInfixOf` txt]

            length toolMsgs `shouldBe` 2
            length warnMsgs `shouldBe` 1

            case (toolMsgs, warnMsgs) of
                ((_, _) : (toolIdx2, _) : _, (warnIdx, _) : _) -> toolIdx2 `shouldSatisfy` (< warnIdx)
                _ -> expectationFailure "Expected 2 tool messages and 1 warning message"

    describe "Batch 3 Challenger Suite: Delimiter-Stack JSON Extraction Stress Tests" $ do
        it "handles arbitrary nested structures: objects in arrays in objects in arrays" $ do
            let raw = "Some prefix text {\"alpha\": [1, 2, {\"beta\": [3, 4, {\"gamma\": [5, 6, {\"delta\": \"deep\"}]}]}], \"status\": \"ok\"} trailing notes"
            case extractJSON raw of
                Right (Object o) -> do
                    KM.lookup (Key.fromText "status") o `shouldBe` Just (String "ok")
                other -> expectationFailure ("Expected Right object, got: " <> show other)

        it "handles strings containing brackets, braces, colons, and escaped quotes" $ do
            let raw = "Result: {\"brackets\": \"[foo]\", \"braces\": \"{bar}\", \"escaped\": \"\\\"[nested bracket]\\\"\", \"code\": 42}"
            case extractJSON raw of
                Right (Object o) -> do
                    KM.lookup (Key.fromText "brackets") o `shouldBe` Just (String "[foo]")
                    KM.lookup (Key.fromText "braces") o `shouldBe` Just (String "{bar}")
                    KM.lookup (Key.fromText "code") o `shouldBe` Just (Number 42)
                other -> expectationFailure ("Expected Right object, got: " <> show other)

        it "handles escaped backslashes preceding string delimiters correctly" $ do
            let raw = "{\"path\": \"C:\\\\Windows\\\\System32\\\\\", \"valid\": true}"
            case extractJSON raw of
                Right (Object o) -> do
                    KM.lookup (Key.fromText "valid") o `shouldBe` Just (Bool True)
                other -> expectationFailure ("Expected Right object, got: " <> show other)

        it "handles markdown code fences with complex content" $ do
            let raw = "```json\n{\n  \"items\": [\"one\", \"two\", \"three\"],\n  \"nested\": {\"flag\": true}\n}\n```"
            case extractJSON raw of
                Right (Object o) -> do
                    KM.lookup (Key.fromText "items") o `shouldBe` Just (Array (V.fromList [String "one", String "two", String "three"]))
                other -> expectationFailure ("Expected Right object, got: " <> show other)

        it "rejects mismatched delimiters at root level: [1, 2, 3}" $ do
            case extractJSON "[1, 2, 3}" of
                Left err -> err `shouldSatisfy` (\s -> "unbalanced" `T.isInfixOf` T.pack s || "no JSON" `T.isInfixOf` T.pack s)
                Right v -> expectationFailure ("Parsed invalid mismatched JSON: " <> show v)

        it "rejects mismatched delimiters at root level: {\"key\": 123]" $ do
            case extractJSON "{\"key\": 123]" of
                Left err -> err `shouldSatisfy` (\s -> "unbalanced" `T.isInfixOf` T.pack s || "no JSON" `T.isInfixOf` T.pack s)
                Right v -> expectationFailure ("Parsed invalid mismatched JSON: " <> show v)

        it "rejects nested bracket mismatches: {\"k\": [1, 2}}" $ do
            case extractJSON "{\"k\": [1, 2}}" of
                Left _ -> pure ()
                Right v -> expectationFailure ("Parsed invalid nested mismatched JSON: " <> show v)

        it "rejects nested brace mismatches: [{\"k\": 1]" $ do
            case extractJSON "[{\"k\": 1]" of
                Left _ -> pure ()
                Right v -> expectationFailure ("Parsed invalid nested mismatched JSON: " <> show v)

        it "rejects interleaved delimiters: [ { ] }" $ do
            case extractJSON "[ { ] }" of
                Left _ -> pure ()
                Right v -> expectationFailure ("Parsed invalid interleaved delimiters: " <> show v)

        it "rejects deeply nested mismatched delimiter: {\"a\": [{\"b\": [{\"c\": 1}]}}" $ do
            -- Missing one closing bracket before final brace: [ { [ } ] }
            case extractJSON "{\"a\": [{\"b\": [{\"c\": 1}]}}" of
                Left _ -> pure ()
                Right v -> expectationFailure ("Parsed invalid deeply nested mismatched JSON: " <> show v)

        it "rejects unclosed truncated JSON: {\"a\": [1, 2, 3" $ do
            case extractJSON "{\"a\": [1, 2, 3" of
                Left err -> err `shouldSatisfy` (\s -> "unbalanced" `T.isInfixOf` T.pack s || "no JSON" `T.isInfixOf` T.pack s)
                Right v -> expectationFailure ("Parsed truncated JSON: " <> show v)

        it "decodes via JSON with schema parsing into strong Haskell types" $ do
            let raw = "Here is the record: {\"name\": \"Haskell User\", \"age\": 35, \"isMember\": true, \"tags\": [\"fp\", \"ghc\"]} hope you enjoy!"
            case decodeViaJSON @PersonRecord raw of
                Right p -> do
                    name p `shouldBe` "Haskell User"
                    age p `shouldBe` 35
                    isMember p `shouldBe` True
                    tags p `shouldBe` ["fp", "ghc"]
                Left err -> expectationFailure ("Failed to decode valid person record: " <> err)

    describe "Batch 3 Challenger Suite: Anthropic Synthetic Schema Tool Disentanglement" $ do
        it "excludes synthetic schema tool and parses real structured payload in non-streaming response" $ do
            let raw =
                    "{\"role\":\"assistant\",\"content\":[{\"type\":\"tool_use\",\"id\":\"call_syn_1\",\"name\":\"__llmonad_structured_output\",\"input\":{\"finalScore\":98,\"outcomeSummary\":\"All tests passed\"}}],\"stop_reason\":\"tool_use\"}"
            case parseMessagesResponse (lbs raw) of
                Right resp -> do
                    crspToolCalls resp `shouldBe` []
                    crspStructuredPayload resp `shouldBe` Just (object [Key.fromText "finalScore" .= (98 :: Int), Key.fromText "outcomeSummary" .= ("All tests passed" :: Text)])
                    crspFinishReason resp `shouldBe` FrStop
                Left e -> expectationFailure (show e)

        it "preserves user tool calls without synthetic payload interference" $ do
            let raw =
                    "{\"role\":\"assistant\",\"content\":[{\"type\":\"tool_use\",\"id\":\"call_user_1\",\"name\":\"person_tool\",\"input\":{\"name\":\"Sam\",\"age\":28,\"isMember\":true,\"tags\":[]}}],\"stop_reason\":\"tool_use\"}"
            case parseMessagesResponse (lbs raw) of
                Right resp -> case crspToolCalls resp of
                    [tc] -> do
                        toolCallName tc `shouldBe` "person_tool"
                        crspStructuredPayload resp `shouldBe` Nothing
                        crspFinishReason resp `shouldBe` FrToolUse
                    otherCalls -> expectationFailure ("Expected 1 tool call, got: " <> show otherCalls)
                Left e -> expectationFailure (show e)

        it "handles streaming synthetic tool events cleanly" $ do
            let events =
                    [ "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"tu_synth_stream\",\"name\":\"__llmonad_structured_output\"}}"
                    , "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"finalScore\\\": 88, \"}}"
                    , "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"\\\"outcomeSummary\\\": \\\"Stream verified\\\"}\"}}"
                    , "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\"}}"
                    , "{\"type\":\"message_stop\"}"
                    ]
                st = foldl (\s e -> fst (handleAnthropicEvent s e)) initialAntStreamState events
            case finalizeAnthropicStream st of
                Right resp -> do
                    crspToolCalls resp `shouldBe` []
                    crspStructuredPayload resp `shouldBe` Just (object [Key.fromText "finalScore" .= (88 :: Int), Key.fromText "outcomeSummary" .= ("Stream verified" :: Text)])
                    crspFinishReason resp `shouldBe` FrStop
                Left e -> expectationFailure (show e)
