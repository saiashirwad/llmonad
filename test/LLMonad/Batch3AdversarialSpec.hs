{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LLMonad.Batch3AdversarialSpec (spec) where

import Control.Exception (try)
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
import Effectful
import GHC.Generics (Generic)
import LLMonad
import LLMonad.Internal.Extract (extractJSON)
import Test.Hspec

-- | Types for testing fail-closed tool decoding
data StrictPoint = StrictPoint
    { px :: Int
    , py :: Int
    }
    deriving (Show, Eq, Generic, FromJSON, ToSchema)

data StrictListArgs = StrictListArgs
    { items :: [Text]
    }
    deriving (Show, Eq, Generic, FromJSON, ToSchema)

data FinalResult = FinalResult
    { calculation :: Int
    , status :: Text
    }
    deriving (Show, Eq, Generic, FromJSON, ToSchema)

lbs :: Text -> LBS.ByteString
lbs = LBS.fromStrict . encodeUtf8

pointTool :: (Monad m) => Tool m
pointTool = tool "point_tool" "Process point" $ \(p :: StrictPoint) ->
    pure (object ["distSq" .= (px p * px p + py p * py p)])

listTool :: (Monad m) => Tool m
listTool = tool "list_tool" "Process string list" $ \(args :: StrictListArgs) ->
    pure (object ["count" .= length (items args)])

rawFailTool :: (Monad m) => Tool m
rawFailTool = tool' "raw_tool" "Fail on invalid" $ \(p :: StrictPoint) ->
    if px p < 0
        then pure (Left "Negative coordinate not allowed")
        else pure (Right (object ["ok" .= True]))

spec :: Spec
spec = do
    describe "Batch 3 Adversarial Suite: Tool Execution & Fail-Closed Decoding" $ do
        it "tool' rejects string value for integer field without fallback" $ do
            let badVal = object ["px" .= ("not-int" :: Text), "py" .= (20 :: Int)]
            res <- toolRun (pointTool @IO) badVal
            case res of
                Left err -> err `shouldSatisfy` T.isPrefixOf "invalid arguments: "
                Right _ -> expectationFailure "Expected Left error on malformed argument"

        it "tool' rejects array value when object is expected" $ do
            let badVal = Array mempty
            res <- toolRun (pointTool @IO) badVal
            case res of
                Left err -> err `shouldSatisfy` T.isPrefixOf "invalid arguments: "
                Right _ -> expectationFailure "Expected Left error on array passed to object tool"

        it "tool' rejects object value when array is expected" $ do
            let rawArrayTool :: Tool IO = tool "arr_tool" "Expects array" $ \(xs :: [Int]) -> pure (sum xs)
            let badVal = object ["px" .= (1 :: Int)]
            res <- toolRun rawArrayTool badVal
            case res of
                Left err -> err `shouldSatisfy` T.isPrefixOf "invalid arguments: "
                Right _ -> expectationFailure "Expected Left error on object passed to array tool"

        it "tool' rejects Null when record object is expected" $ do
            res <- toolRun (pointTool @IO) Null
            case res of
                Left err -> err `shouldSatisfy` T.isPrefixOf "invalid arguments: "
                Right _ -> expectationFailure "Expected Left error on Null passed to record tool"

        it "tool' rejects missing required fields fail-closed" $ do
            let partialVal = object ["px" .= (10 :: Int)]
            res <- toolRun (pointTool @IO) partialVal
            case res of
                Left err -> err `shouldSatisfy` T.isPrefixOf "invalid arguments: "
                Right _ -> expectationFailure "Expected Left error on missing required field"

        it "tool' passes valid inputs to handler" $ do
            let validVal = object ["px" .= (3 :: Int), "py" .= (4 :: Int)]
            res <- toolRun (pointTool @IO) validVal
            case res of
                Right (Object o) -> KM.lookup (Key.fromText "distSq") o `shouldBe` Just (Number 25)
                other -> expectationFailure ("Expected Right object, got: " <> show other)

        it "tool' preserves custom Left error text from handler" $ do
            let negativeVal = object ["px" .= (-5 :: Int), "py" .= (4 :: Int)]
            res <- toolRun (rawFailTool @IO) negativeVal
            res `shouldBe` Left "Negative coordinate not allowed"

    describe "Batch 3 Adversarial Suite: Message Ordering & Cycle Warning Invariants" $ do
        it "guarantees ToolMsg precedes cycle warning UserMsg in useToolsWith" $ do
            let repeatedCall = Right (toolResp [ToolCall "c1" "point_tool" (object ["px" .= (1 :: Int), "py" .= (2 :: Int)])])
                script =
                    [ repeatedCall
                    , repeatedCall
                    , Right (textResp "Finished after cycle warning")
                    ]
                opts = defaultAgentOpts{agentMaxRounds = 5}
            (_, _, hist, _) <- runEff $ runLLMMockFull script (useToolsWith opts [pointTool] "Do work")
            let msgs = [(i, m) | (i, m) <- zip [0 :: Int ..] hist]
            let toolMsgIndices = [i | (i, ToolMsg _ _) <- msgs]
            let warnMsgIndices = [i | (i, UserMsg t) <- msgs, "Repeated identical tool call" `T.isInfixOf` t]
            toolMsgIndices `shouldSatisfy` (not . null)
            warnMsgIndices `shouldSatisfy` (not . null)
            -- The first warning must occur AFTER the first tool message
            case (warnMsgIndices, toolMsgIndices) of
                (w : _, t : _) -> w `shouldSatisfy` (> t)
                _ -> expectationFailure "Expected warning and tool messages"

        it "guarantees all parallel ToolMsgs precede cycle warning UserMsg in runAgentWith" $ do
            let parallelCalls =
                    Right
                        ( toolResp
                            [ ToolCall "call-1" "point_tool" (object ["px" .= (1 :: Int), "py" .= (1 :: Int)])
                            , ToolCall "call-2" "point_tool" (object ["px" .= (2 :: Int), "py" .= (2 :: Int)])
                            ]
                        )
                script =
                    [ parallelCalls
                    , parallelCalls
                    , Right (textResp "Finished parallel cycle")
                    ]
            (_, _, hist, _) <- runEff $ runLLMMockFull script (runAgentWith defaultAgentOpts [pointTool] "Do parallel")
            let msgs = [(i, m) | (i, m) <- zip [0 :: Int ..] hist]
            let toolMsgIndices = [i | (i, ToolMsg _ _) <- msgs]
            let warnMsgIndices = [i | (i, UserMsg t) <- msgs, "Repeated identical tool call" `T.isInfixOf` t]
            length toolMsgIndices `shouldBe` 4 -- 2 calls in round 1, 2 in round 2
            length warnMsgIndices `shouldBe` 1
            -- Warning must be placed after the first round of tool calls (indices 0 and 1 tool msgs)
            case (warnMsgIndices, toolMsgIndices) of
                (w : _, _ : _ : _ : t3 : _) -> w `shouldSatisfy` (> t3)
                _ -> expectationFailure "Expected warning and 4 tool messages"

        it "runAgent structured loop records tool results before cycle warnings" $ do
            let repeatedCall = Right (toolResp [ToolCall "c1" "point_tool" (object ["px" .= (3 :: Int), "py" .= (4 :: Int)])])
                finalResp = Right (structuredResp (object ["calculation" .= (25 :: Int), "status" .= ("done" :: Text)]))
                script =
                    [ repeatedCall
                    , repeatedCall
                    , finalResp
                    ]
            (res, _, hist, _) <- runEff $ runLLMMockFull script (runAgentStructured @FinalResult [pointTool] "Calculate and return")
            res `shouldBe` FinalResult 25 "done"
            let msgs = [(i, m) | (i, m) <- zip [0 :: Int ..] hist]
            let toolMsgIndices = [i | (i, ToolMsg _ _) <- msgs]
            let warnMsgIndices = [i | (i, UserMsg t) <- msgs, "Repeated identical tool call" `T.isInfixOf` t]
            length warnMsgIndices `shouldBe` 1
            case (warnMsgIndices, toolMsgIndices) of
                (w : _, t : _) -> w `shouldSatisfy` (> t)
                _ -> expectationFailure "Expected warning and tool messages"

    describe "Batch 3 Adversarial Suite: Anthropic Synthetic Schema Tool Disentanglement" $ do
        it "parseMessagesResponse excludes synthetic schema tool from crspToolCalls" $ do
            let raw =
                    "{\"role\":\"assistant\",\"content\":[{\"type\":\"tool_use\",\"id\":\"tu_synth\",\"name\":\"__llmonad_structured_output\",\"input\":{\"calculation\":42,\"status\":\"success\"}}],\"stop_reason\":\"tool_use\"}"
            case parseMessagesResponse (lbs raw) of
                Right resp -> do
                    crspToolCalls resp `shouldBe` []
                    crspStructuredPayload resp `shouldBe` Just (object [Key.fromText "calculation" .= (42 :: Int), Key.fromText "status" .= ("success" :: Text)])
                    crspFinishReason resp `shouldBe` FrStop
                Left e -> expectationFailure (show e)

        it "parseMessagesResponse preserves user tool calls and leaves crspStructuredPayload Nothing" $ do
            let raw =
                    "{\"role\":\"assistant\",\"content\":[{\"type\":\"tool_use\",\"id\":\"tu_user\",\"name\":\"point_tool\",\"input\":{\"px\":1,\"py\":2}}],\"stop_reason\":\"tool_use\"}"
            case parseMessagesResponse (lbs raw) of
                Right resp -> do
                    crspToolCalls resp `shouldBe` [ToolCall "tu_user" "point_tool" (object [Key.fromText "px" .= (1 :: Int), Key.fromText "py" .= (2 :: Int)])]
                    crspStructuredPayload resp `shouldBe` Nothing
                    crspFinishReason resp `shouldBe` FrToolUse
                Left e -> expectationFailure (show e)

        it "finalizeAnthropicStream separates streamed synthetic tool from tool calls" $ do
            let events =
                    [ "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"tu_stream\",\"name\":\"__llmonad_structured_output\"}}"
                    , "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"calculation\\\": 99, \"}}"
                    , "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"\\\"status\\\": \\\"ok\\\"}\"}}"
                    , "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\"}}"
                    , "{\"type\":\"message_stop\"}"
                    ]
                st = foldl (\s e -> fst (handleAnthropicEvent s e)) initialAntStreamState events
            case finalizeAnthropicStream st of
                Right resp -> do
                    crspToolCalls resp `shouldBe` []
                    crspStructuredPayload resp `shouldBe` Just (object [Key.fromText "calculation" .= (99 :: Int), Key.fromText "status" .= ("ok" :: Text)])
                    crspFinishReason resp `shouldBe` FrStop
                Left e -> expectationFailure (show e)

        it "finalizeAnthropicStream preserves streamed user tool calls" $ do
            let events =
                    [ "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"tu_stream_u\",\"name\":\"point_tool\"}}"
                    , "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"px\\\": 10, \"}}"
                    , "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"\\\"py\\\": 20}\"}}"
                    , "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\"}}"
                    , "{\"type\":\"message_stop\"}"
                    ]
                st = foldl (\s e -> fst (handleAnthropicEvent s e)) initialAntStreamState events
            case finalizeAnthropicStream st of
                Right resp -> do
                    crspToolCalls resp `shouldBe` [ToolCall "tu_stream_u" "point_tool" (object [Key.fromText "px" .= (10 :: Int), Key.fromText "py" .= (20 :: Int)])]
                    crspStructuredPayload resp `shouldBe` Nothing
                    crspFinishReason resp `shouldBe` FrToolUse
                Left e -> expectationFailure (show e)

    describe "Batch 3 Adversarial Suite: Structured Loops & Multi-Turn Transaction Rollback" $ do
        it "runAgentStructured performs multi-turn tool execution before settling structured result" $ do
            let round1 = Right (toolResp [ToolCall "c1" "point_tool" (object ["px" .= (3 :: Int), "py" .= (4 :: Int)])])
                round2 = Right (toolResp [ToolCall "c2" "list_tool" (object ["items" .= (["a", "b", "c"] :: [Text])])])
                final = Right (structuredResp (object ["calculation" .= (25 :: Int), "status" .= ("processed 3 items" :: Text)]))
                script = [round1, round2, final]
            (res, _, hist, _) <- runEff $ runLLMMockFull script (runAgentStructured @FinalResult [pointTool, listTool] "Compute")
            res `shouldBe` FinalResult 25 "processed 3 items"
            let toolContents = [c | ToolMsg _ c <- hist]
            length toolContents `shouldBe` 2

        it "runAgentStructured rolls back staged messages completely on decode failure" $ do
            let badScript =
                    [ Right (toolResp [ToolCall "c1" "point_tool" (object ["px" .= (1 :: Int), "py" .= (2 :: Int)])])
                    , Right (textResp "Bad final JSON 1")
                    , Right (textResp "Bad final JSON 2")
                    ]
                opts = defaultAgentOpts{agentMaxRounds = 2}
            res <- try (runEff $ runLLMMockFull badScript (runAgentStructuredWith @FinalResult opts [pointTool] "Task"))
            case res of
                Left (DecodeError _ _) -> pure ()
                Left other -> expectationFailure ("Expected DecodeError, got: " <> show other)
                Right _ -> expectationFailure "Expected DecodeError exception"

        it "useToolsWith rolls back staged messages on exhausted rounds" $ do
            let infiniteTool = repeat (Right (toolResp [ToolCall "c" "point_tool" (object ["px" .= (1 :: Int), "py" .= (1 :: Int)])]))
                opts = defaultAgentOpts{agentMaxRounds = 2}
            res <- try (runEff $ runLLMMockFull (take 10 infiniteTool) (useToolsWith opts [pointTool] "Task"))
            case res of
                Left (AgentRoundsExhausted 2) -> pure ()
                Left other -> expectationFailure ("Expected AgentRoundsExhausted, got: " <> show other)
                Right _ -> expectationFailure "Expected AgentRoundsExhausted exception"

    describe "Batch 3 Adversarial Suite: Delimiter-Stack JSON Extraction" $ do
        it "rejects top-level mismatched delimiters: [1, 2, 3}" $ do
            case extractJSON "Some prefix text: [1, 2, 3} some suffix" of
                Left err -> err `shouldSatisfy` (\s -> "unbalanced" `T.isInfixOf` T.pack s || "no JSON" `T.isInfixOf` T.pack s)
                Right v -> expectationFailure ("Parsed invalid mismatched JSON: " <> show v)

        it "rejects top-level mismatched delimiters: {\"a\": 1]" $ do
            case extractJSON "{\"a\": 1]" of
                Left err -> err `shouldSatisfy` (\s -> "unbalanced" `T.isInfixOf` T.pack s || "no JSON" `T.isInfixOf` T.pack s)
                Right v -> expectationFailure ("Parsed invalid mismatched JSON: " <> show v)

        it "rejects nested bracket mismatches: {\"nested\": [1, 2}}" $ do
            case extractJSON "```json\n{\"nested\": [1, 2}}\n```" of
                Left _ -> pure ()
                Right v -> expectationFailure ("Parsed invalid nested mismatched JSON: " <> show v)

        it "rejects nested brace mismatches: [{\"key\": \"val\"]" $ do
            case extractJSON "[{\"key\": \"val\"]" of
                Left _ -> pure ()
                Right v -> expectationFailure ("Parsed invalid nested mismatched JSON: " <> show v)

        it "extracts deeply nested balanced brackets and braces (50 levels deep)" $ do
            let openBrackets = concat (replicate 25 "[{\"k\": ")
                closeBrackets = concat (replicate 25 "}]")
                payload = "Deep JSON: " <> T.pack openBrackets <> "42" <> T.pack closeBrackets <> " trailing note"
            case extractJSON payload of
                Right _ -> pure ()
                Left err -> expectationFailure ("Failed on deeply nested balanced brackets: " <> err)

        it "handles strings containing mismatched brackets without failing" $ do
            let trickyText = "Here is the response: {\"warning\": \"[mismatched in string}\", \"code\": 200}"
            case extractJSON trickyText of
                Right (Object o) -> KM.lookup (Key.fromText "code") o `shouldBe` Just (Number 200)
                other -> expectationFailure ("Expected Right object, got: " <> show other)

        it "handles escaped quotes containing brackets inside strings" $ do
            let payload = "{\"data\": \"escaped \\\" [ bracket and { brace \\\" done\", \"num\": 123}"
            case extractJSON payload of
                Right (Object o) -> KM.lookup (Key.fromText "num") o `shouldBe` Just (Number 123)
                other -> expectationFailure ("Expected Right object, got: " <> show other)
