{-# LANGUAGE OverloadedStrings #-}

module LLMonad.OpenAICompatSpec (spec) where

import Data.Aeson
  ( Value (..)
  , object
  , (.=)
  )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as V
import LLMonad.Error (LLMError (..))
import LLMonad.Provider (StructuredMode (..))
import LLMonad.Providers.OpenAICompatible
import LLMonad.Types
import Test.Hspec

lookupV :: Text -> Value -> Maybe Value
lookupV k (Object o) = KM.lookup (Key.fromText k) o
lookupV _ _ = Nothing

at :: [Text] -> Value -> Maybe Value
at [] v = Just v
at (k : ks) v = lookupV k v >>= at ks

sampleSchema :: Value
sampleSchema =
  object
    [ "type" .= ("object" :: Text)
    , "properties" .= object [Key.fromText "q" .= object ["type" .= ("string" :: Text)]]
    , "required" .= ["q" :: Text]
    , "additionalProperties" .= False
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

lbs :: Text -> ByteString
lbs = LBS.fromStrict . encodeUtf8

spec :: Spec
spec = do
  describe "structuredTiers" $ do
    it "uses the full ladder for native providers" $
      structuredTiers StructuredNative (RfJsonSchema "x" sampleSchema True)
        `shouldBe` [TierJsonSchema True, TierJsonSchema False, TierJsonObject, TierPromptOnly]

    it "skips json_schema for json-object-only providers" $
      structuredTiers StructuredJsonObjectOnly (RfJsonSchema "x" sampleSchema True)
        `shouldBe` [TierJsonObject, TierPromptOnly]

    it "degrades to prompt-only when nothing is supported" $
      structuredTiers StructuredPromptOnly (RfJsonSchema "x" sampleSchema True)
        `shouldBe` [TierPromptOnly]

    it "never uses response_format for plain text" $
      structuredTiers StructuredNative RfText `shouldBe` [TierPromptOnly]

  describe "buildChatCompletionsBody" $ do
    let cfg = defaultOpenAICompatConfig "https://example.com/v1"

    it "renders strict json_schema response_format" $ do
      let body = buildChatCompletionsBody cfg (TierJsonSchema True) schemaReq
      at ["response_format", "type"] body `shouldBe` Just (String "json_schema")
      at ["response_format", "json_schema", "strict"] body `shouldBe` Just (Bool True)
      at ["response_format", "json_schema", "name"] body `shouldBe` Just (String "Answer")

    it "puts the system message first" $ do
      let body = buildChatCompletionsBody cfg TierPromptOnly schemaReq
      case at ["messages"] body of
        Just (Array msgs) -> lookupV "role" (V.head msgs) `shouldBe` Just (String "system")
        other -> expectationFailure ("expected messages array, got: " <> show other)

    it "appends the schema directive to the system prompt on degraded tiers" $ do
      let body = buildChatCompletionsBody cfg TierJsonObject schemaReq
      case at ["messages"] body of
        Just (Array msgs) -> case lookupV "content" (V.head msgs) of
          Just (String sysContent) -> sysContent `shouldSatisfy` T.isInfixOf "JSON Schema"
          other -> expectationFailure ("expected string content, got: " <> show other)
        other -> expectationFailure ("expected messages array, got: " <> show other)

    it "honors the max_completion_tokens flag" $ do
      let cfg' = cfg {ocUseMaxCompletionTokens = True}
          req = schemaReq {crParams = defaultParams {paramMaxTokens = Just 100}}
          body = buildChatCompletionsBody cfg' TierPromptOnly req
      lookupV "max_completion_tokens" body `shouldBe` Just (Number 100)
      lookupV "max_tokens" body `shouldBe` Nothing

    it "encodes tools and forced tool choice" $ do
      let toolSpec1 = ToolSpec "calc" "adds" sampleSchema
          req =
            schemaReq
              { crResponseFormat = RfText
              , crTools = [toolSpec1]
              , crToolChoice = ToolForce "calc"
              }
          body = buildChatCompletionsBody cfg TierPromptOnly req
      case at ["tools"] body of
        Just (Array ts) -> length ts `shouldBe` 1
        other -> expectationFailure ("no tools: " <> show other)
      at ["tool_choice", "function", "name"] body `shouldBe` Just (String "calc")

  describe "parseChatCompletionsResponse" $ do
    it "parses text responses with usage" $ do
      let raw = "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":\"hello\"},\"finish_reason\":\"stop\"}],\"usage\":{\"prompt_tokens\":10,\"completion_tokens\":5}}"
      case parseChatCompletionsResponse (lbs raw) of
        Right resp -> do
          crspText resp `shouldBe` "hello"
          crspFinishReason resp `shouldBe` FrStop
          crspUsage resp `shouldBe` Just (Usage 10 5)
        Left e -> expectationFailure (show e)

    it "parses tool calls and decodes arguments" $ do
      let raw = "{\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":null,\"tool_calls\":[{\"id\":\"call_1\",\"type\":\"function\",\"function\":{\"name\":\"calc\",\"arguments\":\"{\\\"a\\\": 2}\"}}]},\"finish_reason\":\"tool_calls\"}]}"
      case parseChatCompletionsResponse (lbs raw) of
        Right resp -> case crspToolCalls resp of
          [c] -> do
            toolCallId c `shouldBe` "call_1"
            toolCallName c `shouldBe` "calc"
            lookupV "a" (toolCallArguments c) `shouldBe` Just (Number 2)
            crspFinishReason resp `shouldBe` FrToolUse
          other -> expectationFailure ("expected 1 tool call, got: " <> show other)
        Left e -> expectationFailure (show e)

    it "rejects empty choices" $ do
      case parseChatCompletionsResponse (lbs "{\"choices\":[]}") of
        Left NoAssistantMessage -> pure ()
        other -> expectationFailure ("expected NoAssistantMessage, got: " <> show other)

  describe "streaming state machine" $ do
    it "assembles text deltas into a final response" $ do
      let chunks =
            [ "{\"choices\":[{\"delta\":{\"role\":\"assistant\",\"content\":\"Hel\"}}]}"
            , "{\"choices\":[{\"delta\":{\"content\":\"lo\"}}]}"
            , "{\"choices\":[{\"delta\":{},\"finish_reason\":\"stop\"}]}"
            ]
          (st, events) = foldl step (initialOAIStreamState, []) chunks
          step (s, evts) c =
            let (s', evts') = handleOpenAIChunk s c
             in (s', evts ++ evts')
      map streamEventText events `shouldBe` ["Hel", "lo"]
      case finalizeOAIStream st of
        Right resp -> do
          crspText resp `shouldBe` "Hello"
          crspFinishReason resp `shouldBe` FrStop
        Left e -> expectationFailure (show e)

    it "merges streamed tool call fragments by index" $ do
      let chunks =
            [ "{\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"c1\",\"function\":{\"name\":\"calc\",\"arguments\":\"{\\\"a\\\"\"}}]}}]}"
            , "{\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\":1}\"}}]}}]}"
            , "{\"choices\":[{\"delta\":{},\"finish_reason\":\"tool_calls\"}]}"
            ]
          st = foldl (\s c -> fst (handleOpenAIChunk s c)) initialOAIStreamState chunks
      case finalizeOAIStream st of
        Right resp -> case crspToolCalls resp of
          [c] -> do
            toolCallId c `shouldBe` "c1"
            lookupV "a" (toolCallArguments c) `shouldBe` Just (Number 1)
          other -> expectationFailure ("expected 1 tool call, got: " <> show other)
        Left e -> expectationFailure (show e)

    it "ignores [DONE]" $ do
      let (st, evts) = handleOpenAIChunk initialOAIStreamState "[DONE]"
      evts `shouldBe` []
      finalizeOAIStream st `shouldSatisfy` either (const False) (\r -> crspText r == "")
  where
    streamEventText (SEText t) = t
    streamEventText _ = ""
