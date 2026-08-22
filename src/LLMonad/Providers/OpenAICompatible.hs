{-# LANGUAGE OverloadedStrings #-}

-- | Transport for every API that speaks the OpenAI Chat Completions
-- protocol: OpenAI, Groq, DeepSeek, Mistral, Together, OpenRouter,
-- Ollama, LM Studio, vLLM, and friends.
--
-- Structured output degrades gracefully. The request ladder is:
--
-- 1. @response_format: json_schema@ (strict) — enforced server-side.
-- 2. @response_format: json_schema@ (non-strict).
-- 3. @response_format: json_object@ + schema in the system prompt.
-- 4. Schema in the system prompt only.
--
-- If the endpoint rejects a rung with a format-related 4xx, the next rung
-- is tried automatically, so the same code runs against GPT-4o and a
-- two-year-old llama.cpp server alike.
module LLMonad.Providers.OpenAICompatible
  ( -- * Configuration
    OpenAICompatConfig (..)
  , defaultOpenAICompatConfig

    -- * Constructor & presets
  , openAICompat
  , openAIProvider
  , groqProvider
  , deepSeekProvider
  , mistralProvider
  , togetherProvider
  , openRouterProvider
  , ollamaProvider
  , lmStudioProvider

    -- * Pure internals (exposed for testing)
  , StructuredTier (..)
  , structuredTiers
  , buildChatCompletionsBody
  , parseChatCompletionsResponse
  , OAIStreamState (..)
  , initialOAIStreamState
  , handleOpenAIChunk
  , finalizeOAIStream
  ) where

import Data.Aeson
  ( Value (..)
  , object
  , withObject
  , (.!=)
  , (.:?)
  , (.=)
  )
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Control.Applicative ((<|>))
import Data.IORef
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import LLMonad.Error (LLMError (..))
import LLMonad.Internal.Extract (extractJSON)
import LLMonad.Internal.Http (postJSON, postJSONStream)
import LLMonad.Internal.SSE
  ( finishSSE
  , newSSEParser
  , stepSSE
  )
import LLMonad.Provider
  ( Provider (..)
  , StructuredMode (..)
  )
import LLMonad.Types

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

data OpenAICompatConfig = OpenAICompatConfig
  { -- | Base URL up to and including the version segment,
    -- e.g. @https://api.openai.com/v1@.
    ocBaseUrl :: Text
  , -- | Bearer token; 'Nothing' for local servers.
    ocApiKey :: Maybe Text
  , ocExtraHeaders :: [(Text, Text)]
  , -- | Best structured-output capability of this endpoint.
    ocStructured :: StructuredMode
  , -- | Send @max_completion_tokens@ instead of @max_tokens@
    -- (required by newer OpenAI models).
    ocUseMaxCompletionTokens :: Bool
  , ocTimeoutSeconds :: Maybe Int
  }
  deriving (Eq, Show)

defaultOpenAICompatConfig :: Text -> OpenAICompatConfig
defaultOpenAICompatConfig baseUrl =
  OpenAICompatConfig
    { ocBaseUrl = baseUrl
    , ocApiKey = Nothing
    , ocExtraHeaders = []
    , ocStructured = StructuredNative
    , ocUseMaxCompletionTokens = False
    , ocTimeoutSeconds = Nothing
    }

--------------------------------------------------------------------------------
-- Presets
--------------------------------------------------------------------------------

openAICompat :: OpenAICompatConfig -> Provider
openAICompat cfg =
  Provider
    { providerName = "openai-compatible:" <> ocBaseUrl cfg
    , providerStructured = ocStructured cfg
    , providerComplete = completeWith cfg
    , providerStream = streamWith cfg
    }

-- | api.openai.com. Sends @max_completion_tokens@ (newer models reject
-- @max_tokens@) and tolerates older servers through the downgrade ladder.
openAIProvider :: Text -> Provider
openAIProvider key =
  let base = openAICompat ((defaultOpenAICompatConfig "https://api.openai.com/v1") {ocApiKey = Just key, ocUseMaxCompletionTokens = True})
   in base {providerName = "openai"}

groqProvider :: Text -> Provider
groqProvider key =
  let base = openAICompat ((defaultOpenAICompatConfig "https://api.groq.com/openai/v1") {ocApiKey = Just key})
   in base {providerName = "groq"}

deepSeekProvider :: Text -> Provider
deepSeekProvider key =
  let base = openAICompat ((defaultOpenAICompatConfig "https://api.deepseek.com/v1") {ocApiKey = Just key})
   in base {providerName = "deepseek"}

mistralProvider :: Text -> Provider
mistralProvider key =
  let base = openAICompat ((defaultOpenAICompatConfig "https://api.mistral.ai/v1") {ocApiKey = Just key})
   in base {providerName = "mistral"}

togetherProvider :: Text -> Provider
togetherProvider key =
  let base = openAICompat ((defaultOpenAICompatConfig "https://api.together.xyz/v1") {ocApiKey = Just key})
   in base {providerName = "together"}

openRouterProvider :: Text -> Provider
openRouterProvider key =
  let base = openAICompat ((defaultOpenAICompatConfig "https://openrouter.ai/api/v1") {ocApiKey = Just key})
   in base {providerName = "openrouter"}

-- | Local Ollama. Pass the host, e.g. @http:\/\/localhost:11434@.
ollamaProvider :: Text -> Provider
ollamaProvider host =
  let cleanHost = maybe host id (T.stripSuffix "/" host)
      base = openAICompat (defaultOpenAICompatConfig (cleanHost <> "/v1"))
   in base {providerName = "ollama"}

-- | LM Studio's local server. Pass the host, e.g. @http:\/\/localhost:1234@.
lmStudioProvider :: Text -> Provider
lmStudioProvider host =
  let cleanHost = maybe host id (T.stripSuffix "/" host)
      cfg =
        (defaultOpenAICompatConfig (cleanHost <> "/v1"))
          { ocStructured = StructuredJsonObjectOnly
          }
      base = openAICompat cfg
   in base {providerName = "lmstudio"}

--------------------------------------------------------------------------------
-- Structured-output ladder
--------------------------------------------------------------------------------

data StructuredTier
  = TierJsonSchema Bool -- ^ strict?
  | TierJsonObject
  | TierPromptOnly
  deriving (Eq, Show)

structuredTiers :: StructuredMode -> ResponseFormat -> [StructuredTier]
structuredTiers mode rf = case rf of
  RfText -> [TierPromptOnly]
  RfJsonObject -> filterApplicable [TierJsonObject, TierPromptOnly]
  RfJsonSchema _ _ _ ->
    filterApplicable [TierJsonSchema True, TierJsonSchema False, TierJsonObject, TierPromptOnly]
  where
    filterApplicable = case mode of
      StructuredNative -> id
      StructuredJsonObjectOnly ->
        dropWhile (\t -> t == TierJsonSchema True || t == TierJsonSchema False)
      StructuredPromptOnly -> const [TierPromptOnly]

-- | Does this error look like \"your response_format is not supported\"?
isFormatRejection :: LLMError -> Bool
isFormatRejection (ApiError st body)
  | st `elem` [400, 404, 422] =
      any (`T.isInfixOf` T.toLower body)
        [ "json_schema"
        , "response_format"
        , "json_object"
        , "schema"
        , "unsupported"
        , "unrecognized"
        , "unknown argument"
        , "unknown field"
        , "not supported"
        ]
isFormatRejection _ = False

--------------------------------------------------------------------------------
-- Request rendering
--------------------------------------------------------------------------------

chatUrl :: OpenAICompatConfig -> Text
chatUrl cfg =
  let base = ocBaseUrl cfg
   in maybe base id (T.stripSuffix "/" base) <> "/chat/completions"

authHeaders :: OpenAICompatConfig -> [(Text, Text)]
authHeaders cfg = case ocApiKey cfg of
  Nothing -> []
  Just k -> [("Authorization", "Bearer " <> k)]

schemaDirective :: Value -> Text
schemaDirective schema =
  "You must respond with ONLY a single JSON value that conforms to the following JSON Schema. \
  \No prose, no markdown fences, no explanation - just the JSON.\n\n"
    <> renderCompact schema

jsonOnlyDirective :: Text
jsonOnlyDirective = "Respond with ONLY a valid JSON object. No prose, no markdown fences."

renderCompact :: Value -> Text
renderCompact = decodeUtf8With lenientDecode . LBS.toStrict . A.encode

-- | Render the request body for one rung of the structured-output ladder.
buildChatCompletionsBody :: OpenAICompatConfig -> StructuredTier -> CompletionRequest -> Value
buildChatCompletionsBody cfg tier req = object $ concat
  [ ["model" .= unModel (crModel req)]
  , ["messages" .= messages]
  , paramFields
  , toolFields
  , formatFields
  ]
  where
    directive = case tier of
      TierJsonSchema {} -> Nothing
      _ -> case crResponseFormat req of
        RfJsonSchema _ s _ -> Just (schemaDirective s)
        RfJsonObject -> Just jsonOnlyDirective
        RfText -> Nothing
    fullSystem = fmap (\base -> maybe base (\d -> base <> "\n\n" <> d) directive) (crSystem req)
    sysMsgs = case fullSystem of
      Nothing -> []
      Just t -> [object ["role" .= ("system" :: Text), "content" .= t]]
    messages = sysMsgs ++ map encodeMessage (crMessages req)

    p = crParams req
    maxTokensKey = if ocUseMaxCompletionTokens cfg then "max_completion_tokens" else "max_tokens"
    paramFields = catMaybes
      [ ("temperature" .=) <$> paramTemperature p
      , ("top_p" .=) <$> paramTopP p
      , (maxTokensKey .=) <$> paramMaxTokens p
      , if null (paramStopSequences p) then Nothing else Just ("stop" .= paramStopSequences p)
      ]

    toolFields = case (crTools req, crToolChoice req) of
      ([], _) -> []
      (specs, choice) ->
        [ "tools"
            .= [ object ["type" .= ("function" :: Text), "function" .= encodeSpec s]
               | s <- specs
               ]
        , "tool_choice" .= case choice of
            ToolAuto -> String "auto"
            ToolForce n ->
              object ["type" .= ("function" :: Text), "function" .= object ["name" .= n]]
        ]

    formatFields = case tier of
      TierJsonSchema strict -> case crResponseFormat req of
        RfJsonSchema n s _ ->
          [ "response_format" .= object
              [ "type" .= ("json_schema" :: Text)
              , "json_schema" .= object
                  [ "name" .= n
                  , "strict" .= strict
                  , "schema" .= s
                  ]
              ]
          ]
        _ -> []
      TierJsonObject -> ["response_format" .= object ["type" .= ("json_object" :: Text)]]
      TierPromptOnly -> []

    encodeSpec s =
      object
        [ "name" .= toolSpecName s
        , "description" .= toolSpecDescription s
        , "parameters" .= toolSpecParameters s
        ]

encodeMessage :: ChatMessage -> Value
encodeMessage m = case m of
  SystemMsg t -> object ["role" .= ("system" :: Text), "content" .= t]
  UserMsg t -> object ["role" .= ("user" :: Text), "content" .= t]
  AssistantMsg t calls ->
    object $
      ["role" .= ("assistant" :: Text), "content" .= t]
        ++ (if null calls then [] else ["tool_calls" .= map encCall calls])
  ToolMsg cid c ->
    object ["role" .= ("tool" :: Text), "content" .= c, "tool_call_id" .= cid]
  where
    encCall c =
      object
        [ "id" .= toolCallId c
        , "type" .= ("function" :: Text)
        , "function" .= object
            [ "name" .= toolCallName c
            , "arguments" .= renderCompact (toolCallArguments c)
            ]
        ]

--------------------------------------------------------------------------------
-- Response parsing
--------------------------------------------------------------------------------

parseChatCompletionsResponse :: ByteString -> Either LLMError CompletionResponse
parseChatCompletionsResponse body = do
  resp <- eitherDecodeLenient body
  case oaiChoices resp of
    [] -> Left NoAssistantMessage
    (choice : _) -> do
      let msg = oaiChoiceMessage choice
          text = fromMaybe "" (msg >>= oaiContent)
          calls = maybe [] (map convCall) (msg >>= oaiToolCalls)
      pure
        CompletionResponse
          { crspText = text
          , crspToolCalls = calls
          , crspStructuredPayload = Nothing
          , crspFinishReason = maybe FrStop finishFromText (oaiFinishReason choice)
          , crspUsage = fmap convUsage (oaiUsage resp)
          }
  where
    eitherDecodeLenient bs =
      either
        (\err -> Left (DecodeError err (decodeUtf8With lenientDecode (LBS.toStrict bs))))
        Right
        (A.eitherDecode' bs)
    convCall tc =
      ToolCall
        { toolCallId = fromMaybe "" (oaiTcId tc)
        , toolCallName = fromMaybe "" (oaiTcFunction tc >>= oaiFnName)
        , toolCallArguments = parseArgs (oaiTcFunction tc >>= oaiFnArguments)
        }
    parseArgs Nothing = Null
    parseArgs (Just t) = parseArgsText t
    convUsage u = Usage (fromMaybe 0 (oaiUsagePrompt u)) (fromMaybe 0 (oaiUsageCompletion u))

parseArgsText :: Text -> Value
parseArgsText t = case extractJSON t of
  Right v -> v
  Left _ -> String t

finishFromText :: Text -> FinishReason
finishFromText t = case t of
  "stop" -> FrStop
  "length" -> FrLength
  "tool_calls" -> FrToolUse
  "function_call" -> FrToolUse
  "content_filter" -> FrContentFilter
  other -> FrOther other

--------------------------------------------------------------------------------
-- Response shapes
--------------------------------------------------------------------------------

data OAIResp = OAIResp
  { oaiChoices :: [OAIChoice]
  , oaiUsage :: Maybe OAIUsage
  }

instance A.FromJSON OAIResp where
  parseJSON = withObject "chat-completion-response" $ \o ->
    OAIResp <$> o .:? "choices" .!= [] <*> o .:? "usage"

data OAIChoice = OAIChoice
  { oaiChoiceMessage :: Maybe OAIMessage
  , oaiFinishReason :: Maybe Text
  }

instance A.FromJSON OAIChoice where
  parseJSON = withObject "choice" $ \o ->
    OAIChoice <$> o .:? "message" <*> o .:? "finish_reason"

data OAIMessage = OAIMessage
  { oaiContent :: Maybe Text
  , oaiToolCalls :: Maybe [OAIToolCall]
  }

instance A.FromJSON OAIMessage where
  parseJSON = withObject "message" $ \o ->
    OAIMessage <$> o .:? "content" <*> o .:? "tool_calls"

data OAIToolCall = OAIToolCall
  { oaiTcId :: Maybe Text
  , oaiTcFunction :: Maybe OAIFunction
  }

instance A.FromJSON OAIToolCall where
  parseJSON = withObject "tool-call" $ \o ->
    OAIToolCall <$> o .:? "id" <*> o .:? "function"

data OAIFunction = OAIFunction
  { oaiFnName :: Maybe Text
  , oaiFnArguments :: Maybe Text
  }

instance A.FromJSON OAIFunction where
  parseJSON = withObject "function" $ \o ->
    OAIFunction <$> o .:? "name" <*> o .:? "arguments"

data OAIUsage = OAIUsage
  { oaiUsagePrompt :: Maybe Int
  , oaiUsageCompletion :: Maybe Int
  }

instance A.FromJSON OAIUsage where
  parseJSON = withObject "usage" $ \o ->
    OAIUsage <$> o .:? "prompt_tokens" <*> o .:? "completion_tokens"

--------------------------------------------------------------------------------
-- Streaming state machine (pure, testable)
--------------------------------------------------------------------------------

-- | Accumulated state while consuming streamed chunks.
data OAIStreamState = OAIStreamState
  { osText :: Text
  , -- | index -> (id, name, arguments-so-far)
    osCalls :: IntMap (Maybe Text, Maybe Text, Text)
  , osFinish :: Maybe FinishReason
  , osUsage :: Maybe Usage
  , osError :: Maybe LLMError
  , osDoneSeen :: Bool
  }
  deriving (Eq, Show)

initialOAIStreamState :: OAIStreamState
initialOAIStreamState = OAIStreamState mempty IM.empty Nothing Nothing Nothing False

-- | Fold one SSE payload (a JSON chunk or @[DONE]@) into the state,
-- emitting any new text as 'SEText' events.
handleOpenAIChunk :: OAIStreamState -> Text -> (OAIStreamState, [StreamEvent])
handleOpenAIChunk st payload
  | T.strip payload == "[DONE]" = (st {osDoneSeen = True}, [])
  | otherwise = case A.eitherDecode' (LBS.fromStrict (encodeUtf8 payload)) of
      Left _ -> (st, []) -- ignore keep-alives / non-JSON noise
      Right chunk -> case oaiChunkError chunk of
        Just e -> (st {osError = Just (ApiError 500 (renderCompact e))}, [])
        Nothing ->
          let st' = foldl' applyChoice st (oaiChunkChoices chunk)
              st'' = case oaiChunkUsage chunk of
                Just u -> st' {osUsage = Just (Usage (fromMaybe 0 (oaiUsagePrompt u)) (fromMaybe 0 (oaiUsageCompletion u)))}
                Nothing -> st'
           in (st'', deltas st st'')
  where
    deltas before after =
      let oldLen = T.length (osText before)
          newLen = T.length (osText after)
       in if newLen > oldLen
            then [SEText (T.takeEnd (newLen - oldLen) (osText after))]
            else []

applyChoice :: OAIStreamState -> OAIChunkChoice -> OAIStreamState
applyChoice st ch =
  st
    { osText = osText st <> fromMaybe "" (oaiDeltaContent =<< oaiChoiceDelta ch)
    , osCalls = foldl' applyCall (osCalls st) (maybe [] (fromMaybe [] . oaiDeltaToolCalls) (oaiChoiceDelta ch))
    , osFinish = case oaiChoiceFinish ch of
        Just fr -> Just (finishFromText fr)
        Nothing -> osFinish st
    }
  where
    applyCall acc tc =
      let idx = fromMaybe 0 (oaiDeltaTcIndex tc)
          (oldId, oldName, oldArgs) = IM.findWithDefault (Nothing, Nothing, "") idx acc
          newId = oaiDeltaTcId tc <|> oldId
          newName = (oaiDeltaTcFunction tc >>= oaiFnName) <|> oldName
          newArgs = oldArgs <> fromMaybe "" (oaiDeltaTcFunction tc >>= oaiFnArguments)
       in IM.insert idx (newId, newName, newArgs) acc

finalizeOAIStream :: OAIStreamState -> Either LLMError CompletionResponse
finalizeOAIStream st = case osError st of
  Just e -> Left e
  Nothing
    | not (osDoneSeen st) && isNothing (osFinish st) ->
        Left (HttpError "Stream ended prematurely without [DONE] or finish_reason")
    | otherwise ->
        Right
          CompletionResponse
            { crspText = osText st
            , crspToolCalls =
                [ ToolCall
                    { toolCallId = fromMaybe (T.pack ('c' : show i)) mid
                    , toolCallName = fromMaybe "" mn
                    , toolCallArguments = parseArgsText ma
                    }
                | (i, (mid, mn, ma)) <- IM.toAscList (osCalls st)
                ]
            , crspStructuredPayload = Nothing
            , crspFinishReason = fromMaybe FrStop (osFinish st)
            , crspUsage = osUsage st
            }

data OAIChunk = OAIChunk
  { oaiChunkChoices :: [OAIChunkChoice]
  , oaiChunkError :: Maybe Value
  , oaiChunkUsage :: Maybe OAIUsage
  }

instance A.FromJSON OAIChunk where
  parseJSON = withObject "chunk" $ \o ->
    OAIChunk <$> o .:? "choices" .!= [] <*> o .:? "error" <*> o .:? "usage"

data OAIChunkChoice = OAIChunkChoice
  { oaiChoiceDelta :: Maybe OAIDelta
  , oaiChoiceFinish :: Maybe Text
  }

instance A.FromJSON OAIChunkChoice where
  parseJSON = withObject "chunk-choice" $ \o ->
    OAIChunkChoice <$> o .:? "delta" <*> o .:? "finish_reason"

data OAIDelta = OAIDelta
  { oaiDeltaContent :: Maybe Text
  , oaiDeltaToolCalls :: Maybe [OAIToolCallDelta]
  }

instance A.FromJSON OAIDelta where
  parseJSON = withObject "delta" $ \o ->
    OAIDelta <$> o .:? "content" <*> o .:? "tool_calls"

data OAIToolCallDelta = OAIToolCallDelta
  { oaiDeltaTcIndex :: Maybe Int
  , oaiDeltaTcId :: Maybe Text
  , oaiDeltaTcFunction :: Maybe OAIFunction
  }

instance A.FromJSON OAIToolCallDelta where
  parseJSON = withObject "tool-call-delta" $ \o ->
    OAIToolCallDelta <$> o .:? "index" <*> o .:? "id" <*> o .:? "function"

--------------------------------------------------------------------------------
-- IO shells
--------------------------------------------------------------------------------

completeWith :: OpenAICompatConfig -> CompletionRequest -> IO (Either LLMError CompletionResponse)
completeWith cfg req = go (structuredTiers (ocStructured cfg) (crResponseFormat req))
  where
    go [] = pure (Left (UnsupportedCapability "no compatible request shape"))
    go (tier : rest) = do
      let body = buildChatCompletionsBody cfg tier req
      r <- postJSON (chatUrl cfg) (authHeaders cfg ++ ocExtraHeaders cfg) (ocTimeoutSeconds cfg) body
      case r of
        Left e
          | not (null rest)
          , isFormatRejection e ->
              go rest
          | otherwise -> pure (Left e)
        Right (_, _, bodyBs) -> pure (parseChatCompletionsResponse bodyBs)

streamWith ::
  OpenAICompatConfig ->
  CompletionRequest ->
  (StreamEvent -> IO ()) ->
  IO (Either LLMError CompletionResponse)
streamWith cfg req cb = do
  stateRef <- newIORef initialOAIStreamState
  parserRef <- newIORef newSSEParser
  go (structuredTiers (ocStructured cfg) (crResponseFormat req)) stateRef parserRef
  where
    go [] _ _ = pure (Left (UnsupportedCapability "no compatible request shape"))
    go (tier : rest) stateRef parserRef = do
      writeIORef stateRef initialOAIStreamState
      writeIORef parserRef newSSEParser
      let baseBody = buildChatCompletionsBody cfg tier req
          body = case baseBody of
            Object o -> Object (KM.insert "stream" (Bool True) o)
            v -> v
      r <-
        postJSONStream
          (chatUrl cfg)
          (authHeaders cfg ++ ocExtraHeaders cfg)
          (ocTimeoutSeconds cfg)
          body
          (onChunk stateRef parserRef)
      case r of
        Left e
          | not (null rest)
          , isFormatRejection e ->
              go rest stateRef parserRef
          | otherwise -> pure (Left e)
        Right () -> do
          -- Flush any trailing event the peer never terminated with a blank line.
          parser <- readIORef parserRef
          mapM_ (feedEvent stateRef) (finishSSE parser)
          st <- readIORef stateRef
          case finalizeOAIStream st of
            Left e -> pure (Left e)
            Right resp -> do
              cb (SEFinished resp)
              pure (Right resp)

    onChunk stateRef parserRef chunk = do
      parser <- readIORef parserRef
      let (parser', events) = stepSSE parser chunk
      writeIORef parserRef parser'
      mapM_ (feedEvent stateRef) events

    feedEvent stateRef payload = do
      st <- readIORef stateRef
      let (st', evts) = handleOpenAIChunk st payload
      writeIORef stateRef st'
      mapM_ cb evts
