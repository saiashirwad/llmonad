{-# LANGUAGE OverloadedStrings #-}

-- | Transport for Anthropic's native Messages API (@\/v1\/messages@).
--
-- Structured output is implemented the way Anthropic recommends for
-- schema-constrained extraction: the target schema is registered as a
-- synthetic tool and @tool_choice@ forces the model to call it. The tool
-- input is then surfaced as the response text (a JSON document), so the
-- rest of the library treats it identically to OpenAI-style structured
-- output.
module LLMonad.Providers.Anthropic
  ( -- * Configuration
    AnthropicConfig (..)
  , defaultAnthropicConfig

    -- * Constructor & presets
  , anthropicProvider
  , anthropicProviderWith

    -- * Pure internals (exposed for testing)
  , buildMessagesBody
  , parseMessagesResponse
  , AntStreamState (..)
  , initialAntStreamState
  , handleAnthropicEvent
  , finalizeAnthropicStream
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
import Data.Aeson.Key qualified as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as A
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import LLMonad.Error (LLMError (..))
import LLMonad.Internal.Http (postJSON, postJSONStream)
import LLMonad.Internal.SSE
  ( finishSSE
  , newSSEParser
  , stepSSE
  )
import LLMonad.Provider (Provider (..), StructuredMode (..))
import LLMonad.Types

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

data AnthropicConfig = AnthropicConfig
  { acApiKey :: Text
  , -- | Default @https:\/\/api.anthropic.com@.
    acBaseUrl :: Text
  , -- | API version header value.
    acVersion :: Text
  , acExtraHeaders :: [(Text, Text)]
  , -- | Fallback @max_tokens@; Anthropic requires it on every request.
    acDefaultMaxTokens :: Int
  , acTimeoutSeconds :: Maybe Int
  }
  deriving (Eq, Show)

defaultAnthropicConfig :: Text -> AnthropicConfig
defaultAnthropicConfig key =
  AnthropicConfig
    { acApiKey = key
    , acBaseUrl = "https://api.anthropic.com"
    , acVersion = "2023-06-01"
    , acExtraHeaders = []
    , acDefaultMaxTokens = 4096
    , acTimeoutSeconds = Nothing
    }

anthropicProvider :: Text -> Provider
anthropicProvider = anthropicProviderWith . defaultAnthropicConfig

anthropicProviderWith :: AnthropicConfig -> Provider
anthropicProviderWith cfg =
  Provider
    { providerName = "anthropic"
    , providerStructured = StructuredNative
    , providerComplete = completeWith cfg
    , providerStream = streamWith cfg
    }

--------------------------------------------------------------------------------
-- Request rendering
--------------------------------------------------------------------------------

messagesUrl :: AnthropicConfig -> Text
messagesUrl cfg =
  let base = acBaseUrl cfg
   in maybe base id (T.stripSuffix "/" base) <> "/v1/messages"

authHeaders :: AnthropicConfig -> [(Text, Text)]
authHeaders cfg =
  [("x-api-key", acApiKey cfg), ("anthropic-version", acVersion cfg)] ++ ocExtra
  where
    ocExtra = acExtraHeaders cfg

structuredToolName :: Text
structuredToolName = "__llmonad_structured_output"

jsonOnlyDirective :: Text
jsonOnlyDirective = "Respond with ONLY a valid JSON object. No prose, no markdown fences."

renderCompact :: Value -> Text
renderCompact = decodeUtf8With lenientDecode . LBS.toStrict . A.encode

-- | Render the Messages-API request body.
buildMessagesBody :: AnthropicConfig -> CompletionRequest -> Value
buildMessagesBody cfg req = object $ concat
  [ [ "model" .= unModel (crModel req)
    , "max_tokens" .= fromMaybe (acDefaultMaxTokens cfg) (paramMaxTokens p)
    ]
  , maybe [] (\s -> ["system" .= s]) systemText
  , ["messages" .= encodeAnthropicMessages (crMessages req)]
  , catMaybes
      [ ("temperature" .=) <$> paramTemperature p
      , ("top_p" .=) <$> paramTopP p
      , if null (paramStopSequences p) then Nothing else Just ("stop_sequences" .= paramStopSequences p)
      ]
  , toolFields
  ]
  where
    p = crParams req
    rf = crResponseFormat req

    directive = case rf of
      RfJsonObject -> Just jsonOnlyDirective
      _ -> Nothing
    systemText = fmap (\base -> maybe base (\d -> base <> "\n\n" <> d) directive) (crSystem req)

    forcedSchema = case rf of
      RfJsonSchema _n s _ -> Just (structuredToolName, s)
      _ -> Nothing

    forcedName = structuredToolName

    userTools =
      [ object
          [ "name" .= toolSpecName s
          , "description" .= toolSpecDescription s
          , "input_schema" .= toolSpecParameters s
          ]
      | s <- crTools req
      ]

    structuredTools = case forcedSchema of
      Just (n, s) ->
        [ object
            [ "name" .= n
            , "description" .= ("Return the result as JSON conforming to this schema." :: Text)
            , "input_schema" .= s
            ]
        ]
      Nothing -> []

    toolFields = case (userTools ++ structuredTools, crToolChoice req, forcedSchema) of
      ([], _, _) -> []
      (tools, _, Just _) ->
        -- A forced structured-output tool wins over everything else.
        [ "tools" .= tools
        , "tool_choice" .= object ["type" .= ("tool" :: Text), "name" .= forcedName]
        ]
      (tools, choice, Nothing) ->
        [ "tools" .= tools
        , "tool_choice" .= case choice of
            ToolAuto -> object ["type" .= ("auto" :: Text)]
            ToolForce n -> object ["type" .= ("tool" :: Text), "name" .= n]
        ]

encodeAnthropicMessages :: [ChatMessage] -> [Value]
encodeAnthropicMessages = go
  where
    go [] = []
    go (m : rest) = case m of
      SystemMsg _ -> go rest -- system travels in the top-level field
      ToolMsg _ _ ->
        let (toolMsgs, rest') = spanToolMsgs (m : rest)
         in object ["role" .= ("user" :: Text), "content" .= map toolResultBlock toolMsgs] : go rest'
      _ -> encodeOne m : go rest
    spanToolMsgs = span isToolMsg
    isToolMsg (ToolMsg _ _) = True
    isToolMsg _ = False

encodeOne :: ChatMessage -> Value
encodeOne m = case m of
  UserMsg t ->
    object ["role" .= ("user" :: Text), "content" .= [textBlock t]]
  AssistantMsg t calls ->
    object
      [ "role" .= ("assistant" :: Text)
      , "content" .= (catMaybes [if T.null t then Nothing else Just (textBlock t)] ++ map toolUseBlock calls)
      ]
  _ -> object ["role" .= ("user" :: Text), "content" .= [textBlock ("" :: Text)]]
  where
    textBlock (t :: Text) = object ["type" .= ("text" :: Text), "text" .= t]
    toolUseBlock c =
      object
        [ "type" .= ("tool_use" :: Text)
        , "id" .= toolCallId c
        , "name" .= toolCallName c
        , "input" .= toolCallArguments c
        ]

toolResultBlock :: ChatMessage -> Value
toolResultBlock (ToolMsg cid c) =
  object
    [ "type" .= ("tool_result" :: Text)
    , "tool_use_id" .= cid
    , "content" .= c
    ]
toolResultBlock _ = Null

--------------------------------------------------------------------------------
-- Response parsing
--------------------------------------------------------------------------------

parseMessagesResponse :: ByteString -> Either LLMError CompletionResponse
parseMessagesResponse body = do
  resp <- eitherDecodeLenient body
  let blocks = antContent resp
      texts = [t | b <- blocks, Just t <- [antBlockText b]]
      isSynthetic b = antBlockName b == Just structuredToolName
      calls =
        [ ToolCall
            { toolCallId = fromMaybe "" (antBlockId b)
            , toolCallName = fromMaybe "" (antBlockName b)
            , toolCallArguments = fromMaybe Null (antBlockInput b)
            }
        | b <- blocks
        , antBlockType b == Just "tool_use"
        , not (isSynthetic b)
        ]
      -- Structured output arrives inside the forced tool_use block.
      structuredPayload = case [inp | b <- blocks, antBlockType b == Just "tool_use", isSynthetic b, Just inp <- [antBlockInput b]] of
        (inp : _) -> Just inp
        [] -> Nothing
      rawStop = maybe FrStop stopFromText (antStopReason resp)
      finishReason = if null calls && rawStop == FrToolUse then FrStop else rawStop
  if null blocks
    then Left NoAssistantMessage
    else
      Right
        CompletionResponse
          { crspText = T.concat texts
          , crspToolCalls = calls
          , crspStructuredPayload = structuredPayload
          , crspFinishReason = finishReason
          , crspUsage = fmap convUsage (antUsage resp)
          }
  where
    eitherDecodeLenient bs =
      either
        (\err -> Left (DecodeError err (decodeUtf8With lenientDecode (LBS.toStrict bs))))
        Right
        (A.eitherDecode' bs)
    convUsage u = Usage (fromMaybe 0 (antUsageInput u)) (fromMaybe 0 (antUsageOutput u))

stopFromText :: Text -> FinishReason
stopFromText t = case t of
  "end_turn" -> FrStop
  "stop_sequence" -> FrStop
  "tool_use" -> FrToolUse
  "max_tokens" -> FrLength
  "refusal" -> FrContentFilter
  other -> FrOther other

data AntResp = AntResp
  { antContent :: [AntBlock]
  , antStopReason :: Maybe Text
  , antUsage :: Maybe AntUsage
  }

instance A.FromJSON AntResp where
  parseJSON = withObject "messages-response" $ \o ->
    AntResp
      <$> o .:? "content" .!= []
      <*> o .:? "stop_reason"
      <*> o .:? "usage"

data AntBlock = AntBlock
  { antBlockType :: Maybe Text
  , antBlockText :: Maybe Text
  , antBlockId :: Maybe Text
  , antBlockName :: Maybe Text
  , antBlockInput :: Maybe Value
  }

instance A.FromJSON AntBlock where
  parseJSON = withObject "content-block" $ \o ->
    AntBlock
      <$> o .:? "type"
      <*> o .:? "text"
      <*> o .:? "id"
      <*> o .:? "name"
      <*> o .:? "input"

data AntUsage = AntUsage
  { antUsageInput :: Maybe Int
  , antUsageOutput :: Maybe Int
  }

instance A.FromJSON AntUsage where
  parseJSON = withObject "usage" $ \o ->
    AntUsage <$> o .:? "input_tokens" <*> o .:? "output_tokens"

--------------------------------------------------------------------------------
-- Streaming state machine (pure, testable)
--------------------------------------------------------------------------------

data AntBlockAcc
  = AntTextAcc Text
  | AntToolAcc (Maybe Text) (Maybe Text) Text -- id, name, partial JSON
  deriving (Eq, Show)

-- | Accumulated state while consuming streamed events.
data AntStreamState = AntStreamState
  { asBlocks :: IntMap AntBlockAcc
  , asStop :: Maybe FinishReason
  , asInputTokens :: Int
  , asOutputTokens :: Int
  , asError :: Maybe LLMError
  , asFinished :: Bool
  }
  deriving (Eq, Show)

initialAntStreamState :: AntStreamState
initialAntStreamState = AntStreamState IM.empty Nothing 0 0 Nothing False

-- | Fold one SSE payload into the state, emitting new text deltas.
handleAnthropicEvent :: AntStreamState -> Text -> (AntStreamState, [StreamEvent])
handleAnthropicEvent st payload = case A.eitherDecode' (LBS.fromStrict (encodeUtf8 payload)) of
  Left _ -> (st, [])
  Right ev -> case antEventType ev of
    Just "message_start" ->
      (st {asInputTokens = maybe (asInputTokens st) id (antEvInputTokens ev)}, [])
    Just "content_block_start" ->
      let idx = fromMaybe 0 (antEvIndex ev)
          acc = case antEvBlock ev of
            Just blk
              | antBlockType blk == Just "tool_use" ->
                  AntToolAcc (antBlockId blk) (antBlockName blk) ""
            _ -> AntTextAcc ""
       in (st {asBlocks = IM.insert idx acc (asBlocks st)}, [])
    Just "content_block_delta" ->
      let idx = fromMaybe 0 (antEvIndex ev)
       in case antEvDelta ev of
            Just d
              | antDeltaType d == Just "text_delta"
              , Just t <- antDeltaText d ->
                  ( appendText st idx t
                  , [SEText t]
                  )
              | antDeltaType d == Just "input_json_delta"
              , Just pj <- antDeltaPartialJson d ->
                  (appendJson st idx pj, [])
            _ -> (st, [])
    Just "message_delta" ->
      ( st
          { asStop = maybe (asStop st) (Just . stopFromText) (antEvStopReason ev)
          , asOutputTokens = maybe (asOutputTokens st) id (antEvOutputTokens ev)
          }
      , []
      )
    Just "message_stop" ->
      (st {asFinished = True}, [])
    Just "error" ->
      (st {asError = Just (ApiError 500 (renderCompact (antEvErrorBody ev)))}, [])
    _ -> (st, [])
  where
    appendText s idx t =
      let upd = case IM.lookup idx (asBlocks s) of
            Just (AntTextAcc prev) -> AntTextAcc (prev <> t)
            _ -> AntTextAcc t
       in s {asBlocks = IM.insert idx upd (asBlocks s)}
    appendJson s idx pj =
      let upd = case IM.lookup idx (asBlocks s) of
            Just (AntToolAcc i n prev) -> AntToolAcc i n (prev <> pj)
            _ -> AntToolAcc Nothing Nothing pj
       in s {asBlocks = IM.insert idx upd (asBlocks s)}

finalizeAnthropicStream :: AntStreamState -> Either LLMError CompletionResponse
finalizeAnthropicStream st = case asError st of
  Just e -> Left e
  Nothing
    | not (asFinished st) && isNothing (asStop st) ->
        Left (HttpError "Stream ended prematurely without message_stop or stop_reason")
    | otherwise ->
        let ordered = [b | (_, b) <- IM.toAscList (asBlocks st)]
            texts = [t | AntTextAcc t <- ordered, not (T.null t)]
            isSynthetic mn = mn == Just structuredToolName
            calls =
              [ ToolCall
                  { toolCallId = fromMaybe "" mid
                  , toolCallName = fromMaybe "" mn
                  , toolCallArguments = fromMaybe Null (parseJsonText pj)
                  }
              | AntToolAcc mid mn pj <- ordered
              , not (isSynthetic mn)
              ]
            structuredPayload = case [v | AntToolAcc _ mn pj <- ordered, isSynthetic mn, Just v <- [parseJsonText pj]] of
              (v : _) -> Just v
              [] -> Nothing
            rawStop = fromMaybe FrStop (asStop st)
            finishReason = if null calls && rawStop == FrToolUse then FrStop else rawStop
         in Right
              CompletionResponse
                { crspText = T.concat texts
                , crspToolCalls = calls
                , crspStructuredPayload = structuredPayload
                , crspFinishReason = finishReason
                , crspUsage = Just (Usage (asInputTokens st) (asOutputTokens st))
                }
  where
    parseJsonText "" = Nothing
    parseJsonText t = case extractJsonValue t of
      Right v -> Just v
      Left _ -> Nothing

extractJsonValue :: Text -> Either String Value
extractJsonValue t = A.eitherDecode' (LBS.fromStrict (encodeUtf8 t))

--------------------------------------------------------------------------------
-- Event shapes
--------------------------------------------------------------------------------

data AntEvent = AntEvent
  { antEventType :: Maybe Text
  , antEvIndex :: Maybe Int
  , antEvBlock :: Maybe AntBlock
  , antEvDelta :: Maybe AntDelta
  , antEvStopReason :: Maybe Text
  , antEvInputTokens :: Maybe Int
  , antEvOutputTokens :: Maybe Int
  , antEvErrorBody :: Value
  }

instance A.FromJSON AntEvent where
  parseJSON v = withObject "event" inner v
    where
      inner o =
        AntEvent
          <$> o .:? "type"
          <*> o .:? "index"
          <*> o .:? "content_block"
          <*> o .:? "delta"
          <*> (o .:? "delta" >>= maybe (pure Nothing) (.:? "stop_reason"))
          <*> pickToken o "input_tokens"
          <*> pickToken o "output_tokens"
          <*> (o .:? "error" .!= Null)

-- Usage appears top-level on @message_delta@ and nested under @\"message\"@
-- on @message_start@.
pickToken :: A.Object -> Text -> A.Parser (Maybe Int)
pickToken o key = do
  direct <- o .:? "usage"
  nested <- o .:? "message"
  case (direct, nested) of
    (Just uu, _) -> uu .:? Key.fromText key
    (Nothing, Just (A.Object mo)) -> mo .:? "usage" >>= maybe (pure Nothing) (.:? Key.fromText key)
    _ -> pure Nothing

data AntDelta = AntDelta
  { antDeltaType :: Maybe Text
  , antDeltaText :: Maybe Text
  , antDeltaPartialJson :: Maybe Text
  }

instance A.FromJSON AntDelta where
  parseJSON = withObject "delta" $ \o ->
    AntDelta
      <$> o .:? "type"
      <*> o .:? "text"
      <*> o .:? "partial_json"

--------------------------------------------------------------------------------
-- IO shells
--------------------------------------------------------------------------------

completeWith :: AnthropicConfig -> CompletionRequest -> IO (Either LLMError CompletionResponse)
completeWith cfg req = do
  r <-
    postJSON
      (messagesUrl cfg)
      (authHeaders cfg ++ acExtraHeaders cfg)
      (acTimeoutSeconds cfg)
      (buildMessagesBody cfg req)
  pure $ case r of
    Left e -> Left e
    Right (_, _, bodyBs) -> parseMessagesResponse bodyBs

streamWith ::
  AnthropicConfig ->
  CompletionRequest ->
  (StreamEvent -> IO ()) ->
  IO (Either LLMError CompletionResponse)
streamWith cfg req cb = do
  stateRef <- newIORef initialAntStreamState
  parserRef <- newIORef newSSEParser
  let baseBody = buildMessagesBody cfg req
      body = case baseBody of
        Object o -> Object (KM.insert "stream" (Bool True) o)
        v -> v
  r <-
    postJSONStream
      (messagesUrl cfg)
      (authHeaders cfg ++ acExtraHeaders cfg)
      (acTimeoutSeconds cfg)
      body
      (onChunk stateRef parserRef)
  case r of
    Left e -> pure (Left e)
    Right () -> do
      parser <- readIORef parserRef
      mapM_ (feedEvent stateRef) (finishSSE parser)
      st <- readIORef stateRef
      case finalizeAnthropicStream st of
        Left e -> pure (Left e)
        Right resp -> do
          cb (SEFinished resp)
          pure (Right resp)
  where
    onChunk stateRef parserRef chunk = do
      parser <- readIORef parserRef
      let (parser', events) = stepSSE parser chunk
      writeIORef parserRef parser'
      mapM_ (feedEvent stateRef) events

    feedEvent stateRef payload = do
      st <- readIORef stateRef
      let (st', evts) = handleAnthropicEvent st payload
      writeIORef stateRef st'
      mapM_ cb evts
