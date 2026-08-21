{-# LANGUAGE OverloadedStrings #-}

-- | A tiny, pure, incremental parser for @text/event-stream@ (SSE) bodies,
-- per the WHATWG event-source framing: lines, @data:@@ fields, blank-line
-- event dispatch, @[DONE]@ pass-through.
--
-- Pure and chunk-agnostic on purpose: chunks may split anywhere — mid-line,
-- mid-field, even mid multi-byte UTF-8 character (the buffer is bytes;
-- decoding happens only on complete lines).
module LLMonad.Internal.SSE
  ( SSEParser
  , newSSEParser
  , stepSSE
  , finishSSE
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

-- | Accumulated parser state: bytes not yet consumed plus pending data lines.
newtype SSEParser = SSEParser ByteString
  deriving (Eq)

-- | A fresh parser.
newSSEParser :: SSEParser
newSSEParser = SSEParser BS.empty

-- | Feed a chunk; get back completed event payloads (the joined @data:@@
-- lines of each dispatched event) and the new state.
stepSSE :: SSEParser -> ByteString -> (SSEParser, [Text])
stepSSE (SSEParser buf0) chunk =
  let (completeLines, rest) = takeLines (buf0 <> chunk)
   in (SSEParser rest, dispatch completeLines)

-- | Flush at end of stream: emit a trailing event if the peer forgot the
-- final blank line.
finishSSE :: SSEParser -> [Text]
finishSSE (SSEParser buf)
  | BS.null buf = []
  | otherwise = dispatch [buf]

-- Split off all complete newline-terminated lines; return them (without
-- terminators) plus the remainder.
takeLines :: ByteString -> ([ByteString], ByteString)
takeLines = go []
  where
    go acc bs = case BS.elemIndex 10 bs of
      Nothing -> (reverse acc, bs)
      Just i ->
        let (ln, rest) = BS.splitAt i bs
         in go (stripCr ln : acc) (BS.drop 1 rest)
    stripCr l = if not (BS.null l) && BS.last l == 13 then BS.init l else l

-- Turn a list of raw lines into dispatched event payloads.
dispatch :: [ByteString] -> [Text]
dispatch lines0 = reverse (go lines0 [] [])
  where
    -- accEvents: finished payloads (reversed); accData: current data lines (reversed)
    go [] curData accEvents =
      case reverse curData of
        [] -> accEvents
        ds -> T.intercalate "\n" ds : accEvents
    go (ln : rest) curData accEvents
      | BS.null ln = flush rest curData accEvents
      | BS.head ln == 58 = go rest curData accEvents -- ':' comment
      | otherwise =
          case breakColon ln of
            Nothing -> go rest curData accEvents
            Just (field, val)
              | field == "data" -> go rest (stripLeadingSpace val : curData) accEvents
              | otherwise -> go rest curData accEvents -- event:, id:, retry: ignored
    flush rest curData accEvents =
      case reverse curData of
        [] -> go rest [] accEvents
        ds -> go rest [] (T.intercalate "\n" ds : accEvents)

breakColon :: ByteString -> Maybe (Text, Text)
breakColon bs = case BS.elemIndex 58 bs of
  Nothing -> Nothing
  Just i ->
    let f = decodeLenient (BS.take i bs)
        v = decodeLenient (BS.drop (i + 1) bs)
     in Just (f, v)

stripLeadingSpace :: Text -> Text
stripLeadingSpace t = case T.uncons t of
  Just (' ', rest) -> rest
  _ -> t

decodeLenient :: ByteString -> Text
decodeLenient = decodeUtf8With lenientDecode
