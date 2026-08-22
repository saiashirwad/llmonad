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

-- | Accumulated parser state: bytes not yet consumed plus pending data lines in reverse order.
data SSEParser = SSEParser !ByteString ![Text]
  deriving (Eq, Show)

-- | A fresh parser.
newSSEParser :: SSEParser
newSSEParser = SSEParser BS.empty []

-- | Feed a chunk; get back completed event payloads (the joined @data:@
-- lines of each dispatched event) and the new state.
stepSSE :: SSEParser -> ByteString -> (SSEParser, [Text])
stepSSE (SSEParser buf0 pending0) chunk =
  let (completeLines, rest) = takeLines (buf0 <> chunk)
      (pending', events) = processLines completeLines pending0 []
   in (SSEParser rest pending', events)

-- | Process complete lines into (newPendingData, emittedEvents)
processLines :: [ByteString] -> [Text] -> [Text] -> ([Text], [Text])
processLines [] pending accEvents = (pending, reverse accEvents)
processLines (ln : rest) pending accEvents
  | BS.null ln =
      case reverse pending of
        [] -> processLines rest [] accEvents
        ds -> processLines rest [] (T.intercalate "\n" ds : accEvents)
  | BS.head ln == 58 =
      -- Comment line starting with ':'
      processLines rest pending accEvents
  | otherwise =
      case breakColon ln of
        Nothing ->
          if ln == "data"
            then processLines rest ("" : pending) accEvents
            else processLines rest pending accEvents
        Just (field, val)
          | field == "data" ->
              processLines rest (stripLeadingSpace val : pending) accEvents
          | otherwise ->
              -- event:, id:, retry: ignored
              processLines rest pending accEvents

-- | Flush at end of stream: emit a trailing event if the peer forgot the
-- final blank line or has a partial line in the byte buffer.
finishSSE :: SSEParser -> [Text]
finishSSE (SSEParser buf pending) =
  let pendingWithBuf =
        if BS.null buf
          then pending
          else case breakColon buf of
            Nothing ->
              if buf == "data" then "" : pending else pending
            Just (field, val)
              | field == "data" -> stripLeadingSpace val : pending
              | otherwise -> pending
   in case reverse pendingWithBuf of
        [] -> []
        ds -> [T.intercalate "\n" ds]

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
