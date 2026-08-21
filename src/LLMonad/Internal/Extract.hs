{-# LANGUAGE OverloadedStrings #-}

-- | Lenient extraction of JSON values from model output.
--
-- Models are told to emit bare JSON but frequently wrap it in markdown
-- fences, prepend commentary, or append footnotes. These helpers find the
-- JSON regardless.
module LLMonad.Internal.Extract
  ( stripFences
  , extractJSON
  , decodeViaJSON
  ) where

import Data.Aeson (FromJSON, Value, eitherDecode', parseEither, parseJSON)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

-- | Remove markdown code fences (@\`\`\`json ... \@ or @\`\`\` ... \@)
-- surrounding the whole payload.
stripFences :: Text -> Text
stripFences t0 =
  let t1 = T.strip t0
   in case T.stripPrefix "```" t1 of
        Nothing -> t1
        Just rest ->
          let withoutLang = T.dropWhile (/= '\n') rest
           in case T.stripSuffix "```" (T.strip withoutLang) of
                Just inner -> T.strip inner
                Nothing -> T.strip withoutLang

-- | Parse the first balanced JSON value found in the text.
--
-- Strategy: try the whole (fence-stripped) text first; then scan for the
-- first @{@ or @[@ and take the longest balanced span, respecting string
-- literals and escapes.
extractJSON :: Text -> Either String Value
extractJSON input =
  let stripped = stripFences input
      direct = eitherDecode' (LBS.fromStrict (encodeUtf8 stripped))
   in case direct of
        Right v -> Right v
        Left _ -> case scanBalanced stripped of
          Nothing -> Left "no JSON value found in output"
          Just candidate -> eitherDecode' (LBS.fromStrict (encodeUtf8 candidate))

-- | Extract a JSON value and immediately decode it into a Haskell type.
decodeViaJSON :: forall a. FromJSON a => Text -> Either String a
decodeViaJSON t = extractJSON t >>= eitherDecodeValue
  where
    eitherDecodeValue v = parseEither (parseJSON :: Value -> Either String a) v

--------------------------------------------------------------------------------
-- Balanced-span scanner
--------------------------------------------------------------------------------

-- Find the first top-level '{' or '[' and return the balanced substring.
scanBalanced :: Text -> Maybe Text
scanBalanced t = go (T.unpack t)
  where
    go [] = Nothing
    go (c : rest)
      | c == '{' || c == '[' = takeBalanced c rest
      | otherwise = go rest

-- Given an opening delimiter already consumed, consume until its match.
takeBalanced :: Char -> String -> Maybe Text
takeBalanced open rest =
  let close = if open == '{' then '}' else ']'
      walk depth inStr esc acc [] = Nothing
      walk depth inStr esc acc (c : cs)
        | esc = walk depth True False (c : acc) cs
        | c == '\\' && inStr = walk depth True True (c : acc) cs
        | c == '"' = walk depth (not inStr) False (c : acc) cs
        | not inStr && (c == '{' || c == '[') = walk (depth + 1) False False (c : acc) cs
        | not inStr && (c == '}' || c == ']') =
            if c == close && depth == 1
              then Just (T.pack (reverse (c : acc)))
              else walk (max 0 (depth - 1)) False False (c : acc) cs
        | otherwise = walk depth inStr False (c : acc) cs
   in fmap (T.cons open . T.pack . reverse) (walk (1 :: Int) False False [] rest)
