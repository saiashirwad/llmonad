{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Data.Aeson (FromJSON, Value, eitherDecode', parseJSON)
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

-- | Remove markdown code fences (@\`\`\`json ... \@ or @\`\`\` ... \@)
-- surrounding the whole payload. Handles both multi-line and single-line fences.
stripFences :: Text -> Text
stripFences t0 =
  let t1 = T.strip t0
   in case T.stripPrefix "```" t1 of
        Nothing -> t1
        Just rest ->
          if T.any (== '\n') rest
            then
              let afterFirstLine = T.drop 1 (T.dropWhile (/= '\n') rest)
                  trimmed = T.strip afterFirstLine
               in case T.stripSuffix "```" trimmed of
                    Just inner -> T.strip inner
                    Nothing -> trimmed
            else
              let inner = case T.stripSuffix "```" (T.strip rest) of
                    Just s -> T.strip s
                    Nothing -> T.strip rest
                  (tag, val) = T.span isAlphaNum inner
                  lowerTag = T.toLower tag
                  isTag = lowerTag `elem` ["json", "json5", "javascript", "js", "yaml", "yml", "text", "txt"]
               in if isTag
                    then T.strip val
                    else if not (T.null tag) && not (T.null val) && (T.head (T.strip val) `elem` ['{', '[', '"'])
                      then T.strip val
                      else inner

-- | Parse the first balanced JSON value found in the text.
--
-- Strategy: try the whole (fence-stripped) text first; then scan for the
-- first @{@, @[@, or @"@ and take the balanced span; finally scan for primitives
-- if the text does not contain unclosed structural delimiters.
extractJSON :: Text -> Either String Value
extractJSON input =
  let stripped = stripFences input
      direct = eitherDecode' (LBS.fromStrict (encodeUtf8 stripped))
   in case direct of
        Right v -> Right v
        Left _ -> case scanBalanced stripped of
          Just candidate -> eitherDecode' (LBS.fromStrict (encodeUtf8 candidate))
          Nothing
            | hasStructuralDelimiters stripped -> Left "unbalanced or truncated JSON in output"
            | otherwise -> case scanPrimitive stripped of
                Just candidate -> eitherDecode' (LBS.fromStrict (encodeUtf8 candidate))
                Nothing -> Left "no JSON value found in output"

-- | Check if the text contains structural JSON delimiters '{', '}', '[', ']'.
hasStructuralDelimiters :: Text -> Bool
hasStructuralDelimiters t = T.any (\c -> c == '{' || c == '}' || c == '[' || c == ']') t

-- | Extract a JSON value and immediately decode it into a Haskell type.
decodeViaJSON :: forall a. FromJSON a => Text -> Either String a
decodeViaJSON t = extractJSON t >>= parseEither parseJSON

--------------------------------------------------------------------------------
-- Balanced-span scanner
--------------------------------------------------------------------------------

-- Find the first top-level '{', '[', or '"' and return the balanced substring.
scanBalanced :: Text -> Maybe Text
scanBalanced t = go (T.unpack t)
  where
    go [] = Nothing
    go (c : rest)
      | c == '{' || c == '[' = takeBalanced c rest
      | c == '"' = takeQuoted rest
      | otherwise = go rest

takeQuoted :: String -> Maybe Text
takeQuoted cs = walk False [] cs
  where
    walk _ _ [] = Nothing
    walk esc acc (x : xs)
      | esc = walk False (x : acc) xs
      | x == '\\' = walk True (x : acc) xs
      | x == '"' = Just (T.pack ('"' : reverse ('"' : acc)))
      | otherwise = walk False (x : acc) xs

-- Given an opening delimiter already consumed, consume until its match.
takeBalanced :: Char -> String -> Maybe Text
takeBalanced open rest =
  let close = if open == '{' then '}' else ']'
      walk _ _ _ _ [] = Nothing
      walk depth inStr esc acc (c : cs)
        | esc = walk depth True False (c : acc) cs
        | c == '\\' && inStr = walk depth True True (c : acc) cs
        | c == '"' = walk depth (not inStr) False (c : acc) cs
        | not inStr && (c == '{' || c == '[') = walk (depth + 1) False False (c : acc) cs
        | not inStr && (c == '}' || c == ']') =
            if c == close && depth == 1
              then Just (reverse (c : acc))
              else walk (max 0 (depth - 1)) False False (c : acc) cs
        | otherwise = walk depth inStr False (c : acc) cs
   in fmap (T.cons open . T.pack) (walk (1 :: Int) False False [] rest)

--------------------------------------------------------------------------------
-- Primitive literal scanner
--------------------------------------------------------------------------------

scanPrimitive :: Text -> Maybe Text
scanPrimitive t = go (T.words t)
  where
    go [] = Nothing
    go (w : ws)
      | cleanW `elem` ["true", "false", "null"] = Just cleanW
      | isNumericWord cleanNum = Just cleanNum
      | otherwise = go ws
      where
        cleanW = stripPunct w
        cleanNum = stripTrailingPunct w

    stripPunct s =
      T.dropAround (\c -> c `elem` ['.', ',', ';', ':', '!', '?', '(', ')', '[', ']', '{', '}', '\'', '"']) s

    stripTrailingPunct s =
      T.dropWhileEnd (\c -> c `elem` ['.', ',', ';', ':', '!', '?', '(', ')', '[', ']', '{', '}', '\'', '"'])
        (T.dropWhile (\c -> c `elem` ['(', '[', '{', '\'', '"']) s)

    isNumericWord s
      | T.null s = False
      | otherwise = case reads (T.unpack s) :: [(Double, String)] of
          [(_, "")] -> True
          _ -> False
