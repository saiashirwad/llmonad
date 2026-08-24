{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Lenient extraction of JSON values from model output.

Models are told to emit bare JSON but frequently wrap it in markdown
fences, prepend commentary, or append footnotes. These helpers find the
JSON regardless.
-}
module LLMonad.Internal.Extract (
    stripFences,
    extractJSON,
    decodeViaJSON,
) where

import Data.Aeson (FromJSON, Value, eitherDecode', parseJSON)
import Data.Aeson.Types (parseEither)
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isAlphaNum)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)

{- | Remove markdown code fences (@\`\`\`json ... \@ or @\`\`\` ... \@)
surrounding the whole payload. Handles both multi-line and single-line fences.
-}
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
                                else
                                    if not (T.null tag) && not (T.null val) && (T.head (T.strip val) `elem` ['{', '[', '"'])
                                        then T.strip val
                                        else inner

{- | Parse the first valid JSON value found in the text.

Strategy:
1. Try whole fence-stripped text first.
2. Scan and try markdown code blocks (prioritizing ```json ... ``` blocks).
3. Scan all balanced candidate blocks (delimited by '{', '[', '"') in the text.
4. Try primitive literals (true, false, null, numbers).
5. If no candidate decodes and structural delimiters are present, return an error.
-}
extractJSON :: Text -> Either String Value
extractJSON input =
    let stripped = stripFences input
        direct = eitherDecode' (LBS.fromStrict (encodeUtf8 stripped))
     in case direct of
            Right v -> Right v
            Left _ ->
                let (jsonFences, otherFences) = extractFencedBlocks input
                    fencedCandidates = jsonFences ++ otherFences
                    balancedCandidates = scanStructuralBalanced stripped ++ scanStructuralBalanced input
                    quotedCandidates
                        | hasStructuralDelimiters stripped = []
                        | otherwise = scanQuotedStrings stripped
                    allCandidates = fencedCandidates ++ balancedCandidates ++ quotedCandidates
                    decodedCandidates =
                        [ v
                        | cand <- allCandidates
                        , Right v <- [eitherDecode' (LBS.fromStrict (encodeUtf8 (T.strip cand)))]
                        ]
                 in case decodedCandidates of
                        (v : _) -> Right v
                        []
                            | hasStructuralDelimiters stripped -> Left "unbalanced or truncated JSON in output"
                            | otherwise -> case scanPrimitive stripped of
                                Just primitiveCand -> eitherDecode' (LBS.fromStrict (encodeUtf8 primitiveCand))
                                Nothing -> Left "no JSON value found in output"

-- | Extract fenced code blocks from text, partitioned into (jsonBlocks, otherBlocks).
extractFencedBlocks :: Text -> ([Text], [Text])
extractFencedBlocks t = go (T.lines t) [] []
  where
    go [] jsonAcc otherAcc = (reverse jsonAcc, reverse otherAcc)
    go (l : ls) jsonAcc otherAcc
        | "```" `T.isPrefixOf` T.stripStart l =
            let restTag = T.strip (T.drop 3 (T.stripStart l))
                (tag, _) = T.span isAlphaNum restTag
                lowerTag = T.toLower tag
                isJsonTag = lowerTag `elem` ["json", "json5", "javascript", "js"]
                (blockLines, remaining) = break (\line -> "```" `T.isPrefixOf` T.stripStart line) ls
                content = T.unlines blockLines
                nextLs = case remaining of
                    [] -> []
                    (_ : rest) -> rest
             in if isJsonTag
                    then go nextLs (content : jsonAcc) otherAcc
                    else go nextLs jsonAcc (content : otherAcc)
        | otherwise = go ls jsonAcc otherAcc

-- | Check if the text contains structural JSON delimiters '{', '}', '[', ']'.
hasStructuralDelimiters :: Text -> Bool
hasStructuralDelimiters t = T.any (\c -> c == '{' || c == '}' || c == '[' || c == ']') t

-- | Extract a JSON value and immediately decode it into a Haskell type.
decodeViaJSON :: forall a. (FromJSON a) => Text -> Either String a
decodeViaJSON t = extractJSON t >>= parseEither parseJSON

--------------------------------------------------------------------------------
-- Balanced-span scanner
--------------------------------------------------------------------------------

-- | Find all balanced '{...}' and '[...]' candidates in the text.
scanStructuralBalanced :: Text -> [Text]
scanStructuralBalanced t = go (T.unpack t)
  where
    go [] = []
    go (c : rest)
        | c == '{' || c == '[' = case takeBalanced c rest of
            Just (candidate, remaining) -> candidate : go remaining
            Nothing -> []
        | otherwise = go rest

-- | Find quoted string literals in text when no structural delimiters exist.
scanQuotedStrings :: Text -> [Text]
scanQuotedStrings t = go (T.unpack t)
  where
    go [] = []
    go (c : rest)
        | c == '"' = case takeQuoted rest of
            Just candidate -> candidate : go rest
            Nothing -> go rest
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
takeBalanced :: Char -> String -> Maybe (Text, String)
takeBalanced open rest =
    let initialStack = [if open == '{' then '}' else ']']
        walk _ _ _ _ [] = Nothing
        walk [] _ _ _ restStr = Just ([], restStr)
        walk stack@(expected : restStack) inStr esc acc (c : cs)
            | esc = walk stack True False (c : acc) cs
            | c == '\\' && inStr = walk stack True True (c : acc) cs
            | c == '"' = walk stack (not inStr) False (c : acc) cs
            | inStr = walk stack True False (c : acc) cs
            | c == '{' = walk ('}' : stack) False False (c : acc) cs
            | c == '[' = walk (']' : stack) False False (c : acc) cs
            | c == '}' || c == ']' =
                if c == expected
                    then
                        if null restStack
                            then Just (reverse (c : acc), cs)
                            else walk restStack False False (c : acc) cs
                    else Nothing
            | otherwise = walk stack False False (c : acc) cs
     in case walk initialStack False False [] rest of
            Just (matched, remaining) -> Just (T.cons open (T.pack matched), remaining)
            Nothing -> Nothing

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
        T.dropWhileEnd
            (\c -> c `elem` ['.', ',', ';', ':', '!', '?', '(', ')', '[', ']', '{', '}', '\'', '"'])
            (T.dropWhile (\c -> c `elem` ['(', '[', '{', '\'', '"']) s)

    isNumericWord s
        | T.null s = False
        | otherwise = case reads (T.unpack s) :: [(Double, String)] of
            [(_, "")] -> True
            _ -> False
