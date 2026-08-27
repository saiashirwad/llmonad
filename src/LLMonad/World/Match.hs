{- | Line and path matching shared by the World backends so the local-disk
and in-memory interpreters answer 'SearchFiles' identically.
-}
module LLMonad.World.Match (
    matchLine,
    matchesPathFilters,
    simpleGlobMatch,
) where

import Data.List (tails)
import Data.Text (Text)
import Data.Text qualified as T

{- | Does one line of content match a search query?

Plain queries are substrings. Case-insensitive queries lower both sides
first. When the query's @isRegex@ flag is set the engine is the wildcard
glob below, not a full regex engine -- treat that flag as \"wildcard\"
until real regex support lands.
-}
matchLine :: Text -> Bool -> Bool -> Text -> Bool
matchLine query isCaseInsensitive isRegex line =
    let q = if isCaseInsensitive then T.toLower query else query
        l = if isCaseInsensitive then T.toLower line else line
     in if isRegex
            then simpleGlobMatch (T.unpack q) (T.unpack l)
            else q `T.isInfixOf` l

{- | Apply SearchFiles include/exclude filters to one relative path: an
empty include list admits everything, any listed exclude rejects, and
both lists match by substring containment.
-}
matchesPathFilters :: [Text] -> [Text] -> Text -> Bool
matchesPathFilters includes excludes rel =
    let hasInclude = null includes || any (`T.isInfixOf` rel) includes
        hasExclude = not (null excludes) && any (`T.isInfixOf` rel) excludes
     in hasInclude && not hasExclude

-- | Wildcard glob matching supporting '*' wildcards anywhere in the line.
simpleGlobMatch :: String -> String -> Bool
simpleGlobMatch pat str = any (globMatch pat) (tails str)
  where
    globMatch "" _ = True
    globMatch ('*' : ps) s =
        globMatch ps s || case s of
            "" -> False
            (_ : rest) -> globMatch ('*' : ps) rest
    globMatch (p : ps) (s : rest)
        | p == s = globMatch ps rest
        | otherwise = False
    globMatch _ _ = False
