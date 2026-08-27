{- | Line and path matching shared by the World backends so the local-disk
and in-memory interpreters answer 'SearchFiles' identically.
-}
module LLMonad.World.Match (
    matchLine,
    matchesPathFilters,
    simpleGlobMatch,
    skippedDirNames,
    isSkippedDirName,
    pathHasSkippedDir,
) where

import Data.List (tails)
import Data.Text (Text)
import Data.Text qualified as T
import System.FilePath (splitDirectories)

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

{- | Apply SearchFiles include/exclude filters to one relative path.

Each pattern matches two ways: entries without @*@ or @?@ are plain
substrings of the path (preserving the historical behavior for models
that send bare extensions like @\".hs\"@), while entries carrying
metacharacters are anchored globs over the whole path -- @*@ spans any
run including separators, @?@ exactly one character. An empty include
list admits everything; any matching exclude rejects.
-}
matchesPathFilters :: [Text] -> [Text] -> Text -> Bool
matchesPathFilters includes excludes rel =
    let hasInclude = null includes || any (pathMatchesPattern rel) includes
        hasExclude = not (null excludes) && any (pathMatchesPattern rel) excludes
     in hasInclude && not hasExclude

{- | One include\/exclude entry against one path: anchored glob when the
pattern carries metacharacters, otherwise plain containment.
-}
pathMatchesPattern :: Text -> Text -> Bool
pathMatchesPattern rel pattern
    | any (`elem` ("*?" :: String)) pat = wildMatch pat (T.unpack rel)
    | otherwise = pattern `T.isInfixOf` rel
  where
    pat = T.unpack pattern

{- | Wildcard glob matching supporting '*' spanning any run and '?'
exactly one character, anywhere in the line.
-}
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

{- | Anchored variant used for whole-path filters: the pattern must cover
the entire path, which is what \"include '*.hs'\" intends.
-}
wildMatch :: String -> String -> Bool
wildMatch [] [] = True
wildMatch ('*' : ps) cs =
    wildMatch ps cs
        || case cs of
            [] -> False
            (_ : more) -> wildMatch ('*' : ps) more
wildMatch ('?' : ps) (_ : more) = wildMatch ps more
wildMatch (p : remaining) (c : more)
    | p == c = wildMatch remaining more
wildMatch _ _ = False

{- | Directory names that neither backend surfaces or descends into.
Everything here is VCS state or build output, never project source;
keeping the list next to the shared matchers pins local-disk and
in-memory listings to identical shape.
-}
skippedDirNames :: [FilePath]
skippedDirNames = [".git", "dist-newstyle", ".agents"]

-- | Does this directory name belong to 'skippedDirNames'?
isSkippedDirName :: FilePath -> Bool
isSkippedDirName name = name `elem` skippedDirNames

{- | Does any component of this path fall under 'skippedDirNames'?
Memory-world keys and native paths share this predicate so searches,
finds, and listings agree across backends.
-}
pathHasSkippedDir :: FilePath -> Bool
pathHasSkippedDir path = any isSkippedDirName (splitDirectories path)
