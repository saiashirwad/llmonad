{-# LANGUAGE OverloadedStrings #-}

{- | Shared matcher semantics plus local-vs-memory behavioral parity for the
World backends: same tree, same listings, same searches.
-}
module LLMonad.WorldMatchSpec (spec) where

import Data.List (sort)
import qualified Data.Text as T
import Effectful (runEff)
import LLMonad.World (
    DirEntry (..),
    SearchMatch (..),
    SearchOptions (..),
    listDirectory,
    searchFiles,
 )
import LLMonad.World.Local (runWorldLocal)
import LLMonad.World.Match (matchesPathFilters, pathHasSkippedDir)
import LLMonad.World.Memory (runWorldMemoryWithFiles)
import qualified System.Directory as SD
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

gitMarker, readmeMarker, appMarker :: String
gitMarker = "GITSECRETMARK"
readmeMarker = "READMEMARK"
appMarker = "APPMARK"

memoryTree :: [(FilePath, T.Text)]
memoryTree =
    [ ("README.md", T.pack (readmeMarker <> "\nproject blurb\n"))
    , ("src/App.hs", "main = pure ()\n" <> T.pack appMarker <> "\n")
    , (".git/config", T.pack (gitMarker <> "\n"))
    ]

optsFor :: String -> SearchOptions
optsFor query =
    SearchOptions
        { soQuery = T.pack query
        , soSearchDir = "."
        , soIsRegex = False
        , soCaseInsensitive = True
        , soMaxMatches = Nothing
        , soIncludes = []
        , soExcludes = []
        }

buildLocalTree :: FilePath -> IO ()
buildLocalTree tmp = do
    SD.createDirectoryIfMissing False (tmp </> "src")
    SD.createDirectoryIfMissing False (tmp </> ".git")
    writeFile (tmp </> "README.md") (readmeMarker <> "\nproject blurb\n")
    writeFile (tmp </> "src" </> "App.hs") ("main = pure ()\n" <> appMarker <> "\n")
    writeFile (tmp </> ".git" </> "config") gitMarker

{- | Canonical temp roots can surface './'-prefixed relatives on macOS;
normalizing keeps both backends byte-comparable.
-}
normHits :: [(FilePath, Int)] -> [(FilePath, Int)]
normHits =
    map $ \(path, n) ->
        case T.stripPrefix "./" (T.pack path) of
            Just stripped -> (T.unpack stripped, n)
            Nothing -> (path, n)

localHitsFor :: String -> IO [(FilePath, Int)]
localHitsFor marker =
    withSystemTempDirectory "worldmatch-parity" $ \tmp -> do
        buildLocalTree tmp
        runEff . runWorldLocal tmp . fmap (normHits . map hitPair) $
            searchFiles (optsFor marker)

memoryHitsFor :: String -> IO [(FilePath, Int)]
memoryHitsFor marker =
    fst
        <$> ( runEff
                . runWorldMemoryWithFiles memoryTree
                . fmap (map hitPair)
                $ searchFiles (optsFor marker)
            )

hitPair :: SearchMatch -> (FilePath, Int)
hitPair m = (smFile m, smLineNumber m)

spec :: Spec
spec = do
    describe "LLMonad.World.Match path filter patterns" $ do
        it "wildcard patterns are anchored globs over the whole relative path" $ do
            matchesPathFilters ["*.hs"] [] "src/App.hs" `shouldBe` True
            matchesPathFilters ["*.txt"] [] "src/App.hs" `shouldBe` False
            matchesPathFilters ["src/*.hs"] [] "src/App.hs" `shouldBe` True
        -- A bare 'App.hs' has no metacharacters, so legacy substring rules
        -- apply and it does match; only metachar-carrying entries anchor.

        it "bare extensions keep the historical substring behavior" $ do
            matchesPathFilters [".hs"] [] "src/App.hs" `shouldBe` True
            matchesPathFilters [] [".hs"] "src/App.hs" `shouldBe` False

        it "question marks in anchored globs match exactly one character at that spot" $ do
            matchesPathFilters ["docs/readme.?md"] [] "docs/readme.1md" `shouldBe` True
            matchesPathFilters ["docs/readme.?md"] [] "docs/readme.12md" `shouldBe` False
            matchesPathFilters ["readme.?md"] [] "docs/readme.1md" `shouldBe` False

        it "skipped directory segments are detectable in full paths" $ do
            pathHasSkippedDir ".git/config" `shouldBe` True
            pathHasSkippedDir "src/App.hs" `shouldBe` False
            pathHasSkippedDir ".github/workflows/ci.yml" `shouldBe` False

    describe "Local vs Memory backend parity" $ do
        it "lists identical root entries from both interpreters" $ do
            localNames <-
                withSystemTempDirectory "worldmatch-parity" $ \tmp -> do
                    buildLocalTree tmp
                    runEff . runWorldLocal tmp . fmap (sort . map deName) $ listDirectory "."
            memoryNames <-
                fst
                    <$> ( runEff
                            . runWorldMemoryWithFiles memoryTree
                            . fmap (sort . map deName)
                            $ listDirectory "."
                        )
            sort localNames `shouldBe` sort memoryNames
            localNames `shouldContain` ["README.md", "src"]
            any (== ".git") localNames `shouldBe` False

        it "searches return identical hits from both interpreters" $ do
            localHits <- localHitsFor appMarker
            memoryHits <- memoryHitsFor appMarker
            localHits `shouldBe` [("src/App.hs", 2)]
            memoryHits `shouldBe` [("src/App.hs", 2)]

        it "skips VCS directories identically: the marker inside .git is invisible to both" $ do
            localHits <- localHitsFor gitMarker
            memoryHits <- memoryHitsFor gitMarker
            localHits `shouldBe` []
            memoryHits `shouldBe` []
