{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Comprehensive test suite for Standard Coding Tools (Milestone 3 / R3).
module LLMonad.CodingToolsSpec (spec) where

import Data.Aeson (Result (..), fromJSON, toJSON)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful
import LLMonad
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "LLMonad.Tools.Coding (Milestone 3)" $ do
    describe "1. viewFileTool" $ do
        it "reads file contents with line slicing in pure memory" $ do
            let sampleText = "line 1\nline 2\nline 3\nline 4\nline 5\n"
            let st = initMemoryWorld [("src/Main.hs", sampleText)]
            (res, _) <- runEff $ runWorldMemory st $ do
                runViewFile (ViewFileArgs "src/Main.hs" (Just 2) (Just 4) Nothing)

            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right vfr -> do
                    vfrPath vfr `shouldBe` "src/Main.hs"
                    vfrTotalLines vfr `shouldBe` 5
                    vfrStartLine vfr `shouldBe` 2
                    vfrEndLine vfr `shouldBe` 4
                    map lineText (vfrLines vfr) `shouldBe` ["line 2", "line 3", "line 4"]
                    map lineIndex (vfrLines vfr) `shouldBe` [2, 3, 4]
                    vfrIsTruncated vfr `shouldBe` False

        it "clamps line slice to a maximum of 800 lines" $ do
            let manyLines = T.unlines ["line " <> T.pack (show i) | i <- [1 .. 1000 :: Int]]
            let st = initMemoryWorld [("large.txt", manyLines)]
            (res, _) <- runEff $ runWorldMemory st $ do
                runViewFile (ViewFileArgs "large.txt" (Just 1) (Just 1000) Nothing)

            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right vfr -> do
                    vfrTotalLines vfr `shouldBe` 1000
                    length (vfrLines vfr) `shouldBe` 800
                    vfrStartLine vfr `shouldBe` 1
                    vfrEndLine vfr `shouldBe` 800

        it "accurately sets vfrEndLine matching the last truncated line when byte limit is hit" $ do
            let lineTextContent = T.replicate 100 "A"
            let manyLines = T.unlines (replicate 600 lineTextContent)
            let st = initMemoryWorld [("large.txt", manyLines)]
            (res, _) <- runEff $ runWorldMemory st $ do
                runViewFile (ViewFileArgs "large.txt" (Just 1) (Just 600) Nothing)

            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right vfr -> do
                    vfrIsTruncated vfr `shouldBe` True
                    let count = length (vfrLines vfr)
                    vfrEndLine vfr `shouldBe` count
                    vfrEndLine vfr `shouldBe` lineIndex (last (vfrLines vfr))

        it "returns error when file does not exist" $ do
            let st = initMemoryWorld []
            (res, _) <- runEff $ runWorldMemory st $ do
                runViewFile (ViewFileArgs "missing.hs" Nothing Nothing Nothing)

            case res of
                Left err -> err `shouldSatisfy` ("File not found" `T.isInfixOf`)
                Right _ -> expectationFailure "Expected missing file error"

        it "returns error when target path is a directory" $ do
            let st = initMemoryWorld [("dir/file.txt", "content")]
            (res, _) <- runEff $ runWorldMemory st $ do
                runViewFile (ViewFileArgs "dir" Nothing Nothing Nothing)

            case res of
                Left err -> err `shouldSatisfy` ("directory" `T.isInfixOf`)
                Right _ -> expectationFailure "Expected directory error"

        it "detects binary content and prevents text slicing" $ do
            let binaryData = "some\0binary\0bytes"
            let st = initMemoryWorld [("app.bin", binaryData)]
            (res, _) <- runEff $ runWorldMemory st $ do
                runViewFile (ViewFileArgs "app.bin" Nothing Nothing Nothing)

            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right vfr -> do
                    map lineText (vfrLines vfr) `shouldBe` ["<binary content>"]

        it "executes through viewFileTool interface and JSON serialization" $ do
            let st = initMemoryWorld [("test.txt", "hello\nworld\n")]
            (toolRes, _) <- runEff $ runWorldMemory st $ do
                toolRun viewFileTool (toJSON (ViewFileArgs "test.txt" (Just 1) (Just 2) Nothing))

            case toolRes of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right val -> case fromJSON @ViewFileResult val of
                    Success vfr -> map lineText (vfrLines vfr) `shouldBe` ["hello", "world"]
                    Error err -> expectationFailure ("JSON decode failed: " <> err)

    describe "2. editFileTool" $ do
        it "performs exact single string replacement in file" $ do
            let original = "function foo() {\n  return 1;\n}\n"
            let st = initMemoryWorld [("src/app.js", original)]
            (res, finalSt) <- runEff $ runWorldMemory st $ do
                runEditFile (EditFileArgs "src/app.js" (Just "update return") "return 1;" "return 42;" Nothing Nothing Nothing)

            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right efr -> do
                    efrReplacedCount efr `shouldBe` 1
                    efrLinesModified efr `shouldBe` [2]
                    efrDiffSnippet efr `shouldSatisfy` ("-   return 1;" `T.isInfixOf`)
                    efrDiffSnippet efr `shouldSatisfy` ("+   return 42;" `T.isInfixOf`)
                    Map.lookup "src/app.js" (mwsFiles finalSt) `shouldBe` Just "function foo() {\n  return 42;\n}\n"

        it "creates new file when targetContent is empty and file does not exist" $ do
            let st = initMemoryWorld []
            (res, finalSt) <- runEff $ runWorldMemory st $ do
                runEditFile (EditFileArgs "new_file.txt" (Just "create") "" "brand new content\n" Nothing Nothing Nothing)

            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right efr -> do
                    efrReplacedCount efr `shouldBe` 1
                    Map.lookup "new_file.txt" (mwsFiles finalSt) `shouldBe` Just "brand new content\n"

        it "returns error when targetContent is not found" $ do
            let st = initMemoryWorld [("doc.txt", "alpha beta gamma")]
            (res, _) <- runEff $ runWorldMemory st $ do
                runEditFile (EditFileArgs "doc.txt" Nothing "delta" "omega" Nothing Nothing Nothing)

            case res of
                Left err -> err `shouldSatisfy` ("not found" `T.isInfixOf`)
                Right _ -> expectationFailure "Expected not found error"

        it "rejects ambiguous multiple occurrences when allowMultiple is False" $ do
            let st = initMemoryWorld [("repeat.txt", "var x = 1;\nvar y = 1;\nvar z = 1;\n")]
            (res, _) <- runEff $ runWorldMemory st $ do
                runEditFile (EditFileArgs "repeat.txt" Nothing "1;" "2;" Nothing Nothing (Just False))

            case res of
                Left err -> err `shouldSatisfy` ("matched 3 times" `T.isInfixOf`)
                Right _ -> expectationFailure "Expected multiple match error"

        it "replaces all occurrences when allowMultiple is True" $ do
            let st = initMemoryWorld [("repeat.txt", "var x = 1;\nvar y = 1;\nvar z = 1;\n")]
            (res, finalSt) <- runEff $ runWorldMemory st $ do
                runEditFile (EditFileArgs "repeat.txt" Nothing "1;" "2;" Nothing Nothing (Just True))

            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right efr -> do
                    efrReplacedCount efr `shouldBe` 3
                    Map.lookup "repeat.txt" (mwsFiles finalSt) `shouldBe` Just "var x = 2;\nvar y = 2;\nvar z = 2;\n"

        it "constrains replacements to specified line bounds" $ do
            let content = "item = 10\nitem = 10\nitem = 10\n"
            let st = initMemoryWorld [("bounded.txt", content)]
            (res, finalSt) <- runEff $ runWorldMemory st $ do
                -- Target only line 2
                runEditFile (EditFileArgs "bounded.txt" Nothing "10" "99" (Just 2) (Just 2) Nothing)

            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right efr -> do
                    efrReplacedCount efr `shouldBe` 1
                    efrLinesModified efr `shouldBe` [2]
                    Map.lookup "bounded.txt" (mwsFiles finalSt) `shouldBe` Just "item = 10\nitem = 99\nitem = 10\n"

        it "computes unified diff snippet and modified line numbers accurately" $ do
            let oldTxt = "line 1\nold line 2\nline 3\n"
            let newTxt = "line 1\nnew line 2\nline 3\n"
            let snippet = computeDiffSnippet "test.txt" oldTxt newTxt
            let linesMod = computeModifiedLines oldTxt newTxt
            snippet `shouldSatisfy` ("--- a/test.txt" `T.isInfixOf`)
            snippet `shouldSatisfy` ("- old line 2" `T.isInfixOf`)
            snippet `shouldSatisfy` ("+ new line 2" `T.isInfixOf`)
            linesMod `shouldBe` [2]

        it "executes through editFileTool interface" $ do
            let st = initMemoryWorld [("config.env", "PORT=8080\n")]
            (toolRes, _) <- runEff $ runWorldMemory st $ do
                toolRun editFileTool (toJSON (EditFileArgs "config.env" Nothing "8080" "3000" Nothing Nothing Nothing))

            case toolRes of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right val -> case fromJSON @EditFileResult val of
                    Success efr -> efrReplacedCount efr `shouldBe` 1
                    Error err -> expectationFailure ("JSON decode failed: " <> err)

    describe "3. grepSearchTool" $ do
        it "searches file contents across workspace" $ do
            let files =
                    [ ("src/A.hs", "import Data.Text\nmain = print \"hello\"\n")
                    , ("src/B.hs", "import Data.Map\nrun = print \"hello world\"\n")
                    , ("test/Spec.hs", "main = pure ()\n")
                    ]
            let st = initMemoryWorld files
            (res, _) <- runEff $ runWorldMemory st $ do
                runGrepSearch (GrepSearchArgs "hello" (Just "src") Nothing Nothing Nothing (Just True) Nothing)

            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right gsr -> do
                    gsrTotalCount gsr `shouldBe` 2
                    map gmFilePath (gsrMatches gsr) `shouldBe` ["src/A.hs", "src/B.hs"]
                    map gmLineNumber (gsrMatches gsr) `shouldBe` [Just 2, Just 2]

        it "supports case-insensitive search" $ do
            let st = initMemoryWorld [("notes.txt", "IMPORTANT NOTICE\nimportant text\n")]
            (res, _) <- runEff $ runWorldMemory st $ do
                runGrepSearch (GrepSearchArgs "notice" Nothing Nothing (Just True) Nothing (Just True) Nothing)

            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right gsr -> do
                    gsrTotalCount gsr `shouldBe` 1
                    map gmLineNumber (gsrMatches gsr) `shouldBe` [Just 1]

        it "supports regex and pattern matching" $ do
            let st = initMemoryWorld [("data.txt", "user_123: active\nuser_456: inactive\nadmin_999: active\n")]
            (res, _) <- runEff $ runWorldMemory st $ do
                runGrepSearch (GrepSearchArgs "user_*" Nothing (Just True) Nothing Nothing (Just True) Nothing)

            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right gsr -> do
                    gsrTotalCount gsr `shouldBe` 2

        it "respects maxMatches truncation limit" $ do
            let st = initMemoryWorld [("f.txt", "match 1\nmatch 2\nmatch 3\nmatch 4\nmatch 5\n")]
            (res, _) <- runEff $ runWorldMemory st $ do
                runGrepSearch (GrepSearchArgs "match" Nothing Nothing Nothing Nothing (Just True) (Just 2))

            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right gsr -> do
                    length (gsrMatches gsr) `shouldBe` 2
                    gsrIsTruncated gsr `shouldBe` True

        it "returns unique files when matchPerLine is False" $ do
            let st = initMemoryWorld [("f.txt", "hit\nhit\nhit\n"), ("g.txt", "hit\n")]
            (res, _) <- runEff $ runWorldMemory st $ do
                runGrepSearch (GrepSearchArgs "hit" Nothing Nothing Nothing Nothing (Just False) Nothing)

            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right gsr -> do
                    map gmFilePath (gsrMatches gsr) `shouldBe` ["f.txt", "g.txt"]
                    map gmLineNumber (gsrMatches gsr) `shouldBe` [Nothing, Nothing]

        it "executes through grepSearchTool interface" $ do
            let st = initMemoryWorld [("src/Lib.hs", "module Lib where\n")]
            (toolRes, _) <- runEff $ runWorldMemory st $ do
                toolRun grepSearchTool (toJSON (GrepSearchArgs "Lib" Nothing Nothing Nothing Nothing Nothing Nothing))

            case toolRes of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right val -> case fromJSON @GrepSearchResult val of
                    Success gsr -> gsrTotalCount gsr `shouldBe` 1
                    Error err -> expectationFailure ("JSON decode failed: " <> err)

        it "returns World directory failures to the model as tool errors" $ do
            let st = initMemoryWorld [("src/Main.hs", "module Main where")]
            (toolRes, _) <- runEff $ runWorldMemory st $ do
                toolRun grepSearchTool (toJSON (GrepSearchArgs "Main" (Just "src/Main.hs") Nothing Nothing Nothing Nothing Nothing))

            case toolRes of
                Left err -> err `shouldSatisfy` ("Directory not found" `T.isInfixOf`)
                Right _ -> expectationFailure "Expected a recoverable World tool error"

    describe "4. findByNameTool" $ do
        it "discovers files matching pattern in memory" $ do
            let files =
                    [ ("src/LLMonad/Core.hs", "core")
                    , ("src/LLMonad/World.hs", "world")
                    , ("test/Spec.hs", "spec")
                    ]
            let st = initMemoryWorld files
            (res, _) <- runEff $ runWorldMemory st $ do
                runFindByName (FindByNameArgs (Just "World") (Just "src") (Just "file") Nothing Nothing Nothing)

            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right fbr -> do
                    fbrTotalCount fbr `shouldBe` 1
                    map feiPath (fbrEntries fbr) `shouldBe` ["src/LLMonad/World.hs"]

        it "filters by type (file vs directory)" $ do
            let files =
                    [ ("app/Main.hs", "main")
                    , ("src/Lib.hs", "lib")
                    ]
            let st = initMemoryWorld files
            (resDirs, _) <- runEff $ runWorldMemory st $ do
                runFindByName (FindByNameArgs Nothing Nothing (Just "directory") (Just 1) Nothing Nothing)

            case resDirs of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right fbr -> do
                    map feiPath (fbrEntries fbr) `shouldBe` ["app", "src"]
                    map feiType (fbrEntries fbr) `shouldBe` ["directory", "directory"]

        it "executes through findByNameTool interface" $ do
            let st = initMemoryWorld [("README.md", "# Title")]
            (toolRes, _) <- runEff $ runWorldMemory st $ do
                toolRun findByNameTool (toJSON (FindByNameArgs (Just "README") Nothing Nothing Nothing Nothing Nothing))

            case toolRes of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right val -> case fromJSON @FindByNameResult val of
                    Success fbr -> fbrTotalCount fbr `shouldBe` 1
                    Error err -> expectationFailure ("JSON decode failed: " <> err)

    describe "5. listDirTool" $ do
        it "lists contents and metadata of a directory" $ do
            let files =
                    [ ("project/README.md", "# Readme")
                    , ("project/package.json", "{}")
                    , ("project/src/index.js", "console.log()")
                    ]
            let st = initMemoryWorld files
            (res, _) <- runEff $ runWorldMemory st $ do
                runListDir (ListDirArgs "project" Nothing Nothing)

            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right ldr -> do
                    ldrPath ldr `shouldBe` "project"
                    map deiName (ldrEntries ldr) `shouldBe` ["README.md", "package.json", "src"]
                    map deiIsDir (ldrEntries ldr) `shouldBe` [False, False, True]

        it "lists directory contents recursively when recursive = Just True" $ do
            let files =
                    [ ("project/README.md", "# Readme")
                    , ("project/package.json", "{}")
                    , ("project/src/index.js", "console.log()")
                    , ("project/src/util/helper.js", "export const x = 1;")
                    ]
            let st = initMemoryWorld files
            (res, _) <- runEff $ runWorldMemory st $ do
                runListDir (ListDirArgs "project" (Just True) Nothing)

            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right ldr -> do
                    let names = map deiName (ldrEntries ldr)
                    names `shouldContain` ["README.md", "package.json", "src", "src/index.js", "src/util", "src/util/helper.js"]

        it "respects maxDepth when listing directory contents" $ do
            let files =
                    [ ("project/README.md", "# Readme")
                    , ("project/src/index.js", "console.log()")
                    , ("project/src/util/helper.js", "export const x = 1;")
                    ]
            let st = initMemoryWorld files
            (res, _) <- runEff $ runWorldMemory st $ do
                runListDir (ListDirArgs "project" (Just True) (Just 1))

            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right ldr -> do
                    let names = map deiName (ldrEntries ldr)
                    names `shouldContain` ["README.md", "src"]
                    names `shouldNotContain` ["src/index.js", "src/util/helper.js"]

        it "returns error when directory does not exist" $ do
            let st = initMemoryWorld []
            (res, _) <- runEff $ runWorldMemory st $ do
                runListDir (ListDirArgs "non_existent_folder" Nothing Nothing)

            case res of
                Left err -> err `shouldSatisfy` ("Directory not found" `T.isInfixOf`)
                Right _ -> expectationFailure "Expected directory not found error"

        it "executes through listDirTool interface" $ do
            let st = initMemoryWorld [("docs/guide.md", "guide")]
            (toolRes, _) <- runEff $ runWorldMemory st $ do
                toolRun listDirTool (toJSON (ListDirArgs "docs" Nothing Nothing))

            case toolRes of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right val -> case fromJSON @ListDirResult val of
                    Success ldr -> length (ldrEntries ldr) `shouldBe` 1
                    Error err -> expectationFailure ("JSON decode failed: " <> err)

    describe "6. runCommandTool" $ do
        it "executes command synchronously and returns CommandCompleted with default bounded timeout" $ do
            let st = initMemoryWorld []
            (res, finalSt) <- runEff $ runWorldMemory st $ do
                runRunCommand (RunCommandArgs "echo hello" Nothing Nothing Nothing Nothing)

            case res of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right (CommandCompleted code stdout _ _) -> do
                    code `shouldBe` 0
                    stdout `shouldBe` "hello\n"
                    case mwsCommandHistory finalSt of
                        (cmd : _) -> cmdTimeoutMs cmd `shouldBe` Just 30000
                        [] -> expectationFailure "Expected recorded command in history"
                Right other -> expectationFailure ("Expected CommandCompleted, got: " <> show other)

        it "reports CommandTimedOut when timeout expires" $ do
            withSystemTempDirectory "llmonad-coding-cmd" $ \tmpDir -> do
                res <- runEff $ runWorldLocal tmpDir $ do
                    runRunCommand (RunCommandArgs "sleep 5" Nothing (Just 100) Nothing Nothing)

                case res of
                    Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                    Right (CommandTimedOut ms _ _) -> ms `shouldBe` 100
                    Right other -> expectationFailure ("Expected CommandTimedOut, got: " <> show other)

        it "executes through runCommandTool interface" $ do
            let st = initMemoryWorld []
            (toolRes, _) <- runEff $ runWorldMemory st $ do
                toolRun runCommandTool (toJSON (RunCommandArgs "echo ok" Nothing Nothing Nothing Nothing))

            case toolRes of
                Left err -> expectationFailure ("Unexpected error: " <> T.unpack err)
                Right val -> case fromJSON @RunCommandResult val of
                    Success (CommandCompleted code stdout _ _) -> do
                        code `shouldBe` 0
                        stdout `shouldBe` "ok\n"
                    Success other -> expectationFailure ("Unexpected result: " <> show other)
                    Error err -> expectationFailure ("JSON decode failed: " <> err)

    describe "7. Standard Coding Tools Suite Integration" $ do
        it "contains all six standard tools" $ do
            let toolNames = map (toolSpecName . toolSpec) (standardCodingTools @'[World])
            toolNames
                `shouldBe` [ "view_file"
                           , "edit_file"
                           , "grep_search"
                           , "find_by_name"
                           , "list_dir"
                           , "run_command"
                           ]

        it "runs an autonomous agent loop using standard coding tools" $ do
            let initialFiles = [("src/calc.hs", "add a b = a - b\n")]
            let worldState = initMemoryWorld initialFiles
            let script =
                    [ Right (toolResp [ToolCall "call-1" "view_file" (toJSON (ViewFileArgs "src/calc.hs" Nothing Nothing Nothing))])
                    , Right (toolResp [ToolCall "call-2" "edit_file" (toJSON (EditFileArgs "src/calc.hs" Nothing "a - b" "a + b" Nothing Nothing Nothing))])
                    , Right (textResp "Fixed subtraction bug in calc.hs")
                    ]

            ((answer, _reqs), finalWorld) <- runEff $ runWorldMemory worldState $ runLLMMock script $ do
                ans <- runTextLoop standardCodingTools "Fix subtraction bug in calc.hs"
                pure ans

            answer `shouldBe` "Fixed subtraction bug in calc.hs"
            Map.lookup "src/calc.hs" (mwsFiles finalWorld) `shouldBe` Just "add a b = a + b\n"
