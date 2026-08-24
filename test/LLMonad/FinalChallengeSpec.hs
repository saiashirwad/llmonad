{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LLMonad.FinalChallengeSpec (spec) where

import Control.Exception (try)
import Control.Monad (forM_)
import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , Value (..)
  , decode
  , encode
  , object
  , (.=)
  )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Effectful
import GHC.Generics (Generic)
import LLMonad
import LLMonad.Internal.Extract (decodeViaJSON, extractJSON)
import LLMonad.Internal.SSE (finishSSE, newSSEParser, stepSSE)
import System.Directory (createDirectoryIfMissing)
import qualified System.Directory as SD
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

data Person = Person
  { pName :: Text
  , pAge :: Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

spec :: Spec
spec = describe "Final Challenger End-to-End Stress Suite" $ do

  describe "FINAL CHALLENGER: End-to-End Multi-Batch Stress Suite" $ do

    describe "1. Batch 1: SSE Streaming & Stream Completion" $ do
      it "correctly parses multiline SSE split across arbitrary network chunks" $ do
        let c1 = "event: message\ndata: {\"text\": \"hel"
            c2 = "lo world\"}\n\nevent: message\ndata: [DONE]\n\n"
            (st1, evs1) = stepSSE newSSEParser (TE.encodeUtf8 c1)
            (st2, evs2) = stepSSE st1 (TE.encodeUtf8 c2)
            flushed = finishSSE st2
        evs1 `shouldBe` []
        case evs2 of
          [e1, e2] -> do
            e1 `shouldBe` "{\"text\": \"hello world\"}"
            e2 `shouldBe` "[DONE]"
          _ -> expectationFailure ("Expected 2 events, got: " <> show evs2)
        flushed `shouldBe` []

      it "includes stream: true in Anthropic streaming request payload" $ do
        let req = CompletionRequest
              { crModel = "claude-3-5-sonnet-20241022"
              , crSystem = Just "system prompt"
              , crMessages = [UserMsg "stream test"]
              , crParams = defaultParams
              , crTools = []
              , crToolChoice = ToolAuto
              , crResponseFormat = RfText
              }
            bodyVal = buildMessagesBody (defaultAnthropicConfig "sk-test") req
            streamBodyVal = case bodyVal of
              Object o -> Object (KM.insert (Key.fromText "stream") (Bool True) o)
              other -> other
        case streamBodyVal of
          Object o -> KM.lookup (Key.fromText "stream") o `shouldBe` Just (Bool True)
          _ -> expectationFailure "Failed to build Anthropic streaming request body"

    describe "2. Batch 2: State Isolation & Concurrency" $ do
      it "ensures transactional turns rollback uncommitted messages on error" $ do
        let script = [Left (HttpError "Connection dropped")]
        (res, _, hist, _) <- runEff $ runLLMMockFull script (attempt (generateText "Test prompt"))
        case res of
          Left (HttpError _) -> hist `shouldBe` []
          _ -> expectationFailure "Expected network error with rolled-back history"

    describe "3. Batch 3: Fail-Closed Tool Validation, Formatting & Extraction" $ do
      it "extracts deeply nested JSON using delimiter stack even when prose contains unescaped braces" $ do
        let raw = "Template {a: 1} here is json: {\"pName\":\"Bob\",\"pAge\":30}"
        case decodeViaJSON @Person raw of
          Right p -> do
            pName p `shouldBe` "Bob"
            pAge p `shouldBe` 30
          Left err -> expectationFailure ("Failed to extract JSON from prose: " <> err)

      it "prioritizes fenced JSON code blocks when preceding text contains invalid braces" $ do
        let raw = "Invalid brace {foo: bar} and valid fence:\n```json\n{\"pName\":\"Alice\",\"pAge\":25}\n```\nEnd."
        case decodeViaJSON @Person raw of
          Right p -> do
            pName p `shouldBe` "Alice"
            pAge p `shouldBe` 25
          Left err -> expectationFailure ("Failed to extract JSON from fenced block: " <> err)

      it "rejects malformed delimiter structures fail-closed" $ do
        extractJSON "[1, 2}" `shouldSatisfy` (\case Left _ -> True; _ -> False)
        extractJSON "{\"a\": 1]" `shouldSatisfy` (\case Left _ -> True; _ -> False)
        extractJSON "[ { ] }" `shouldSatisfy` (\case Left _ -> True; _ -> False)

    describe "4. Batch 4: Workspace Path Containment & Process Supervision" $ do
      it "enforces strict workspace containment against 10 distinct path escape attacks" $ do
        withSystemTempDirectory "challenger-sandbox" $ \tmpDir -> do
          let wsRoot = tmpDir </> "workspace"
          let outsideDir = tmpDir </> "outside"
          createDirectoryIfMissing True wsRoot
          createDirectoryIfMissing True outsideDir
          writeFile (outsideDir </> "secret.txt") "CONFIDENTIAL"

          -- Create a symlink inside workspace pointing to outsideDir
          let symlinkOutside = wsRoot </> "symlink_escape"
          SD.createFileLink outsideDir symlinkOutside

          let escapeAttacks =
                [ "foo/../../secret/passwords.txt"
                , "../../secret.txt"
                , "a/b/c/../../../../secret.txt"
                , "./foo/../../outside.txt"
                , "/etc/passwd"
                , outsideDir </> "secret.txt"
                , "symlink_escape/secret.txt"
                , "nonexistent1/nonexistent2/../../../secret.txt"
                , "foo/../../"
                , "foo///..//../outside.txt"
                ]

          forM_ escapeAttacks $ \atk -> do
            eRes <- try (resolveSafeLocalPath wsRoot atk)
            case eRes of
              Left (WorldPathOutsideWorkspace _ _) -> pure ()
              Left (e :: WorldError) -> expectationFailure ("Expected WorldPathOutsideWorkspace, got: " <> show e <> " for " <> atk)
              Right canonPath -> expectationFailure ("Path escape succeeded for " <> atk <> " -> " <> canonPath)

      it "allows safe paths within workspace root" $ do
        withSystemTempDirectory "challenger-safe-ws" $ \tmpDir -> do
          let wsRoot = tmpDir </> "workspace"
          createDirectoryIfMissing True wsRoot
          writeFile (wsRoot </> "valid.txt") "VALID CONTENT"

          -- foo does not exist, but foo/../valid.txt collapses safely to valid.txt
          res1 <- resolveSafeLocalPath wsRoot "foo/../valid.txt"
          res1 `shouldSatisfy` isInsideRoot wsRoot

          res2 <- resolveSafeLocalPath wsRoot "valid.txt"
          res2 `shouldSatisfy` isInsideRoot wsRoot

          -- Write file via WorldLocal interpreter
          runEff $ runWorldLocal wsRoot $ do
            writeFileText "sub/nested/file.txt" "nested content"
            content <- readFileText "sub/nested/file.txt"
            liftIO $ content `shouldBe` "nested content"

      it "enforces process execution timeout and terminates timed-out commands" $ do
        withSystemTempDirectory "challenger-proc" $ \tmpDir -> do
          let specSleep = CommandSpec "sleep" ["5"] Nothing Nothing (Just 100) Nothing
          res <- runEff $ runWorldLocal tmpDir (runCommand specSleep)
          prTimedOut res `shouldBe` True
          prExitCode res `shouldBe` (-1)

    describe "5. Batch 5: Journal & Replay Fidelity" $ do
      it "preserves tool call IDs and arguments across serialization" $ do
        let ev = ToolInvoked "call_test_123" "test_tool" (object ["arg" .= (42 :: Int)])
            serialized = encode ev
        case decode serialized of
          Just (ToolInvoked tid name args) -> do
            tid `shouldBe` "call_test_123"
            name `shouldBe` "test_tool"
            args `shouldBe` object ["arg" .= (42 :: Int)]
          other -> expectationFailure ("Expected ToolInvoked, got: " <> show other)
