{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LLMonad.JournalAutoRecordSpec (spec) where

import Control.Exception (IOException, throwIO)
import Data.Aeson (FromJSON, ToJSON (toJSON), object, (.=))
import Data.Text (Text)
import Effectful
import Effectful.Exception qualified as E
import GHC.Generics (Generic)
import LLMonad
import LLMonad.Journal.Types qualified as JT
import Test.Hspec

data EchoArgs = EchoArgs {echoText :: Text}
    deriving (Show, Eq, Generic, FromJSON, ToSchema)

call1 :: ToolCall
call1 = ToolCall "c1" "echo" (object ["echoText" .= ("hi" :: Text)])

expectedTurn :: [JT.JournalEvent]
expectedTurn =
    [ JT.TurnStarted "turn-1"
    , JT.UserMsg "Summarize."
    , JT.ToolInvoked "c1" "echo" (object ["echoText" .= ("hi" :: Text)])
    , JT.ModelTurn "" [call1]
    , JT.ToolCompleted "c1" (Right (toJSON ("hi" :: Text)))
    , JT.ModelTurn "done" []
    , JT.TurnFinished "turn-1"
    ]

spec :: Spec
spec = do
    describe "Automatic Session Recording" $ do
        it "records prompts, tool calls and results, and model turns as they happen" $ do
            let runtime =
                    applyMiddleware journaling $
                        mockModel [Right (toolResp [call1]), Right (textResp "done")]
                agent =
                    mount
                        runtime
                        (tools [toolSync "echo" "Echo the text back" echoTool])
                        (textAgent "sys" id)
                echoTool (EchoArgs t) = t
            (reply, evs) <-
                runEff . runJournalMemory $ do
                    reply <- withRecordedTurn "turn-1" (invoke agent "Summarize.")
                    pure reply

            reply `shouldBe` "done"
            evs `shouldBe` expectedTurn

            case replayAudit evs of
                Left err -> expectationFailure ("recorded session failed audit: " ++ show err)
                Right summary -> do
                    rsTotalTurns summary `shouldBe` 1
                    rsUserMessages summary `shouldBe` 1
                    rsModelTurns summary `shouldBe` 2
                    rsToolInvocations summary `shouldBe` 1
                    rsToolCompletions summary `shouldBe` 1
                    rsIsValidSequence summary `shouldBe` True

        it "closes the turn even when the run aborts mid-flight" $ do
            (crashOutcome, evs) <-
                runEff
                    . runJournalMemory
                    . E.try @IOException
                    . withRecordedTurn "turn-x"
                    $ do
                        recordUserMsg "before the crash"
                        liftIO (throwIO (userError "mid-turn crash"))

            case crashOutcome of
                Left e -> show e `shouldContain` "mid-turn crash"
                Right () -> expectationFailure "expected the mid-turn crash to propagate"

            evs
                `shouldBe` [ JT.TurnStarted "turn-x"
                           , JT.UserMsg "before the crash"
                           , JT.TurnFinished "turn-x"
                           ]

            case replayAudit evs of
                Left err -> expectationFailure ("aborted session failed audit: " ++ show err)
                Right summary -> rsIsValidSequence summary `shouldBe` True
