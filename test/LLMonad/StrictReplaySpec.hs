{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module LLMonad.StrictReplaySpec (spec) where

import Control.Exception (try)
import Data.Aeson (FromJSON, Value, object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import GHC.Generics (Generic)
import LLMonad
import LLMonad.Journal.Strict
import LLMonad.Journal.Types (emptyModelMetrics)
import LLMonad.Model (runModelRuntime)
import LLMonad.Types qualified as Types
import Test.Hspec

-- A recording with one tool-backed turn followed by a final answer.
fullRecording :: [JournalEvent]
fullRecording =
    [ TurnStarted "t1"
    , JournalUserMsg "inspect"
    , ModelTurn "checking" [ToolCall "c1" "view_file" readmeArgs]
    , ToolInvoked "c1" "view_file" readmeArgs
    , ToolCompleted "c1" (Right (object ["lines" .= (3 :: Int)]))
    , MetricsReported (emptyModelMetrics "mock-model")
    , TurnFinished "t1"
    , ModelTurn "done" []
    ]

readmeArgs :: Value
readmeArgs = object ["path" .= ("README.md" :: Text)]

argsFor :: Text -> Value
argsFor p = object ["path" .= p]

bodyOf :: Text -> Value
bodyOf b = object ["body" .= b]

data ReadArgs = ReadArgs {path :: Text}
    deriving (Generic, FromJSON, ToSchema)

data NoArgs = NoArgs
    deriving (Generic, FromJSON, ToSchema)

-- Reads through World, which every replay test wires to an empty in-memory
-- store: if this handler ever executes, the test fails loudly instead of
-- silently passing on real execution.
boobyTrappedReader :: (World :> es) => Tool (Eff es)
boobyTrappedReader =
    tool "view_file" "read a file" $ \(ReadArgs p) -> do
        contents <- readFileText (T.unpack p)
        pure (object ["lines" .= length (T.lines contents)])

mysteryTool :: (World :> es) => Tool (Eff es)
mysteryTool =
    tool "mystery_tool" "never recorded anywhere" $ \NoArgs ->
        pure (object [])

-- Mount a strict-replay model runtime and toolset for one definition.
replayMount ::
    (IOE :> es, World :> es) =>
    ReplayScript ->
    AgentDef Text Text ->
    IO (Agent es Text Text)
replayMount script def = do
    wrapped <- strictReplayToolset script (tools [boobyTrappedReader, mysteryTool])
    rt <- strictReplayRuntime script
    pure (mount rt wrapped def)

spec :: Spec
spec =
    describe "LLMonad.Journal.Strict" $ do
        describe "extractReplayScript" $ do
            it "parses turns and pairs invocations with completions" $ do
                extractReplayScript fullRecording
                    `shouldBe` ReplayScript
                        { scriptTurns =
                            [ RecordedTurn "checking" [ToolCall "c1" "view_file" readmeArgs]
                            , RecordedTurn "done" []
                            ]
                        , scriptToolCalls =
                            [ RecordedToolCall
                                "c1"
                                "view_file"
                                readmeArgs
                                (Just (Right (object ["lines" .= (3 :: Int)])))
                            ]
                        }

            it "gives each invocation of a reused call id its own result" $ do
                -- recordToolCall writes the tool name as the call id, so a
                -- tool called twice yields two invocations sharing one id.
                let evs =
                        [ ToolInvokedSimple "view_file" (argsFor "a.hs")
                        , ToolCompleted "view_file" (Right (bodyOf "FIRST"))
                        , ToolInvokedSimple "view_file" (argsFor "b.hs")
                        , ToolCompleted "view_file" (Right (bodyOf "SECOND"))
                        ]
                scriptToolCalls (extractReplayScript evs)
                    `shouldBe` [ RecordedToolCall "view_file" "view_file" (argsFor "a.hs") (Just (Right (bodyOf "FIRST")))
                               , RecordedToolCall "view_file" "view_file" (argsFor "b.hs") (Just (Right (bodyOf "SECOND")))
                               ]

            it "leaves invocations without a completion result-less" $ do
                let evs =
                        [ ModelTurn "" [ToolCall "c9" "edit_file" (object [])]
                        , ToolInvoked "c9" "edit_file" (object [])
                        ]
                    [call] = scriptToolCalls (extractReplayScript evs)
                rtcResult call `shouldBe` Nothing

        describe "end-to-end strict replay" $ do
            it "plays back results without executing handlers and settles on the final turn" $ do
                agent <- replayMount (extractReplayScript fullRecording) (textAgent "sys" id)
                out <- runEff . fmap fst . runWorldMemoryWithFiles [] $ invoke agent "inspect"
                out `shouldBe` "done"

            it "raises naming the ordinal of an unrecorded model turn" $ do
                agent <- replayMount (extractReplayScript fullRecording) (textAgent "sys" id)
                res <-
                    try @LLMError . runEff . fmap fst . runWorldMemoryWithFiles [] $
                        invoke agent "a" >> invoke agent "b"
                res
                    `shouldBe` Left
                        (ReplayDivergence "strict replay: model turn #3 was not in the recording")

            it "raises on a tool the recording never made" $ do
                let script =
                        ReplayScript
                            { scriptTurns =
                                [ RecordedTurn "" [ToolCall "c1" "mystery_tool" (object [])]
                                , RecordedTurn "final" []
                                ]
                            , scriptToolCalls = []
                            }
                agent <- replayMount script (textAgent "sys" id)
                res <-
                    try @LLMError . runEff . fmap fst . runWorldMemoryWithFiles [] $
                        invoke agent "go"
                res
                    `shouldBe` Left
                        (ReplayDivergence "strict replay: tool 'mystery_tool' was not in the recording")

            it "raises when the workflow calls a different tool than the one expected next" $ do
                let script =
                        ReplayScript
                            { scriptTurns =
                                [ RecordedTurn "" [ToolCall "c1" "view_file" readmeArgs]
                                , RecordedTurn "final" []
                                ]
                            , scriptToolCalls = [RecordedToolCall "c1" "edit_file" (object []) Nothing]
                            }
                agent <- replayMount script (textAgent "sys" id)
                res <-
                    try @LLMError . runEff . fmap fst . runWorldMemoryWithFiles [] $
                        invoke agent "go"
                res
                    `shouldBe` Left
                        (ReplayDivergence "strict replay: expected tool 'edit_file' next; workflow called 'view_file'")

            it "raises when arguments drift from the recording" $ do
                let drifted = object ["path" .= ("src/other.hs" :: Text)]
                    script =
                        ReplayScript
                            { scriptTurns =
                                [ RecordedTurn "" [ToolCall "c1" "view_file" drifted]
                                , RecordedTurn "final" []
                                ]
                            , scriptToolCalls =
                                [RecordedToolCall "c1" "view_file" readmeArgs (Just (Right (object [])))]
                            }
                agent <- replayMount script (textAgent "sys" id)
                res <-
                    try @LLMError . runEff . fmap fst . runWorldMemoryWithFiles [] $
                        invoke agent "go"
                res
                    `shouldBe` Left
                        (ReplayDivergence "strict replay: tool 'view_file' arguments differ from the recording")

            it "returns a tool-error answer when the invocation has no recorded result" $ do
                let script =
                        ReplayScript
                            { scriptTurns =
                                [ RecordedTurn "" [ToolCall "c1" "view_file" readmeArgs]
                                , RecordedTurn "settled" []
                                ]
                            , scriptToolCalls = [RecordedToolCall "c1" "view_file" readmeArgs Nothing]
                            }
                agent <- replayMount script (textAgent "sys" id)
                out <- runEff . fmap fst . runWorldMemoryWithFiles [] $ invoke agent "go"
                out `shouldBe` "settled"

            it "names each excess turn by its own ordinal, not the recording's end" $ do
                agent <- replayMount (ReplayScript [] []) (textAgent "sys" id)
                let call = try @LLMError . runEff . fmap fst . runWorldMemoryWithFiles [] . invoke agent
                diverged <- mapM call ["a", "b"]
                diverged
                    `shouldBe` [ Left (ReplayDivergence "strict replay: model turn #1 was not in the recording")
                               , Left (ReplayDivergence "strict replay: model turn #2 was not in the recording")
                               ]

            it "raises when the recording used a tool this toolset no longer has" $ do
                let script =
                        ReplayScript
                            { scriptTurns =
                                [ RecordedTurn "" [ToolCall "c1" "view_file" readmeArgs]
                                , RecordedTurn "done" []
                                ]
                            , scriptToolCalls =
                                [RecordedToolCall "c1" "view_file" readmeArgs (Just (Right (object [])))]
                            }
                rt <- strictReplayRuntime script
                stripped <- strictReplayToolset script noTools
                res <-
                    try @LLMError . runEff . fmap fst . runWorldMemoryWithFiles [] $
                        invoke (mount rt stripped (textAgent "sys" id)) "go"
                res
                    `shouldBe` Left
                        (ReplayDivergence "strict replay: the recording called tool 'view_file', which this toolset no longer has")

        describe "conversation scope" $
            -- The library's promise is that one workflow runs against a live
            -- provider, a mock, or a recording. All three scope a conversation
            -- to one invocation; a shared one would let concurrent agents
            -- write into each other's history.
            it "starts each invocation from an empty conversation, as model and mockModel do" $ do
                rt <- strictReplayRuntime (ReplayScript [] [])
                carried <- runEff $ do
                    runModelRuntime rt (pushMessage (Types.UserMsg "from an earlier invocation"))
                    runModelRuntime rt getHistory
                carried `shouldBe` []
