{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module LLMonad.AgentSpec (spec) where

import Control.Exception (try)
import Data.Aeson (FromJSON (..), Value (..), object, (.=))
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import GHC.Generics (Generic)
import LLMonad
import Test.Hspec

data AddArgs = AddArgs
  { x :: Int
  , y :: Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToSchema)

data SearchArgs = SearchArgs
  { query :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToSchema)

data NoArgs = NoArgs {}
  deriving (Show, Eq, Generic, ToSchema)

instance FromJSON NoArgs where
  parseJSON (Object _) = pure NoArgs
  parseJSON (Array _) = pure NoArgs
  parseJSON Null = pure NoArgs
  parseJSON _ = fail "expected object for NoArgs"

data AgentSummary = AgentSummary
  { totalSum :: Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToSchema)

addTool :: Monad m => Tool m
addTool = mkTool "add" "Add two integers" $ \(args :: AddArgs) -> pure (x args + y args)

searchTool :: (IOE :> es) => IORef [Text] -> Tool (Eff es)
searchTool logRef = mkTool "search" "Search knowledge base" $ \(SearchArgs q) -> do
  liftIO (modifyIORef' logRef (q :))
  pure ("Found article for: " <> q)

noArgsTool :: Monad m => Tool m
noArgsTool = mkTool "ping" "Ping status" $ \(_ :: NoArgs) -> pure ("pong" :: Text)

runAgentScript ::
  [Either LLMError CompletionResponse] ->
  Eff '[LLM, IOE] x ->
  IO (x, [ChatMessage], [CompletionRequest])
runAgentScript script act = do
  (x, reqs, hist, _) <- runEff (runLLMMockFull script act)
  pure (x, hist, reqs)

toolContentsOf :: [ChatMessage] -> [Text]
toolContentsOf msgs = [c | ToolMsg _ c <- msgs]

spec :: Spec
spec = do
  describe "Autonomous Agent Loop (Tier 1: Feature Coverage)" $ do
    it "mkTool produces valid ToolSpec with name, description, and schema" $ do
      let spec' = toolSpec (addTool @IO)
      toolSpecName spec' `shouldBe` "add"
      toolSpecDescription spec' `shouldBe` "Add two integers"
      toolSpecParameters spec' `shouldBe` toSchema @AddArgs

    it "useTools executes a single tool call and terminates on final text" $ do
      let script =
            [ Right (toolResp [ToolCall "call-1" "add" (object ["x" .= (10 :: Int), "y" .= (20 :: Int)])])
            , Right (textResp "The sum is 30")
            ]
      (answer, conv, _) <- runAgentScript script (useTools [addTool] "What is 10 + 20?")
      answer `shouldBe` "The sum is 30"
      toolContentsOf conv `shouldBe` ["30"]

    it "executes multiple parallel tool calls in one round" $ do
      let script =
            [ Right
                ( toolResp
                    [ ToolCall "call-1" "add" (object ["x" .= (1 :: Int), "y" .= (2 :: Int)])
                    , ToolCall "call-2" "add" (object ["x" .= (3 :: Int), "y" .= (4 :: Int)])
                    ]
                )
            , Right (textResp "Sums calculated")
            ]
      (answer, conv, _) <- runAgentScript script (useTools [addTool] "compute two sums")
      answer `shouldBe` "Sums calculated"
      toolContentsOf conv `shouldBe` ["3", "7"]

    it "executes multi-round sequential agent steps" $ do
      searchLogs <- newIORef []
      let sTool = searchTool searchLogs
          script =
            [ Right (toolResp [ToolCall "c1" "search" (object ["query" .= ("Haskell" :: Text)])])
            , Right (toolResp [ToolCall "c2" "add" (object ["x" .= (5 :: Int), "y" .= (5 :: Int)])])
            , Right (textResp "Completed both research and computation")
            ]
      (answer, conv, _) <- runAgentScript script (useTools [sTool, addTool] "run workflow")
      answer `shouldBe` "Completed both research and computation"
      logs <- readIORef searchLogs
      logs `shouldBe` ["Haskell"]
      toolContentsOf conv `shouldBe` ["\"Found article for: Haskell\"", "10"]

    it "reports unknown tool calls back to model as errors" $ do
      let script =
            [ Right (toolResp [ToolCall "c1" "unknown_func" (object ["arg" .= (1 :: Int)])])
            , Right (textResp "Recovered after unknown tool")
            ]
      (answer, conv, _) <- runAgentScript script (useTools [addTool] "try unknown")
      answer `shouldBe` "Recovered after unknown tool"
      toolContentsOf conv `shouldSatisfy` any (T.isInfixOf "unknown tool: unknown_func")

    it "supports zero-argument tools" $ do
      let script =
            [ Right (toolResp [ToolCall "c1" "ping" (object [])])
            , Right (textResp "pong received")
            ]
      (answer, conv, _) <- runAgentScript script (useTools [noArgsTool] "check ping")
      answer `shouldBe` "pong received"
      toolContentsOf conv `shouldBe` ["\"pong\""]

  describe "Autonomous Agent Loop (Tier 2: Boundary & Corner Cases)" $ do
    it "throws AgentRoundsExhausted when exceeding maximum allowed steps" $ do
      let infiniteToolCalls =
            repeat (Right (toolResp [ToolCall "loop" "add" (object ["x" .= (1 :: Int), "y" .= (1 :: Int)])]))
          opts = defaultAgentOpts {agentMaxRounds = 3}
      res <- try (runAgentScript (take 10 infiniteToolCalls) (useToolsWith opts [addTool] "infinite loop"))
      case res of
        Left (AgentRoundsExhausted n) -> n `shouldBe` 3
        Left other -> expectationFailure ("Expected AgentRoundsExhausted, got: " <> show other)
        Right _ -> expectationFailure "Expected AgentRoundsExhausted exception"

    it "succeeds with empty tools list when model answers directly" $ do
      let script = [Right (textResp "Direct answer without tools")]
      (answer, conv, _) <- runAgentScript script (useTools [] "No tools needed")
      answer `shouldBe` "Direct answer without tools"
      toolContentsOf conv `shouldBe` []

    it "handles malformed tool arguments JSON gracefully with error reporting" $ do
      let script =
            [ Right (toolResp [ToolCall "c1" "add" (object ["x" .= ("not-an-int" :: Text), "y" .= (2 :: Int)])])
            , Right (textResp "Handled malformed arguments")
            ]
      (answer, conv, _) <- runAgentScript script (useTools [addTool] "bad args")
      answer `shouldBe` "Handled malformed arguments"
      toolContentsOf conv `shouldSatisfy` any (T.isInfixOf "error")

    it "records high-volume tool execution responses without truncation" $ do
      let bigString = T.replicate 1000 "X"
          bigTool = mkTool "big" "Returns large output" $ \(_ :: NoArgs) -> pure bigString
          script =
            [ Right (toolResp [ToolCall "c1" "big" (object [])])
            , Right (textResp "Processed large payload")
            ]
      (answer, conv, _) <- runAgentScript script (useTools [bigTool] "get big string")
      answer `shouldBe` "Processed large payload"
      toolContentsOf conv `shouldBe` ["\"" <> bigString <> "\""]

    it "passes custom sampling parameters to every round of agent loop" $ do
      let customParams = defaultParams {paramTemperature = Just 0.1, paramMaxTokens = Just 500}
          opts = AgentOpts {agentMaxRounds = 5, agentParams = customParams}
          script =
            [ Right (toolResp [ToolCall "c1" "ping" (object [])])
            , Right (textResp "done")
            ]
      (_, _, reqs) <- runAgentScript script (useToolsWith opts [noArgsTool] "param test")
      length reqs `shouldBe` 2
      map (paramTemperature . crParams) reqs `shouldSatisfy` all (== Just 0.1)
      map (paramMaxTokens . crParams) reqs `shouldSatisfy` all (== Just 500)

    it "runAgent executes tools and returns final answer" $ do
      let script =
            [ Right (toolResp [ToolCall "c1" "add" (object ["x" .= (15 :: Int), "y" .= (25 :: Int)])])
            , Right (textResp "Computed sum is 40")
            ]
      (answer, conv, _) <- runAgentScript script (runAgent [addTool] "Add 15 and 25")
      answer `shouldBe` "Computed sum is 40"
      toolContentsOf conv `shouldBe` ["40"]

    it "runAgentStructured executes tools and decodes structured record" $ do
      let script =
            [ Right (toolResp [ToolCall "c1" "add" (object ["x" .= (100 :: Int), "y" .= (200 :: Int)])])
            , Right (structuredResp (object ["totalSum" .= (300 :: Int)]))
            ]
      (summary, _, _) <- runAgentScript script (runAgentStructured @AgentSummary [addTool] "Compute sum")
      summary `shouldBe` AgentSummary 300

    it "detects repeated tool call cycles and pushes warning message" $ do
      let repeatCall = Right (toolResp [ToolCall "c1" "add" (object ["x" .= (1 :: Int), "y" .= (1 :: Int)])])
          script =
            [ repeatCall
            , repeatCall
            , Right (textResp "Recovered after cycle warning")
            ]
      (answer, conv, _) <- runAgentScript script (runAgent [addTool] "Run repeated call")
      answer `shouldBe` "Recovered after cycle warning"
      let userMsgs = [m | UserMsg m <- conv]
      userMsgs `shouldSatisfy` any (T.isInfixOf "Repeated identical tool call signature detected")
