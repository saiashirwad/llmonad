{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import DeepSeek (deepSeekRuntime)
import Effectful (Eff, runEff)
import GHC.Generics (Generic)
import LLMonad
import System.Environment (getArgs)

-- | The file handed to the reviewer.
data ReviewRequest = ReviewRequest
    { requestPath :: T.Text
    , requestLineCount :: Int
    , requestSource :: T.Text
    }

-- | The single finding the reviewer must come back with.
data Verdict = Verdict
    { severity :: T.Text
    , summary :: T.Text
    , suggestion :: T.Text
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

severities :: T.Text
severities = "blocker, warning, nit, clean"

systemPrompt :: T.Text
systemPrompt =
    [prompt|You are a terse Haskell reviewer.
Report the single worst problem in the file and nothing else.
Rate it as one of: #{severities}.|]

renderRequest :: ReviewRequest -> T.Text
renderRequest ReviewRequest{requestPath, requestLineCount, requestSource} =
    [prompt|Review #{requestPath} (#{requestLineCount} lines).

#{requestSource}

Quote the offending line in the summary.|]

definition :: AgentDef ReviewRequest Verdict
definition = structuredAgent systemPrompt renderRequest

workflow :: Agent es ReviewRequest Verdict -> ReviewRequest -> Eff es Verdict
workflow = invoke

main :: IO ()
main = do
    arguments <- getArgs
    path <- case arguments of
        [single] -> pure single
        _ -> fail "usage: deepseek-review-file <path>"
    runtime <- deepSeekRuntime "deepseek-v4-flash"
    source <- TIO.readFile path
    let request =
            ReviewRequest
                { requestPath = T.pack path
                , requestLineCount = length (T.lines source)
                , requestSource = source
                }
        reviewer =
            mount
                runtime
                noTools
                definition
    Verdict{severity, summary, suggestion} <- runEff (workflow reviewer request)
    TIO.putStrLn
        [prompt|#{severity}: #{summary}

Suggested fix: #{suggestion}|]
