{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Effectful (Eff, runEff)
import GHC.Generics (Generic)
import LLMonad
import System.Environment (getArgs, lookupEnv)

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

reviewer key =
    bind
        (model (deepSeekProvider key) "deepseek-v4-flash")
        noTools
        definition

main :: IO ()
main = do
    arguments <- getArgs
    path <- case arguments of
        [single] -> pure single
        _ -> fail "usage: deepseek-review-file <path>"
    key <- requireDeepSeekKey
    source <- TIO.readFile path
    let request =
            ReviewRequest
                { requestPath = T.pack path
                , requestLineCount = length (T.lines source)
                , requestSource = source
                }
        reviewer =
            bind
                (model (deepSeekProvider key) "deepseek-v4-flash")
                noTools
                definition
    Verdict{severity, summary, suggestion} <- runEff (workflow reviewer request)
    TIO.putStrLn
        [prompt|#{severity}: #{summary}

Suggested fix: #{suggestion}|]

requireDeepSeekKey :: IO T.Text
requireDeepSeekKey = do
    value <- lookupEnv "DEEPSEEK_API_KEY"
    case value of
        Just key | not (null key) -> pure (T.pack key)
        _ -> fail "DEEPSEEK_API_KEY is not set"
