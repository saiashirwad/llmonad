{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import Effectful (runEff)
import LLMonad
  ( TUIConfig (..)
  , loadJournalFile
  , runTUIAppWithConfig
  )
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

-- | CLI options record.
data CLIOptions = CLIOptions
  { cliWorkspace   :: !(Maybe FilePath)
  , cliModel       :: !Text
  , cliSystem      :: !(Maybe Text)
  , cliResume      :: !(Maybe FilePath)
  , cliShowHelp    :: !Bool
  } deriving (Show, Eq)

defaultCLIOptions :: CLIOptions
defaultCLIOptions = CLIOptions
  { cliWorkspace = Nothing
  , cliModel     = "deepseek-chat"
  , cliSystem    = Nothing
  , cliResume    = Nothing
  , cliShowHelp  = False
  }

-- | Parse command line arguments.
parseCLIArgs :: [String] -> Either String CLIOptions
parseCLIArgs = go defaultCLIOptions
  where
    go opts [] = Right opts
    go opts ("-h":_) = Right (opts { cliShowHelp = True })
    go opts ("--help":_) = Right (opts { cliShowHelp = True })
    go opts ("-w":p:rest) = go (opts { cliWorkspace = Just p }) rest
    go opts ("--workspace":p:rest) = go (opts { cliWorkspace = Just p }) rest
    go opts ("-m":m:rest) = go (opts { cliModel = T.pack m }) rest
    go opts ("--model":m:rest) = go (opts { cliModel = T.pack m }) rest
    go opts ("-s":s:rest) = go (opts { cliSystem = Just (T.pack s) }) rest
    go opts ("--system":s:rest) = go (opts { cliSystem = Just (T.pack s) }) rest
    go opts ("-r":r:rest) = go (opts { cliResume = Just r }) rest
    go opts ("--resume":r:rest) = go (opts { cliResume = Just r }) rest
    go _ (unknown:_) = Left ("Unknown or incomplete argument: " ++ unknown)

-- | Print usage string.
printUsage :: String -> IO ()
printUsage progName = do
  putStrLn "LLMonad Interactive Terminal User Interface (TUI)\n"
  putStrLn $ "Usage: " ++ progName ++ " [OPTIONS]\n"
  putStrLn "Options:"
  putStrLn "  -w, --workspace PATH   Workspace directory path (default: current directory)"
  putStrLn "  -m, --model MODEL      Model identifier (default: deepseek-chat)"
  putStrLn "  -s, --system PROMPT    Initial system prompt instructions"
  putStrLn "  -r, --resume FILE      Resume previous session from JSONL journal file"
  putStrLn "  -h, --help             Show this help message"

-- | Main executable entry point.
main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case parseCLIArgs args of
    Left err -> do
      hPutStrLn stderr ("Error: " ++ err)
      printUsage progName
      exitFailure

    Right opts
      | cliShowHelp opts -> do
          printUsage progName
          exitSuccess
      | otherwise -> do
          currentDir <- getCurrentDirectory
          let wsPath = maybe currentDir id (cliWorkspace opts)
          
          -- Check session resume if specified
          case cliResume opts of
            Nothing -> do
              let config = TUIConfig
                    { cfgWorkspacePath   = wsPath
                    , cfgModelName       = cliModel opts
                    , cfgSystemPrompt    = cliSystem opts
                    , cfgSessionFilePath = Nothing
                    }
              runTUIAppWithConfig config

            Just resumePath -> do
              exists <- doesFileExist resumePath
              if not exists
                then do
                  hPutStrLn stderr ("Error: Resume journal file does not exist: " ++ resumePath)
                  exitFailure
                else do
                  journalRes <- runEff (loadJournalFile resumePath)
                  case journalRes of
                    Left jErr -> do
                      hPutStrLn stderr ("Error reading session journal: " ++ T.unpack jErr)
                      exitFailure
                    Right events -> do
                      putStrLn ("Loaded " ++ show (length events) ++ " events from journal.")
                      let config = TUIConfig
                            { cfgWorkspacePath   = wsPath
                            , cfgModelName       = cliModel opts
                            , cfgSystemPrompt    = cliSystem opts
                            , cfgSessionFilePath = Just resumePath
                            }
                      runTUIAppWithConfig config
