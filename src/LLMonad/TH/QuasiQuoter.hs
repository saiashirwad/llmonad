{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell QuasiQuoter for prompts with variable interpolation.
module LLMonad.TH.QuasiQuoter (
    prompt,
    parsePromptChunks,
    PromptChunk (..),
) where

import Data.Char (isSpace)
import Data.Text qualified as T
import LLMonad.Prompt (toPromptArg)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))

-- | A chunk in a parsed prompt template: either literal text or a variable name.
data PromptChunk
    = ChunkLit String
    | ChunkVar String
    deriving (Show, Eq)

-- | Parse prompt template text into literal and variable chunks.
parsePromptChunks :: String -> Either String [PromptChunk]
parsePromptChunks = go [] ""
  where
    go acc litAcc [] =
        let acc' = if null litAcc then acc else ChunkLit (reverse litAcc) : acc
         in Right (reverse acc')
    go acc litAcc ('\\' : '#' : '{' : rest) =
        go acc ('{' : '#' : litAcc) rest
    go acc litAcc ('#' : '{' : rest) =
        let acc' = if null litAcc then acc else ChunkLit (reverse litAcc) : acc
         in case break (== '}') rest of
                (varStr, '}' : remaining) ->
                    let cleanVar = trim varStr
                     in if null cleanVar
                            then Left "Empty variable interpolation in #{}"
                            else go (ChunkVar cleanVar : acc') "" remaining
                _ -> Left "Unclosed #{ in prompt template"
    go acc litAcc (c : rest) = go acc (c : litAcc) rest

    trim = f . f
      where
        f = reverse . dropWhile isSpace

-- | Compile parsed prompt chunks into a Haskell expression generating Text.
compilePromptExp :: String -> Q Exp
compilePromptExp str = case parsePromptChunks str of
    Left err -> fail ("prompt quasi-quoter syntax error: " ++ err)
    Right [] -> [|T.empty|]
    Right [ChunkLit s] -> [|T.pack $(stringE s)|]
    Right [ChunkVar v] -> [|toPromptArg $(varE (mkName v))|]
    Right chunks -> do
        let chunkExp (ChunkLit s) = [|T.pack $(stringE s)|]
            chunkExp (ChunkVar v) = [|toPromptArg $(varE (mkName v))|]
        expList <- mapM chunkExp chunks
        let listExp = ListE expList
        [|T.concat $(pure listExp)|]

-- | QuasiQuoter for prompt templates with @#{var}@ interpolation.
prompt :: QuasiQuoter
prompt =
    QuasiQuoter
        { quoteExp = compilePromptExp
        , quotePat = \_ -> fail "prompt QuasiQuoter is not supported in patterns"
        , quoteType = \_ -> fail "prompt QuasiQuoter is not supported in types"
        , quoteDec = \_ -> fail "prompt QuasiQuoter is not supported in declarations"
        }
