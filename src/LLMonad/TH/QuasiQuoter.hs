{-# LANGUAGE TemplateHaskell #-}

{- | QuasiQuoter for prompt templates with variable interpolation.

> query = [prompt|Customer #{customer} owes $#{amount}. \#{not-a-tag}|]

Grammar:

> template := (literal | escape | tag)*
> escape   := "\#{"                    -- renders as the literal text "#{"
> tag      := "#{" whitespace name whitespace "}"
-}
module LLMonad.TH.QuasiQuoter (
    prompt,
    parsePromptChunks,
    PromptChunk (..),
) where

import Data.Text qualified as T
import LLMonad.Prompt (toPromptArg)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))

-- | One piece of a parsed template.
data PromptChunk
    = -- | Literal text, emitted verbatim.
      ChunkLit String
    | -- | Name of a variable in scope at the splice site.
      ChunkVar String
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Parsing: template text -> chunks
--------------------------------------------------------------------------------

{- | Parse template text into chunks.

Neighbouring literals are merged, so escapes never split one run of text
into several 'ChunkLit's: the output alternates literal \/ tag \/ literal \/ …
-}
parsePromptChunks :: String -> Either String [PromptChunk]
parsePromptChunks = fmap mergeLiterals . go . T.pack
  where
    -- Each guard mirrors one grammar production, in specification order.
    go text
        | Just rest <- T.stripPrefix "\\#{" text = cons (ChunkLit "#{") rest
        | Just rest <- T.stripPrefix "#{" text = tag rest
        | otherwise = plain text

    cons chunk = fmap (chunk :) . go

    -- Tag body: everything up to the first '}', trimmed; must be non-empty.
    tag body
        | T.null close = Left "Unclosed #{ in prompt template"
        | T.null name = Left "Empty variable interpolation in #{}"
        | otherwise = cons (ChunkVar (T.unpack name)) (T.drop 1 close)
      where
        (raw, close) = T.breakOn "}" body
        name = T.strip raw

    -- Literal text: everything up to the next tag opening.
    plain text
        | T.null rest = pure [flush text]
        -- A '\' directly before "#{": the backslash belongs to the opener,
        -- not to the text. Fold it into the literal "#" and resume after "{".
        | Just (before, '\\') <- T.unsnoc beforeTag =
            cons (flush (before <> "#{")) (T.drop 2 rest)
        | otherwise = cons (flush beforeTag) rest
      where
        (beforeTag, rest) = T.breakOn "#{" text

    flush = ChunkLit . T.unpack

    -- Escapes can split one run of text into neighbouring literals; glue
    -- those runs back together so output shape stays predictable.
    mergeLiterals (ChunkLit a : ChunkLit b : rest) = mergeLiterals (ChunkLit (a <> b) : rest)
    mergeLiterals (chunk : rest) = chunk : mergeLiterals rest
    mergeLiterals [] = []

--------------------------------------------------------------------------------
-- Rendering: chunks -> expression of type Text
--------------------------------------------------------------------------------

compilePromptExp :: String -> Q Exp
compilePromptExp input = either failWith render (parsePromptChunks input)
  where
    failWith message = fail ("prompt quasi-quoter syntax error: " ++ message)

    render [] = [|T.empty|]
    render [chunk] = chunkExp chunk
    render chunks = [|T.concat $(ListE <$> traverse chunkExp chunks)|]

    chunkExp (ChunkLit text) = [|T.pack $(litE (StringL text))|]
    chunkExp (ChunkVar name) = [|toPromptArg $(varE (mkName name))|]

--------------------------------------------------------------------------------
-- The quoter
--------------------------------------------------------------------------------

{- | Interpolate variables from the calling scope:

> [prompt|Hello #{name}, you have #{count} messages.|]
-}
prompt :: QuasiQuoter
prompt =
    QuasiQuoter
        { quoteExp = compilePromptExp
        , quotePat = expressionOnly
        , quoteType = expressionOnly
        , quoteDec = expressionOnly
        }
  where
    expressionOnly _ = fail "prompt can only be used as an expression"
