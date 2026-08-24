{-# LANGUAGE TemplateHaskell #-}

{- | QuasiQuoter for prompt templates with variable interpolation.

> query = [prompt|Customer #{customer} owes $#{amount}. \#{not-a-tag}|]

Grammar:

> template := (literal | escape | tag)*
> escape   := "\#{"                    -- renders as the literal text "#{"
> tag      := "#{" whitespace name whitespace "}"

The body is dedented before parsing, so a multi-line template can be indented
to match the code around it. See 'dedentTemplate'.
-}
module LLMonad.TH.QuasiQuoter (
    prompt,
    parsePromptChunks,
    dedentTemplate,
    PromptChunk (..),
) where

import Data.Char (isSpace)
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
-- Layout: strip the indentation the call site imposes
--------------------------------------------------------------------------------

{- | Remove the layout the surrounding code forces on a quoted body.

Three steps: drop an opening blank line, strip the smallest indent every
non-blank line shares, drop a closing line made only of whitespace. So

> [prompt|
>     Review #{path}.
>     Quote the offending line.
> |]

carries no leading spaces and no trailing newline. Single-line templates and
templates already written flush left come through unchanged, and indentation
inside an interpolated value is never touched.
-}
dedentTemplate :: String -> String
dedentTemplate = T.unpack . T.intercalate "\n" . stripIndent . trimEdges . T.lines . T.pack
  where
    trimEdges = dropClosing . dropOpening

    dropOpening (firstLine : rest) | blank firstLine = rest
    dropOpening lns = lns

    dropClosing lns
        | not (null lns), blank (last lns) = init lns
        | otherwise = lns

    stripIndent lns = map (T.drop (commonIndent lns)) lns

    commonIndent lns = case map indentOf (filter (not . blank) lns) of
        [] -> 0
        indents -> minimum indents

    indentOf = T.length . T.takeWhile (== ' ')

    blank = T.all isSpace

--------------------------------------------------------------------------------
-- Rendering: chunks -> expression of type Text
--------------------------------------------------------------------------------

compilePromptExp :: String -> Q Exp
compilePromptExp input = either failWith render (parsePromptChunks (dedentTemplate input))
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

A multi-line body may be indented; 'dedentTemplate' strips the layout back off.
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
