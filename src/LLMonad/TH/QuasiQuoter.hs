-- | Template Haskell QuasiQuoter for prompts.
module LLMonad.TH.QuasiQuoter
  ( prompt
  ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))

-- | QuasiQuoter for prompt templates with #{var} interpolation.
prompt :: QuasiQuoter
prompt = QuasiQuoter
  { quoteExp = \_ -> error "prompt QuasiQuoter: implemented in Milestone 6"
  , quotePat = \_ -> error "prompt QuasiQuoter not supported for patterns"
  , quoteType = \_ -> error "prompt QuasiQuoter not supported for types"
  , quoteDec = \_ -> error "prompt QuasiQuoter not supported for declarations"
  }
