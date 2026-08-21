-- | Template Haskell splices and QuasiQuoters.
module LLMonad.TH
  ( prompt
  , makeTool
  , makeToolNamed
  ) where

import Data.Text (Text)
import Language.Haskell.TH
import LLMonad.TH.QuasiQuoter (prompt)

-- | Automatically create a Tool from a function name.
makeTool :: Name -> Q Exp
makeTool _ = error "makeTool: implemented in Milestone 6"

-- | Automatically create a named Tool from a function name.
makeToolNamed :: Text -> Name -> Q Exp
makeToolNamed _ _ = error "makeToolNamed: implemented in Milestone 6"
