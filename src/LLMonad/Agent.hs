{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Autonomous multi-step ReAct agent execution.
module LLMonad.Agent
  ( runAgent
  , runAgentStructured
  ) where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import Effectful
import LLMonad.Core
import LLMonad.Schema (ToSchema)
import LLMonad.Tools (Tool)

-- | Run autonomous ReAct agent loop.
runAgent :: (LLM :> es, IOE :> es) => [Tool] -> Text -> Eff es Text
runAgent _ _ = error "runAgent: implemented in Milestone 4"

-- | Run autonomous ReAct agent loop returning structured output.
runAgentStructured :: forall a es. (LLM :> es, IOE :> es, FromJSON a, ToSchema a) => [Tool] -> Text -> Eff es a
runAgentStructured _ _ = error "runAgentStructured: implemented in Milestone 4"
