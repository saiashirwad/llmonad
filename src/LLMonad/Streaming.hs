{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- | Token streaming utilities for LLMonad.
module LLMonad.Streaming (
    streamSSE,
) where

import Data.Text (Text)
import Effectful
import LLMonad.Core
import LLMonad.Types

-- | Stream SSE tokens from the model.
streamSSE :: (LLM :> es) => Params -> [ToolSpec] -> (Text -> IO ()) -> Eff es Text
streamSSE p specs cb = do
    resp <- streamRound p RfText specs forward
    pure (crspText resp)
  where
    forward ev = case ev of
        SEText t -> cb t
        SEFinished _ -> pure ()
