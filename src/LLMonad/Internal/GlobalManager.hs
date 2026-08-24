{- | A process-wide HTTP connection manager.

One manager per process means connection pooling and keep-alive work
across every call, without threading a @Manager@ through user code.
-}
module LLMonad.Internal.GlobalManager (
    globalManager,
) where

import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.IO.Unsafe (unsafePerformIO)

-- | Shared TLS-enabled manager. Created lazily on first use.
globalManager :: Manager
globalManager = unsafePerformIO (newManager tlsManagerSettings)
{-# NOINLINE globalManager #-}
