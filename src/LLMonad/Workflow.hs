{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- | Safe concurrency combinators for Effectful workflows.
module LLMonad.Workflow (
    concurrently,
    race,
    mapConcurrentlyN,
    WorkflowError (..),
) where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (Exception, bracket_, throwIO)
import Effectful

-- | Invalid workflow execution policy.
newtype WorkflowError = InvalidConcurrencyLimit Int
    deriving (Eq, Show)

instance Exception WorkflowError

-- | Run two effects concurrently. If either effect fails, cancel the other.
concurrently :: (IOE :> es) => Eff es a -> Eff es b -> Eff es (a, b)
concurrently left right =
    withEffToIO (ConcUnlift Ephemeral Unlimited) $ \unlift ->
        Async.concurrently (unlift left) (unlift right)

-- | Return the first result and cancel the other effect.
race :: (IOE :> es) => Eff es a -> Eff es a -> Eff es a
race left right =
    withEffToIO (ConcUnlift Ephemeral Unlimited) $ \unlift -> do
        result <- Async.race (unlift left) (unlift right)
        pure (either id id result)

{- | Map an effect over a list with a fixed concurrency limit. If one effect
fails, cancel all remaining effects.
-}
mapConcurrentlyN :: (IOE :> es) => Int -> (a -> Eff es b) -> [a] -> Eff es [b]
mapConcurrentlyN limit run values
    | limit <= 0 = liftIO (throwIO (InvalidConcurrencyLimit limit))
    | otherwise =
        withEffToIO (ConcUnlift Ephemeral Unlimited) $ \unlift -> do
            semaphore <- newQSem limit
            Async.mapConcurrently
                (\value -> bracket_ (waitQSem semaphore) (signalQSem semaphore) (unlift (run value)))
                values
