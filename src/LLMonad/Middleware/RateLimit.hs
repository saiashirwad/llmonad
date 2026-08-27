{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- | Higher-order middleware for client-side rate limiting.

Attach to a single agent's runtime with 'rateLimited', or scope over an
entire effect block with 'withRateLimit'. Both spellings share
'rateLimitHandler'.
-}
module LLMonad.Middleware.RateLimit (
    RateLimiter (..),
    newRateLimiter,
    withRateLimit,
    rateLimited,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Monad (when)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Effectful
import Effectful.Dispatch.Dynamic
import LLMonad.Core
import LLMonad.Middleware (Middleware (..))
import LLMonad.Model (ModelRuntime (..))
import LLMonad.Types

-- | Rate limiter interface.
data RateLimiter = RateLimiter
    { rlAcquire :: Int -> IO ()
    }

{- | Construct a token-bucket rate limiter.
Parameters: tokensPerSecond, maxBurstCapacity.
-}
newRateLimiter :: Double -> Double -> IO RateLimiter
newRateLimiter rate capacity = do
    now <- realToFrac <$> getPOSIXTime
    bucketVar <- newMVar (capacity, now :: Double)
    pure
        RateLimiter
            { rlAcquire = \cost -> do
                let costD = fromIntegral cost
                let loop = do
                        curNow <- realToFrac <$> getPOSIXTime
                        shouldWait <- modifyMVar bucketVar $ \(tokens, lastTime) -> do
                            let elapsed = max 0 (curNow - lastTime)
                            let replenished = min capacity (tokens + elapsed * rate)
                            if replenished >= costD
                                then pure ((replenished - costD, curNow), 0 :: Double)
                                else do
                                    let deficit = costD - replenished
                                    let waitSecs = deficit / rate
                                    pure ((replenished, curNow), waitSecs)
                        when (shouldWait > 0) $ do
                            threadDelay (ceiling (shouldWait * 1000000))
                            loop
                loop
            }

{- | Handler interposing rate limiting on the 'LLM' effect.

Each round acquires one token before reaching the wrapped interpreter; the
call blocks until the bucket can cover it.
-}
rateLimitHandler :: forall es. (LLM :> es, IOE :> es) => RateLimiter -> EffectHandler_ LLM es
rateLimitHandler limiter = \case
    ChatRound p fmt specs choice -> limited (send (ChatRound p fmt specs choice))
    StreamRound p fmt specs cb -> limited (send (StreamRound p fmt specs cb))
    GetHistory -> send GetHistory
    SetHistory msgs -> send (SetHistory msgs)
    PushMessage msg -> send (PushMessage msg)
    ClearHistory -> send ClearHistory
    GetSystem -> send GetSystem
    SetSystem sys -> send (SetSystem sys)
    ClearSystem -> send ClearSystem
  where
    limited ::
        Eff es CompletionResponse ->
        Eff es CompletionResponse
    limited action = do
        liftIO (rlAcquire limiter 1)
        resp <- action
        pure resp

-- | Rate limiting as first-class model middleware; attach to individual runtimes.
rateLimited :: (IOE :> es) => RateLimiter -> Middleware es
rateLimited limiter = Middleware $ \(ModelRuntime run) ->
    ModelRuntime $ \action ->
        run (interpose_ (rateLimitHandler limiter) action)

-- | Interpose rate limiting on the LLM effect.
withRateLimit :: (LLM :> es, IOE :> es) => RateLimiter -> Eff es a -> Eff es a
withRateLimit limiter = interpose_ (rateLimitHandler limiter)
