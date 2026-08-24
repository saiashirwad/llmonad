{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Higher-order middleware for client-side rate limiting.
module LLMonad.Middleware.RateLimit (
    RateLimiter (..),
    newRateLimiter,
    withRateLimit,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Data.Time.Clock.POSIX (getPOSIXTime)
import Effectful
import Effectful.Dispatch.Dynamic
import LLMonad.Core
import LLMonad.Types

-- | Rate limiter interface.
data RateLimiter = RateLimiter
    { rlAcquire :: Int -> IO ()
    , rlUpdate :: Maybe Usage -> IO ()
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
                                then pure ((replenished - costD, curNow), (0 :: Double))
                                else do
                                    let deficit = costD - replenished
                                    let waitSecs = deficit / rate
                                    pure ((replenished, curNow), waitSecs)
                        if shouldWait > 0
                            then do
                                threadDelay (ceiling (shouldWait * 1000000))
                                loop
                            else pure ()
                loop
            , rlUpdate = \_ -> pure ()
            }

-- | Interpose rate limiting on the LLM effect.
withRateLimit :: (LLM :> es, IOE :> es) => RateLimiter -> Eff es a -> Eff es a
withRateLimit limiter = interpose_ $ \case
    ChatRound p fmt specs choice -> do
        liftIO (rlAcquire limiter 1)
        resp <- send (ChatRound p fmt specs choice)
        liftIO (rlUpdate limiter (crspUsage resp))
        pure resp
    StreamRound p fmt specs cb -> do
        liftIO (rlAcquire limiter 1)
        resp <- send (StreamRound p fmt specs cb)
        liftIO (rlUpdate limiter (crspUsage resp))
        pure resp
    GetHistory -> send GetHistory
    SetHistory msgs -> send (SetHistory msgs)
    PushMessage msg -> send (PushMessage msg)
    ClearHistory -> send ClearHistory
    GetSystem -> send GetSystem
    SetSystem sys -> send (SetSystem sys)
    ClearSystem -> send ClearSystem
