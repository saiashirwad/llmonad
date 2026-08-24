{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Higher-order middleware for caching LLM responses.
module LLMonad.Middleware.Cache (
    CacheStore (..),
    newInMemoryCache,
    withCache,
    withCacheModel,
    isCacheableResponse,
) where

import Control.Monad (when)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import LLMonad.Core
import LLMonad.Types

-- | Abstract interface for cache storage.
data CacheStore = CacheStore
    { cacheLookup :: CompletionRequest -> IO (Maybe CompletionResponse)
    , cacheInsert :: CompletionRequest -> CompletionResponse -> IO ()
    }

{- | Check if response is a terminal text response suitable for caching.
Responses containing tool invocations or intermediate tool turns must never be cached.
-}
isCacheableResponse :: CompletionResponse -> Bool
isCacheableResponse resp =
    null (crspToolCalls resp) && crspFinishReason resp /= FrToolUse

-- | Create a thread-safe in-memory cache store using atomic CAS primitives.
newInMemoryCache :: IO CacheStore
newInMemoryCache = do
    ref <- newIORef (Map.empty :: Map (Model, Maybe Text, [ChatMessage], Params, ResponseFormat, [ToolSpec], ToolChoice) CompletionResponse)
    let keyOf req =
            ( crModel req
            , crSystem req
            , crMessages req
            , crParams req
            , crResponseFormat req
            , crTools req
            , crToolChoice req
            )
    pure
        CacheStore
            { cacheLookup = \req -> atomicModifyIORef' ref (\m -> (m, Map.lookup (keyOf req) m))
            , cacheInsert = \req resp ->
                if isCacheableResponse resp
                    then atomicModifyIORef' ref (\m -> (Map.insert (keyOf req) resp m, ()))
                    else pure ()
            }

-- | Interpose caching middleware on the LLM effect with default model identity.
withCache :: (LLM :> es, IOE :> es) => CacheStore -> Eff es a -> Eff es a
withCache = withCacheModel (Model "default-model")

-- | Interpose caching middleware on the LLM effect with an explicit model identity.
withCacheModel :: (LLM :> es, IOE :> es) => Model -> CacheStore -> Eff es a -> Eff es a
withCacheModel model store = interpose_ $ \case
    ChatRound p fmt specs choice -> do
        sys <- getSystem
        hist <- getHistory
        let req =
                CompletionRequest
                    { crModel = model
                    , crSystem = sys
                    , crMessages = hist
                    , crParams = p
                    , crTools = specs
                    , crToolChoice = choice
                    , crResponseFormat = fmt
                    }
        mHit <- liftIO (cacheLookup store req)
        case mHit of
            Just cachedResp -> do
                pushMessage (AssistantMsg (crspText cachedResp) (crspToolCalls cachedResp))
                pure cachedResp
            Nothing -> do
                resp <- send (ChatRound p fmt specs choice)
                when (isCacheableResponse resp) $
                    liftIO (cacheInsert store req resp)
                pure resp
    StreamRound p fmt specs cb -> send (StreamRound p fmt specs cb)
    GetHistory -> send GetHistory
    SetHistory msgs -> send (SetHistory msgs)
    PushMessage msg -> send (PushMessage msg)
    ClearHistory -> send ClearHistory
    GetSystem -> send GetSystem
    SetSystem sys -> send (SetSystem sys)
    ClearSystem -> send ClearSystem
