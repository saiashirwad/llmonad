{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Higher-order middleware for caching LLM responses.
module LLMonad.Middleware.Cache
  ( CacheStore (..)
  , newInMemoryCache
  , withCache
  ) where

import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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

-- | Create a thread-safe in-memory cache store.
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
  pure CacheStore
    { cacheLookup = \req -> Map.lookup (keyOf req) <$> readIORef ref
    , cacheInsert = \req resp -> modifyIORef' ref (Map.insert (keyOf req) resp)
    }

-- | Interpose caching middleware on the LLM effect.
withCache :: (LLM :> es, IOE :> es) => CacheStore -> Eff es a -> Eff es a
withCache store = interpose_ $ \case
  ChatRound p fmt specs choice -> do
    sys <- getSystem
    hist <- getHistory
    let req = CompletionRequest
          { crModel = Model "cached"
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
