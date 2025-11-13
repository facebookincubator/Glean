{-
  Derived from Data.ConcurrentCache in static-ls

  Copyright (c) 2023 Joseph Sumabat
-}

module Data.ConcurrentCache (
  ConcurrentCache,
  new,
  flush,
  remove,
  insert
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import UnliftIO.Exception
import UnliftIO.IORef
import UnliftIO.MVar

newtype ConcurrentCache k v = ConcurrentCache {
  map :: IORef (HashMap k (MVar (Maybe v)))
    -- k -> missing => computation for k not cached
    -- k -> MVar empty => computation for k in progress
    -- k -> MVar (Just v) => computation for k cached with value v
    -- k -> MVar Nothing => computation for k failed, try again
}

new :: (MonadIO m) => m (ConcurrentCache k v)
new = do
  map <- newIORef HashMap.empty
  pure ConcurrentCache {map}

flush :: (MonadIO m) => ConcurrentCache k v -> m ()
flush cache = writeIORef cache.map HashMap.empty

remove :: (Hashable k, MonadIO m) => k -> ConcurrentCache k v -> m ()
remove k cache = do
  atomicModifyIORef' cache.map $ \m -> do
    (HashMap.delete k m, ())

-- | Perform a computation and insert its result into the cache. If
-- multiple threads try to insert a computation for the same key, one
-- will perform its computation while the others wait. If the
-- computation throws, another thread will try its computation.
insert :: (Hashable k, MonadUnliftIO m) => k -> ConcurrentCache k v -> m v -> m v
insert k cache act = mask $ \restore -> do
  var <- newEmptyMVar
  existed <- atomicModifyIORef' cache.map $ \m -> do
    case HashMap.lookup k m of
      Nothing -> (HashMap.insert k var m, Nothing)
      Just otherVar -> (m, Just otherVar)
  case existed of
    Nothing -> do
      v <-
        (restore act) `onException` do
          remove k cache
          putMVar var Nothing
      putMVar var (Just v)
      pure v
    Just otherVar ->
      join $ restore $ do
        v <- readMVar otherVar
        case v of
          Just v -> return $ return v
          Nothing ->
            -- try again; the key was removed from the cache
            return $ insert k cache act
