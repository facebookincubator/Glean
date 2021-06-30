-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.Util.Mutex (
  Mutex, newMutex, withMutex, withMutex_, tryWithMutex
) where

import Control.Concurrent.MVar
import Control.Exception (bracket)

newtype Mutex a = Mutex (MVar a)

newMutex :: a -> IO (Mutex a)
newMutex x = Mutex <$> newMVar x

withMutex :: Mutex a -> (a -> IO b) -> IO b
withMutex (Mutex v) = withMVar v

withMutex_ :: Mutex a -> IO b -> IO b
withMutex_ m = withMutex m . const

tryWithMutex :: Mutex a -> (a -> IO b) -> IO (Maybe b)
tryWithMutex (Mutex v) f =
  bracket
    (tryTakeMVar v)
    (maybe (return ()) $ putMVar v)
    (traverse f)
