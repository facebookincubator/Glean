{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Util.Mutex (
  Mutex, newMutex, withMutex, withMutexSafe, withMutex_,
  tryWithMutex, tryWithMutexSafe,
) where

import Control.Concurrent.MVar
import Control.Exception (bracket)

newtype Mutex a = Mutex (MVar a)

newMutex :: a -> IO (Mutex a)
newMutex x = Mutex <$> newMVar x

withMutex :: Mutex a -> (a -> IO b) -> IO b
withMutex (Mutex v) = withMVar v

withMutexSafe :: Mutex (f a) -> (forall s . f s -> IO b) -> IO b
withMutexSafe (Mutex v) = withMVar v

withMutex_ :: Mutex a -> IO b -> IO b
withMutex_ m = withMutex m . const

tryWithMutex :: Mutex a -> (a -> IO b) -> IO (Maybe b)
tryWithMutex (Mutex v) f =
  bracket
    (tryTakeMVar v)
    (maybe (return ()) $ putMVar v)
    (traverse f)

tryWithMutexSafe :: Mutex (f a) -> (forall s . f s -> IO b) -> IO (Maybe b)
tryWithMutexSafe = tryWithMutex
