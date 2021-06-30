-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.Util.Throttle (
  Execute, atMostEvery
) where

import Control.Monad
import Data.IORef
import System.Clock

type Execute = IO () -> IO ()

atMostEvery :: TimeSpec -> IO Execute
atMostEvery tm = do
  now <- getTime Monotonic
  ref <- newIORef now
  return $ \action -> do
    now <- getTime Monotonic
    go <- atomicModifyIORef' ref $ \next ->
      if now >= next
        then (now + tm, True)
        else (next, False)
    when go action
