{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

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
