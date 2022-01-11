{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Util.Periodic (
    doPeriodically
  ) where

import Control.Exception
import Control.Monad

import Util.Control.Exception
import Util.Log

import Glean.Util.Time

-- | Run an action every t seconds.  If the action throws an
-- exception, log and swallow it, and then wait t seconds before
-- retrying.
doPeriodically :: DiffTimePoints -> IO () -> IO ()
doPeriodically period action = mask $ \restore -> forever $ handler restore $ do
  t0 <- getTimePoint
  action
  t <- getElapsedTime t0
  delay (addDiffTimePoints period (negate t))
 where
  handler restore action = do
    r <- tryAll (restore action)
    case r of
      Left e -> do logError (show e); delay period; return ()
      Right () -> return ()
