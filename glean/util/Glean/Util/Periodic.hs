{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Util.Periodic (
    doPeriodically,
    doPeriodicallySynchronised
  ) where

import Control.Exception
import Control.Monad

import Util.Control.Exception
import Util.Log
import Util.Time

-- | Run an action every t seconds.  If the action throws an
-- exception, log and swallow it, and then wait t seconds before
-- retrying.
doPeriodically :: DiffTimePoints -> IO () -> IO ()
doPeriodically period action = mask $ \restore ->
  forever $ handler restore period $ do
    t0 <- getTimePoint
    action
    t <- getElapsedTime t0
    delay (addDiffTimePoints period (negate t))

-- | Run an action every t seconds from epoch.  If the action throws an
-- exception, log and swallow it, and then wait t seconds before
-- retrying.
doPeriodicallySynchronised :: DiffTimePoints -> IO () -> IO ()
doPeriodicallySynchronised period action = mask $ \restore ->
  forever $ handler restore period $ do
    now <- getEpochTime
    let
      nanosSinceEpoch = fromIntegral $ toEpochNanos now
      nanosSinceLastTick = nanosSinceEpoch `rem` toDiffNanos period
      durationSinceLastTick = nanoseconds $ fromIntegral nanosSinceLastTick
      durationToNextTick =
        addDiffTimePoints period (negate durationSinceLastTick)
    delay durationToNextTick
    action

handler :: (t -> IO ()) -> DiffTimePoints -> t -> IO ()
handler restore period action = do
  r <- tryAll (restore action)
  case r of
    Left e -> do logError (show e); delay period; return ()
    Right () -> return ()
