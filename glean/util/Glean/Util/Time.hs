{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE NamedFieldPuns #-}
module Glean.Util.Time
( -- * Util.TimeSec
  module Util.TimeSec
  -- * absolute time
, EpochClock(..), getEpochTime, toEpochSeconds
, epochClockToUTCTime, showUTC, readUTC, showEpochTime, showNominalDiffTime
  -- * time point differences
, TimePoint(..), DiffTimePoints(..), getTimePoint
, diffTimePoints, addDiffTimePoints, getElapsedTime, addToTimePoint
, toDiffMillis, toDiffMicros, toDiffNanos, toDiffSeconds
  -- * utils
, delay, nanoseconds, seconds, minutes, hours
) where

import Control.Concurrent
import Data.Ratio
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc as P
import qualified Data.Time.Clock as TC ( UTCTime, NominalDiffTime )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime, POSIXTime )
import Data.Time.Format ( defaultTimeLocale, formatTime, parseTimeM )
import qualified System.Clock as SC

import Util.TimeSec
  ( Time(..), TimeSpan(..), toUTCTime, fromUTCTime, ppTimeSpan )

newtype EpochClock = EpochClock SC.TimeSpec deriving (Eq, Ord, Show)

-- | Use 'Clock' of 'Realtime'
getEpochTime :: IO EpochClock
getEpochTime = EpochClock <$> SC.getTime SC.Realtime

-- | Using this with 'getEpochTime' and 'showEpochTime' agrees with running
--
-- > /usr/local/bin/date +'%s'
toEpochSeconds :: EpochClock -> Double
toEpochSeconds (EpochClock SC.TimeSpec{sec, nsec}) = fromRational $
  fromIntegral sec + (fromIntegral nsec % 1000000000)

-- | Using this with 'getEpochTime' and 'showEpochTime' agrees with running
--
-- > /usr/local/bin/date --rfc-3339=ns
epochClockToUTCTime :: EpochClock -> TC.UTCTime
epochClockToUTCTime (EpochClock SC.TimeSpec{sec, nsec}) =
  let posixSeconds :: POSIXTime
      posixSeconds = fromRational $
        fromIntegral sec + (fromIntegral nsec % 1000000000)
  in posixSecondsToUTCTime posixSeconds

-- | For seconds, this has variable number of digits after decimal point.
-- We want to log RFC 3339 format
showUTC :: TC.UTCTime -> T.Text
showUTC = T.pack . formatTime defaultTimeLocale "%FT%X%QZ"

readUTC :: T.Text -> Maybe TC.UTCTime
readUTC = parseTimeM True defaultTimeLocale "%FT%X%QZ" . T.unpack

-- | We want to log RFC 3339 format
showEpochTime :: EpochClock -> T.Text
showEpochTime = showUTC . epochClockToUTCTime

showNominalDiffTime :: TC.NominalDiffTime -> String
showNominalDiffTime t = T.unpack (ppTimeSpan (TimeSpan (round t)))

-- -----------------------------------------------------------------------------

newtype TimePoint = TimePoint SC.TimeSpec deriving (Eq, Ord, Show)

instance P.Pretty TimePoint where
  pretty (TimePoint SC.TimeSpec{..}) = P.pretty $ "TimePoint "
    <> T.pack (show sec) <> "." <> T.justifyRight 9 '0' (T.pack (show nsec))

-- | Use 'Clock' of 'MonotonicRaw'
getTimePoint :: IO TimePoint
getTimePoint = TimePoint <$> SC.getTime SC.MonotonicRaw

newtype DiffTimePoints = DiffTimePoints SC.TimeSpec
  deriving (Eq, Ord, Show, Num)

instance P.Pretty DiffTimePoints where
  pretty (DiffTimePoints SC.TimeSpec{..}) = P.pretty $ "DiffTimePoints "
    <> T.pack (show sec) <> "." <> T.justifyRight 9 '0' (T.pack (show nsec))

addDiffTimePoints :: DiffTimePoints -> DiffTimePoints -> DiffTimePoints
addDiffTimePoints (DiffTimePoints a) (DiffTimePoints b) = DiffTimePoints (a+b)

-- | This takes the earlier (smaller) then the later (larger) 'TimePoint'
diffTimePoints :: TimePoint -> TimePoint -> DiffTimePoints
diffTimePoints (TimePoint small) (TimePoint big) =
  DiffTimePoints (big - small)

addToTimePoint :: TimePoint -> DiffTimePoints -> TimePoint
addToTimePoint (TimePoint time) (DiffTimePoints diff) =
  TimePoint (time + diff)

getElapsedTime :: TimePoint -> IO DiffTimePoints
getElapsedTime small = do
  big <- getTimePoint
  return (diffTimePoints small big)

-- | We want to log milliseconds, use 'round'
toDiffMillis :: DiffTimePoints -> Int
toDiffMillis (DiffTimePoints SC.TimeSpec{sec,nsec}) =
  1000 * fromIntegral sec +
    (round :: Rational -> Int) (fromIntegral nsec % 1000000)

toDiffMicros :: DiffTimePoints -> Int
toDiffMicros (DiffTimePoints SC.TimeSpec{sec,nsec}) =
  1000000 * fromIntegral sec +
    (round :: Rational -> Int) (fromIntegral nsec % 1000)

toDiffNanos :: DiffTimePoints -> Int
toDiffNanos (DiffTimePoints SC.TimeSpec{sec,nsec}) =
  1000000000 * fromIntegral sec + fromIntegral nsec

toDiffSeconds :: DiffTimePoints -> Double
toDiffSeconds (DiffTimePoints SC.TimeSpec{sec,nsec}) =
  fromIntegral sec + 1e-9 * fromIntegral nsec

delay :: DiffTimePoints -> IO ()
delay = threadDelay . toDiffMicros

nanoseconds :: Int -> DiffTimePoints
nanoseconds n = DiffTimePoints (SC.fromNanoSecs (fromIntegral n))

seconds :: Int -> DiffTimePoints
seconds s = DiffTimePoints (SC.TimeSpec (fromIntegral s) 0)

minutes :: Int -> DiffTimePoints
minutes m = seconds (m * 60)

hours :: Int -> DiffTimePoints
hours h = minutes (h * 60)
