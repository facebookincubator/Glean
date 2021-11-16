{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Util.Metric (
  Tick(..), Point(..), beginTick, endTick, endTicks,
  showLatency, showThroughput,
  tickLatencyNanoSecs, tickMillis,
  showMemory, showCount
) where

import Data.List (find)
import Data.Word
import System.Clock

data Tick = Tick
  { tickValue :: !Word64
  , tickDuration :: !TimeSpec
  }

instance Semigroup Tick where
  (<>) = mappend

instance Monoid Tick where
  mempty = Tick 0 0
  mappend (Tick v1 d1) (Tick v2 d2) = Tick (v1+v2) (d1+d2)

showLatency :: Tick -> String
showLatency (Tick 0 _) = "-"
showLatency tick = display $ tickLatencyNanoSecs tick
  where
    display n
      | n <= 2000 = show n ++ "ns"
      | n <= 2000000 = show (n `div` 1000) ++ "us"
      | n <= 2000000000 = show (n `div` 1000000) ++ "ms"
      | otherwise = show (n `div` 1000000000) ++ "s"

tickLatencyNanoSecs :: Tick -> Integer
tickLatencyNanoSecs (Tick value time) =
  toNanoSecs time `div` fromIntegral value

showScaled :: [(Word64, String)] -> Word64 -> String
showScaled scale value
  | Just (k,sfx) <- find ((value >=) . fst) scale =
      let n = (value * 10) `div` k
      in
      concat [show $ n `div` 10, ".", show $ n `mod` 10, sfx]
  | otherwise = show value

showThroughput :: Tick -> String
showThroughput tick
  | ms == 0 = "-"
  | otherwise = showScaled deciscale rate ++ "B/s"
  where
    ms = tickMillis tick
    rate = (tickValue tick * 1000) `div` ms

deciscale :: [(Word64, String)]
deciscale = [(1000*1000*1000, "G"), (1000*1000, "M"), (1000, "K")]

tickMillis :: Tick -> Word64
tickMillis (Tick _ time) =
  fromIntegral $ sec time * 1000 + nsec time `div` 1000000

data Point = Point
  { pointValue :: !Word64
  , pointStart :: !TimeSpec
  }

beginTick :: Word64 -> IO Point
beginTick value = Point value <$> getTime Monotonic

endTick :: Point -> IO Tick
endTick (Point value start) = do
  now <- getTime Monotonic
  return $ Tick value (now - start)

endTicks :: [Point] -> IO [Tick]
endTicks ticks = do
  now <- getTime Monotonic
  return [Tick value (now - start) | Point value start <- ticks]

showMemory :: Word64 -> String
showMemory n = showScaled deciscale n ++ "B"

showCount :: Word64 -> String
showCount n = showScaled deciscale n
