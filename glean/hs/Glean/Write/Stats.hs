{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Write.Stats (
  Stats, new, Bump, bump, tick,
  mutatorLatency, mutatorInput, mutatorThroughput, mutatorDedupedThroughput,
  mutatorDupThroughput,
  renameThroughput, commitThroughput,
  lookupCacheStats
) where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Char (toLower)
import Data.Functor.Contravariant
import Data.IORef
import Data.Word
import System.Clock (TimeSpec)

import qualified Glean.RTS.Foreign.LookupCache as LookupCache

import ServiceData.GlobalStats
import ServiceData.Types
import Util.Log

import Glean.Util.Metric
import Glean.Util.Throttle

data Stat a = Stat
  { statNew :: !a
  , statAll :: !a
  }

data Stats = Stats
  { statsLogger :: Execute
  , statsMutatorLatency :: !(IORef (Stat Tick))
  , statsMutatorInput :: !(IORef (Stat Tick))
  , statsMutatorThroughput :: !(IORef (Stat Tick))
  , statsMutatorDedupedThroughput :: !(IORef (Stat Tick))
  , statsMutatorDupThroughput :: !(IORef (Stat Tick))
  , statsRenameThroughput :: !(IORef (Stat Tick))
  , statsCommitThroughput :: !(IORef (Stat Tick))
  , statsLookupCache :: !(IORef (Stat LookupCache.StatValues))
  }

data Ref = forall a. Monoid a => Ref (Stats -> IORef (Stat a))

refs :: [Ref]
refs =
  [ Ref statsMutatorLatency
  , Ref statsMutatorInput
  , Ref statsMutatorThroughput
  , Ref statsMutatorDedupedThroughput
  , Ref statsMutatorDupThroughput
  , Ref statsRenameThroughput
  , Ref statsCommitThroughput
  , Ref statsLookupCache
  ]

bumper :: Monoid a => (Stats -> IORef (Stat a)) -> Bump a
bumper f = Bump $ \stats x -> atomicModifyIORef' (f stats) $
  \(Stat new all) -> (Stat (new <> x) (all <> x), ())

-- | A metric that can be bumped using 'bump'.  Metrics of the same
-- type can be bumped together using the `Monoid` instance.
newtype Bump a = Bump { runBump :: Stats -> a -> IO () }

instance Semigroup (Bump a) where
  Bump p <> Bump q = Bump $ \stats a -> p stats a >> q stats a

instance Monoid (Bump a) where
  mempty = Bump $ const $ const $ return ()

instance Contravariant Bump where
  contramap f (Bump g) = Bump $ \stats a -> g stats (f a)

-- | The latency between a write request being received, and it being completed.
mutatorLatency :: Bump Tick
mutatorLatency =
  bumper statsMutatorLatency <>
  bumpServiceCounterLatency "glean.db.write.latency_ms"

-- | The throughput of writes in bytes per unit time
mutatorThroughput :: Bump Tick
mutatorThroughput =
  bumper statsMutatorThroughput <>
  contramap tickValue (bumpServiceCounter "glean.db.write.throughput")

mutatorInput :: Bump Tick
mutatorInput =
  bumper statsMutatorInput

-- | The throughput of writes in bytes per unit time
mutatorDedupedThroughput :: Bump Tick
mutatorDedupedThroughput =
  bumper statsMutatorDedupedThroughput <>
  contramap tickValue (bumpServiceCounter "glean.db.write.deduped.bytes")

-- | The throughput of writes in bytes per unit time
mutatorDupThroughput :: Bump Tick
mutatorDupThroughput =
  bumper statsMutatorDupThroughput <>
  contramap tickValue (bumpServiceCounter "glean.db.write.undeduped.bytes") <>
  contramap tickMillis (bumpServiceCounter "glean.db.write.undeduped.ms")

-- | The throughput of renames in bytes per unit time
renameThroughput :: Bump Tick
renameThroughput =
  bumper statsRenameThroughput <>
  contramap tickValue (bumpServiceCounter "glean.db.write.rename.bytes") <>
  contramap tickMillis (bumpServiceCounter "glean.db.write.rename.ms")

-- | The throughput of commits in bytes per unit time
commitThroughput :: Bump Tick
commitThroughput =
  bumper statsCommitThroughput <>
  contramap tickValue (bumpServiceCounter "glean.db.write.commit.bytes") <>
  contramap tickMillis (bumpServiceCounter "glean.db.write.commit.ms")

lookupCacheStats :: Bump LookupCache.StatValues
lookupCacheStats = mconcat $ bumper statsLookupCache
  : [contramap (`LookupCache.getStat` c) bump | (c,bump) <- lookupCacheCounters]

lookupCacheCounters :: [(LookupCache.Stat, Bump Word64)]
lookupCacheCounters =
  [(c, bump c $ Char8.pack $ "glean.db.write.cache." ++ map toLower (show c))
    | c <- [minBound .. maxBound]]
  where
    bump c
      | LookupCache.isCounter c = bumpServiceCounter
      | otherwise = setServiceCounter

-- | Bump an fb303 counter representing latency
bumpServiceCounterLatency :: ByteString -> Bump Tick
bumpServiceCounterLatency counter = Bump $ \_ tick ->
  let ms = fromIntegral (tickLatencyNanoSecs tick `div` 1000000) in
  addStatValueType counter ms Avg

-- | Bump an fb303 counter
bumpServiceCounter :: ByteString -> Bump Word64
bumpServiceCounter counter = Bump $ \_ value ->
  addStatValueType counter (fromIntegral value) Sum

-- | Set an fb303 counter
setServiceCounter :: ByteString -> Bump Word64
setServiceCounter counter = Bump $ \_ value ->
  void $ setCounter counter (fromIntegral value)

allStats :: [(String, Stats -> IO String)]
allStats =
  [("mut_lat", counter showLatency statsMutatorLatency)
  ,("mut_thp", counter showThroughput statsMutatorThroughput)
  ,("ded_thp", counter showThroughput statsMutatorDedupedThroughput)
  ,("dup_thp", counter showThroughput statsMutatorDupThroughput)
  ,("rnm_thp", counter showThroughput statsRenameThroughput)
  ,("cmt_thp", counter showThroughput statsCommitThroughput)
  ,("ibk_mis", counter
    (showMissRate LookupCache.IdByKey_hits LookupCache.IdByKey_misses)
    statsLookupCache)
  ,("tbi_mis", counter
    (showMissRate LookupCache.TypeById_hits LookupCache.TypeById_misses)
    statsLookupCache)
  ,("fbi_mis", counter
    (showMissRate LookupCache.FactById_hits LookupCache.FactById_misses)
    statsLookupCache)
  ,("lch_mem", value
    (LookupCache.Fact_bytes `using` showMemory)
    statsLookupCache)
  ,("lch_cnt", value
    (LookupCache.Fact_count `using` showCount)
    statsLookupCache)
  ]
  where
    counter :: Monoid a
            => (a -> String) -> (Stats -> IORef (Stat a)) -> Stats -> IO String
    counter shw get stats = do
      stat <- readIORef (get stats)
      return $ concat [shw $ statNew stat, " [", shw $ statAll stat, "]"]

    showMissRate h m stats = pad 5 $ case hits+misses of
      0 -> "-"
      n ->
        let !rate = (misses * 1000) `div` n
        in concat [show (rate `div` 10), ".", show (rate `mod` 10), "%"]
      where
        hits = LookupCache.getStat stats h
        misses = LookupCache.getStat stats m

    value :: (a -> String) -> (Stats -> IORef (Stat a)) -> Stats -> IO String
    value shw get stats = shw . statNew <$> readIORef (get stats)

    using c f stats = f (LookupCache.getStat stats c)

class Initial a where
  initial :: IO a -> IO Stats

instance Initial Stats where
  initial = id

instance (Monoid a, Initial b) => Initial (IORef (Stat a) -> b) where
  initial p = initial $ do
    f <- p
    f <$> newIORef (Stat mempty mempty)

new :: TimeSpec -> IO Stats
new period = initial $ Stats <$> atMostEvery period

-- | Bump a metric
bump :: Stats -> Bump a -> a -> IO ()
bump stats b a = do
  runBump b stats a
  statsLogger stats $ do
    s <- fmap unwords $ forM allStats $ \(tag, f) -> do
      s <- f stats
      return $ tag ++ ": " ++ s
    logInfo s
    forM_ refs $ \(Ref f) -> atomicModifyIORef' (f stats) $
      \stat -> (stat { statNew = mempty }, ())

tick :: Stats -> Bump Tick -> Word64 -> IO a -> IO a
tick stats b val io = do
  p <- beginTick val
  x <- io
  t <- endTick p
  bump stats b t
  return x
