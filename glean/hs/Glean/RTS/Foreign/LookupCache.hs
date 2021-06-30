-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.RTS.Foreign.LookupCache
 ( LookupCache, new, clear, withCache
 , Stats, StatValues, Stat(..)
 , isCounter, getStat, newStats, readStatsAndResetCounters
 )
where

import Control.Exception
import Data.List
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr

import Glean.FFI
import Glean.RTS.Foreign.Lookup

newtype LookupCache = LookupCache (ForeignPtr LookupCache)

instance Object LookupCache where
  wrap = LookupCache
  unwrap (LookupCache p) = p
  destroy = glean_lookupcache_free

-- | Pair the 'LookupCache' with a base 'Lookup' for looking up facts. It is
-- the user's responsibility to ensure that the 'Lookup's are morally the same
-- throughout the cache's lifetime (such as different instances of the same
-- database).
withCache :: CanLookup base => base -> LookupCache -> (Lookup -> IO a) -> IO a
withCache base cache f =
  withLookup base $ \base_lookup ->
  with cache $ \cache_ptr ->
  bracket
    (invoke $ glean_lookupcache_anchor_new base_lookup cache_ptr)
    glean_lookupcache_anchor_free
    f

-- | The 'Stat' object can be shared between different 'LookupCache's which will
-- accumulate their statistics into it (cf. 'new').
newtype Stats = Stats (ForeignPtr Stats)

instance Object Stats where
  wrap = Stats
  unwrap (Stats p) = p
  destroy = glean_lookupcache_stats_free


-- | An approximate snapshot of the values in a 'Stats' object
newtype StatValues = StatValues (V.Vector Word64)

instance Semigroup StatValues where
  StatValues old <> StatValues new = StatValues $
    V.izipWith (\i o n -> if isCounter (toEnum i) then o+n else n) old new

instance Monoid StatValues where
  mempty = StatValues $ V.replicate sTAT_COUNT 0

instance Show StatValues where
  show statValues = intercalate ", " $ map (f statValues) $ enumFrom minBound
    where
      f statValues stat =
        show stat <> ": " <> show (getStat statValues stat)

-- NOTE: This must be kept in sync with the stats in rts/cache.h
data Stat
  = IdByKey_hits
  | IdByKey_misses
  | IdByKey_failures
  | IdByKey_deletes
  | TypeById_hits
  | TypeById_misses
  | TypeById_failures
  | FactById_hits
  | FactById_misses
  | FactById_failures
  | FactById_deletes

    -- slightly ugly names because we want them to be nice on ODS
  | Fact_bytes
  | Fact_count
  deriving(Eq,Ord,Enum,Bounded,Show)

sTAT_COUNT :: Int
sTAT_COUNT = fromEnum (maxBound :: Stat) + 1

lAST_COUNTER :: Stat
lAST_COUNTER = FactById_deletes

-- | Counters are values which we want to bump in ODS; the others we want to
-- set.
isCounter :: Stat -> Bool
isCounter x = x <= lAST_COUNTER

getStat :: StatValues -> Stat -> Word64
getStat (StatValues as) c = as V.! fromEnum c

newStats :: IO Stats
newStats = construct $ invoke glean_lookupcache_stats_new

-- | Obtain an approximate snapshot of the values in 'Stats' and reset all
-- counters to 0 (but not sums like total cache size).
readStatsAndResetCounters :: Stats -> IO StatValues
readStatsAndResetCounters stats = do
  v <- VM.new sTAT_COUNT
  with stats $ \pstats ->
    VM.unsafeWith v $ \pdata ->
    glean_lookupcache_stats_read_and_reset_counters
      pstats
      pdata
      (fromIntegral $ VM.length v)
  StatValues <$> V.unsafeFreeze v

new :: Int -> Int -> Stats -> IO LookupCache
new capacity shards stats =
  with stats $ construct . invoke . glean_lookupcache_new
    (fromIntegral capacity)
    (fromIntegral shards)

clear :: LookupCache -> IO ()
clear cache = with cache $ invoke . glean_lookupcache_clear

foreign import ccall unsafe glean_lookupcache_stats_new
  :: Ptr (Ptr Stats) -> IO CString
foreign import ccall unsafe "&glean_lookupcache_stats_free"
  glean_lookupcache_stats_free :: Destroy Stats
foreign import ccall safe glean_lookupcache_stats_read_and_reset_counters
  :: Ptr Stats -> Ptr Word64 -> CSize -> IO ()


foreign import ccall unsafe glean_lookupcache_new
  :: CSize
  -> CSize
  -> Ptr Stats
  -> Ptr (Ptr LookupCache)
  -> IO CString
foreign import ccall unsafe "&glean_lookupcache_free" glean_lookupcache_free
  :: Destroy LookupCache
foreign import ccall safe glean_lookupcache_clear
  :: Ptr LookupCache -> IO ()

foreign import ccall unsafe glean_lookupcache_anchor_new
  :: Lookup
  -> Ptr LookupCache
  -> Ptr Lookup
  -> IO CString
foreign import ccall unsafe glean_lookupcache_anchor_free
  :: Lookup -> IO ()
