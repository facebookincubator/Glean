{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}


-- | Simple first-in, first-out cache holding a limited number of MonadIO
-- results.  This is ideal for repeated operations that expensive and
-- idempotent, such as queries.
--
-- When the number of items is more that the "high water mark" then
-- the older items are removed so that only "low water mark" remain.  The gap
-- between high and low values reduces the frequency of this garbage collection.
-- For example, I am aiming for a low water mark of half the high water mark.
--
-- Assumes 'withCacher' will not be called enough times to overflow
-- 'Word64' (within the high water mark).
--
-- Design of 'withCacher c f k' operation:
--
-- * If 'withCacher' find the value for 'k' the 'c_cache' then it returns
-- the cached value without blocking. Implemented here by reading
-- two 'IORefs'.
--
-- * If 'withCacher' does not find the value in the 'c_cache' then it
-- tries to run the operation 'f' on 'k' and add it to the cache
--
-- * When the operation 'f' is being run on 'k', additional requests for 'k'
-- should block instead of running 'f' on 'k' concurrently.
--
--     * This is \"should block\" instead of \"must block\" because
--     pending operations count against the high/low water mark
--
--     * If you have more pending keys than the low water mark then the cache
--     will lose track and the next 'withCacher' may run a new operation 'f'
--     on a 'k' that is already running concurrently
--
--     * Note that for 'q' not equal to 'k' the operation 'f' on 'q' will
--     happily run concurrently to 'f' on 'k', as you hopefully expected
--
-- * If 'withCacher' or the operation 'f' on 'k' fails to produce a value, e.g.
-- due to an exception, then
--
--     *  none of the properties or invariants break
--
--     * this exception bubbles up to the caller of 'withCacher' that was
--     running 'f'
--
--     * concurrent 'withCacher f k' that blocked will all wake up and try
--     again, and will they will not see the exception
--
--     * the next attempt will find no value and rerun operation 'f' on 'k'
--
-- Properties of c_write which holds pair '(lo, hi)' are:
--
--     * 'newCacher' starts at '(0,0)'
--
--     * has '(lo <= hi)' until 'hi' exceeds Work64 size (2^64 - 1). I think
--       rollover will not break Cacher.
--       In particular 0 <= (hi-lo) <= high water mark.
--
--     * the number of (possibly failed) items in c_cache is '(hi - lo)'
--
--     * 'withCacher' may increment 'hi' by one,
-- and may increment 'lo' to implement the high/low watermark policy
--
--     * 'clearCacher' will reset to '(0,0)'
--
-- Properties of c_cache, with values '(index, ciRef)' :
--
--     * Each index value in at most one value in the c_cache
--
--     * For c_write holding '(lo, hi)', we have 'lo < index <= hi'.
--
--     * The ciRef holding CacheItem b has a usual lifecycle of
--
--         * ciRef starts with CI_Pending v, with empty MVar v
--
--         * ciRef set to CI_Ready b, holding the cached result
--
--         * putMVar v () executes, unblocking any waiting readers
--
--     * The ciRef holding CacheItem b has a failure lifecycle of
--
--         * ciRef starts with CI_Pending v, with empty MVar v
--
--         * ciRef set to CI_Failed, when an exception has happened
--
--         * putMVar v () executes, unblocking any waiting readers (and one will
--             try the operation again)
--
--     * Note: Under high contention on a key, it is possible two 'tryRead' see
--       the cache is empty for a key and try to fill it with 'tryWrite'
--
--         * Both will allocate a new ciRef  but only one will put it in the
--           c_cache, the other ciRef will be discarded
--
--         * This race to fill the key is the source of 'R_LostRace'
--           and 'R_WonRace'
--
--         * The 'R_LostRace' path leads to calling 'tryRead' to wait in the
--           winner.
--
-- Other notes:
--
-- * Pending operations count against the high/low watermark, and might lead to
-- running 'f' on 'k' more than once concurrently.  If this is a problem then
-- you need a different data structure, not a cache.
--
-- * If the operation on 'k' fails then this still counts against the high/low
-- watermark.  Retrying on 'k' re-uses the same 'index', so will not
-- continue to use up the allowed number of items.  But a high failure rate
-- on many different values will leave less room in the cache for
-- successful results.
--
-- * I originally wrote 'checkLimits' so as not to slow down
-- the 'withCacher', by using forkIO to do the work in the background.  This
-- was overkill, since the 'withCacher' has already potentially blocked
-- on c_write.
--
-- * The current 'checkLimits' does any work of reducing the cache
-- to low water mark while blocking in 'withCacher', so the c_cache never
-- exceeds the high water mark.  If filtering the map becomes a latency issue
-- then we can resurrect my original code (from earlier version of D13215939)
-- or some other optimization.
module Glean.Util.Cacher
  ( CacherConf(..), Cacher
  , newCacher, clearCacher, withCacher
  , checkCacher, insertCacher
  ) where

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.IORef
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Word ( Word64 )

-- | When the number of items would go over the high-water mark then the
-- number of items is reduced to at most the low-water mark.
--
-- The low water mark shoud not be greater than the high water mark.
--
-- A high water mark of 0 means nothing will get cached.
--
-- A low water mark of 0 means all items are deleted when the high water
-- mark is passed.
data CacherConf = CacherConf
  { cc_item_high_water_mark :: Word64
  , cc_item_low_water_mark :: Word64
  }

data CacheItem b = CI_Ready b
                 | CI_Pending (IO ()) -- ^ action to block until done
                 | CI_Failed

type CacheMap a b = Map a (Word64, IORef (CacheItem b))

-- | Opaque type holding the cache
data Cacher a b = Cacher
  { c_conf :: CacherConf
  , c_write :: MVar (Word64, Word64) -- ^ number of items is snd minus fst
  , c_cache :: IORef (CacheMap a b)
  }

data Res b = R_LostRace
           | R_WonRace b

io :: MonadIO m => IO a -> m a
io = liftIO

atomicRead :: MonadIO m => IORef x -> m x
atomicRead r = io (atomicModifyIORef r (\ old -> (old, old)))

atomicWrite :: MonadIO m => IORef x -> x -> m ()
atomicWrite r new = io (atomicModifyIORef' r (const (new, ())))

-- | When over high water mark items bring down to low water mark items.
-- These are Word64, so the subtraction should be mod 2^64, and rollover
-- is not a problem.
checkLimits
  :: CacherConf
  -> (CacheMap a b, (Word64, Word64))
  -> (CacheMap a b, (Word64, Word64))
checkLimits cc old@(oldCache, (loIn, hiIn))
  | hiIn - loIn <= highWaterMark = old
  | otherwise =
      let keep (n, _) = hiIn - n < lowWaterMark
          newCache = M.filter keep oldCache
          newLo = hiIn - lowWaterMark
      in (newCache, (newLo, hiIn))
  where
    highWaterMark = cc_item_high_water_mark cc
    lowWaterMark = cc_item_low_water_mark cc

-- | If you weirdly pass in a low water mark greater than the high water mark,
-- then the high is raised to meet low.
newCacher :: (Ord a, MonadIO m) => CacherConf -> m (Cacher a b)
newCacher ccIn = io (Cacher ccSane <$> newMVar (0,0)
                                   <*> newIORef mempty)
  where ccSane = ccIn{ cc_item_high_water_mark =
          max (cc_item_high_water_mark ccIn) (cc_item_low_water_mark ccIn) }

-- | Remove all items in the cache
clearCacher :: (Ord a, MonadIO m) => Cacher a b -> m ()
clearCacher c = io $ modifyMVar (c_write c) $ \ _ -> do
  atomicModifyIORef' (c_cache c) (const (mempty, ()))
  return ((0,0), ())

-- | Query if there is a CI_Ready value for this key. Only uses 'readIORef'
-- and does not block.
checkCacher :: (Ord a, MonadIO m) => Cacher a b -> a -> m (Maybe b)
checkCacher c a = io $ do
  m <- readIORef (c_cache c)
  case M.lookup a m of
    Nothing -> return Nothing
    Just (_, otherRef) -> do
      other <- readIORef otherRef
      case other of
        CI_Ready b -> return (Just b)
        CI_Pending{} -> return Nothing
        CI_Failed -> return Nothing

-- | Inserts the item only if the key is not already Ready or Pending.  This
-- will replace Failed.
insertCacher :: (Ord a, MonadIO m) => Cacher a b -> a -> b -> m ()
insertCacher c a b = io $
  modifyMVar_ (c_write c) $ \ oldLim@(oldLo, oldHi) -> do
    orig <- atomicRead (c_cache c)
    case M.lookup a orig of
      Nothing -> do
        ciRef <- newIORef (CI_Ready b)
        let newHi = succ oldHi
            proposeM = M.insert a (newHi, ciRef) orig
            proposeLim = (oldLo, newHi)
            (newM, newLim) = checkLimits (c_conf c) (proposeM, proposeLim)
        atomicWrite (c_cache c) newM
        return newLim
      Just (_otherIndex, otherRef) -> do
        other <- io $ readIORef otherRef
        case other of
          CI_Ready{} -> return oldLim
          CI_Pending{} -> return oldLim
          CI_Failed -> do
            atomicWrite otherRef (CI_Ready b)
            return oldLim

-- | 'withCacher c f x' is semantically equivalent to 'f x', but it will also
-- try to cache the result in 'c' so that future 'withCacher c f x'
-- calls return the result immediately.
--
-- During 'withCacher c op a', if there is an async exception or 'op' throws
-- an excecption then 'withCacher' throws the same exception and the cache
-- does not record the failure.  Thus calling 'withCacher c op a' after
-- an exception will retry the 'op'.
--
-- If 'withCacher c op a' runs 'op' then any concurrent calls with the same key
-- 'a' will block until 'op' finishes (or throws an exception).
--
-- An async exception thrown to 'withCacher' is safe, but whether the cache
-- is updated is not specified (because 'op' may succeed and store the result).
withCacher :: (Ord a, MonadIO m, MonadMask m)
           => Cacher a b -> (a -> m b) -> a -> m b
withCacher c op a = tryRead
  where
    tryRead = do
      orig <- io $ readIORef (c_cache c)
      case M.lookup a orig of
        Nothing -> tryWrite
        Just (_, ciRef) -> do
          ci <- io $ readIORef ciRef
          case ci of
            CI_Ready b -> return b               -- happy path for cached value
            CI_Pending block -> do
              io block                           -- block on pending calculation
              tryRead
            CI_Failed -> tryWrite

    tryWrite = do
      res <- safePendingMVar $ \ v ->
        safeCiRef v $ \ ciRef -> do
          r <- io $ modifyMVar (c_write c) $ \ oldLim@(oldLo, oldHi) -> do
            orig <- atomicRead (c_cache c)
            let lost = return (oldLim, R_LostRace)
                wonNew = do
                  let newHi = succ oldHi
                      proposeM = M.insert a (newHi, ciRef) orig
                      proposeLim = (oldLo, newHi)
                      (newM, newLim) = checkLimits (c_conf c)
                        (proposeM, proposeLim)
                  atomicWrite (c_cache c) newM
                  return (newLim, R_WonRace ())
                wonOther otherIndex = do
                  let newM = M.insert a (otherIndex, ciRef) orig
                  atomicWrite (c_cache c) newM
                  return (oldLim, R_WonRace ())
            case M.lookup a orig of       -- double check with c_write lock held
              Nothing -> wonNew                                    -- happy path
              Just (otherIndex, otherRef) -> do
                other <- io $ readIORef otherRef
                case other of
                  CI_Ready{} -> lost
                  CI_Pending{} -> lost
                  CI_Failed -> wonOther otherIndex          -- mostly happy path
          case r of
            R_LostRace -> return R_LostRace
            R_WonRace () -> do
              b <- op a                     -- 'op' runs outside of c_write lock
              atomicWrite ciRef (CI_Ready b)
              return (R_WonRace b)                                 -- happy path
      case res of
        R_LostRace -> tryRead            -- otherRef held CI_Ready or CI_Pending
        R_WonRace b -> return b                                    -- happy path

    safePendingMVar act = bracket (io newEmptyMVar)
      (\v -> io (putMVar v ())) act

    safeCiRef v act = bracket newCiRef (io . cleanup) act
      where
        newCiRef = io (newIORef (CI_Pending (readMVar v)))
        cleanup ciRef = atomicModifyIORef' ciRef $ \ old -> case old of
          CI_Ready{} -> (old, ())
          CI_Pending{} -> (CI_Failed, ())    -- one true source of CI_Failed
          CI_Failed -> (old, ()) -- impossible path
