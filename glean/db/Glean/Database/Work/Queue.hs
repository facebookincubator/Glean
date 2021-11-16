{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Work.Queue (
  Parcel(..), WorkQueue,
  newWorkQueue, insertWorkQueue, writeWorkQueue, readWorkQueue
) where

import qualified Glean.Types as Thrift

import Control.Applicative
import Control.Concurrent.STM
import Control.Exception (assert)
import Data.Foldable (asum)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Text.Regex.Base as RE
import qualified Text.Regex.PCRE.ByteString as RE

-- | An available work parcel
data Parcel = Parcel
  { parcelRepo :: Thrift.Repo
  , parcelTask :: Text
  , parcelIndex :: Int
  }

-- | A "queue" of work parcels. It is never empty (i.e., getting a parcel
-- always succeeds).
data ParcelQueue
  = ParcelRange Int Int  -- ^ no parcels assigned yet, grab first one
  | ParcelQueue (IntMap Int)  -- ^ set of parcel ranges (start -> size)

-- | Create a queue for a range of work parcels.
newParcelQueue
  :: Int -- ^ index of first parcel
  -> Int -- ^ number of parcels (>0)
  -> ParcelQueue
newParcelQueue i n = assert (n>0) $ ParcelRange i n

-- | Get the next parcel from the queue as well as the (non-empty) rest of the
-- queue.
readParcelQueue :: ParcelQueue -> (Int, Maybe ParcelQueue)
readParcelQueue (ParcelRange i n) =
  -- No parcels assigned yet, just grab the first one.
  ( i
  , if n > 1
      then Just $ ParcelQueue $ IntMap.singleton (i+1) (n-1)
      else Nothing
  )
readParcelQueue (ParcelQueue ranges) =
    -- Find the largest consecutive range, split it in half and grab the first
    -- parcel in the *second* half - the assumption is that another worker is
    -- processing the first half.
    --
    -- NOTE: This is O(number of ranges) but we expect that to be small, at
    -- least for now.
    let (i,n) = maximumBy (comparing snd) $ IntMap.toList ranges
        !k = i + n `div` 2
    in
    ( k
    , let rs = foldr
            (uncurry IntMap.insert)
            (IntMap.delete i ranges)
            (filter ((>0) . snd) [(i,k-i),(k+1, i+n-(k+1))])
      in
      if IntMap.null rs
        then Nothing
        else Just $ ParcelQueue rs
    )

-- | Get the next parcel from the queue, preferring parcels close to the given
-- one. Also return the (non-empty) rest of the queue.
readParcelQueueNear
  :: Int   -- ^ index of parcel to stay close to
  -> ParcelQueue  -- ^ queue
  -> (Int, Maybe ParcelQueue)
readParcelQueueNear i (ParcelQueue ranges)
  -- We only check if the next parcel is available and fall back to the default
  -- algorithm otherwise. We could try to be cleverer here but it's unclear if
  -- it would gain us much.
  | (Just n, ranges0) <-
      IntMap.updateLookupWithKey (\_ _ -> Nothing) (i+1) ranges =
      ( i+1
      , if n > 1
          then Just $ ParcelQueue $ IntMap.insert (i+2) (n-1) ranges0
          else if IntMap.null ranges0
            then Nothing
            else Just $ ParcelQueue ranges0
      )
readParcelQueueNear _ pq = readParcelQueue pq

data RepoTask = RepoTask
  { rtRepo :: Thrift.Repo
  , rtParcels :: ParcelQueue
  }

-- | A work queue which can be queried by task.
newtype WorkQueue = WorkQueue
  { wqQueue :: TVar (HashMap Text (TQueue RepoTask)) }

-- | Create an empty work queue.
newWorkQueue :: IO WorkQueue
newWorkQueue = WorkQueue <$> newTVarIO mempty

-- Get the TQueue for a particular task, creating a new one if necessary.
taskQueue :: WorkQueue -> Text -> STM (TQueue RepoTask)
taskQueue WorkQueue{..} name = do
  ts <- readTVar wqQueue
  case HashMap.lookup name ts of
    Just tq -> return tq
    Nothing -> do
      tq <- newTQueue
      writeTVar wqQueue $ HashMap.insert name tq ts
      return tq

-- | Add `n` work parcels for a specific task into the queue.
insertWorkQueue
  :: WorkQueue      -- ^ queue
  -> Thrift.Repo    -- ^ repo
  -> Text           -- ^ task name
  -> Int            -- ^ number of parcels
  -> STM ()
insertWorkQueue wq repo name n = do
  tasks <- taskQueue wq name
  writeTQueue tasks $ RepoTask repo $ newParcelQueue 0 n

-- | Add a parcel to the queue.
writeWorkQueue :: WorkQueue -> Parcel -> STM ()
writeWorkQueue wq Parcel{..} = do
  -- NOTE: We don't try to join this with existing parcels for the same
  -- repo/task because presumably, this is the parcel that's getting retried
  -- and we want to prioritise it to fail fast. Also, it's not worth the effort.
  tasks <- taskQueue wq parcelTask
  unGetTQueue tasks $ RepoTask parcelRepo $ newParcelQueue parcelIndex 1

-- | Given a list of acceptable tasks, get the next available work parcel for
-- one of them.
readWorkQueue :: WorkQueue -> [Text] -> Maybe Parcel -> STM Parcel
readWorkQueue queue xs near = do
  tasks <- readTVar $ wqQueue queue
  read_near tasks near <|> read_any tasks
  where
    read_near tasks (Just Parcel{..})
      | Just q <- HashMap.lookup parcelTask tasks = do
          t@RepoTask{..} <- readTQueue q
          if rtRepo == parcelRepo
            then get_parcel (readParcelQueueNear parcelIndex) q parcelTask t
            else retry
    read_near _ _ = retry

    read_any tasks = asum
      [ get_parcel readParcelQueue q t =<< readTQueue q
        | pat <- xs
        , let re = RE.makeRegex (Text.encodeUtf8 pat) :: RE.Regex
        , (t,q) <- HashMap.toList tasks
        , RE.matchTest re (Text.encodeUtf8 t) ]

    get_parcel f q task RepoTask{..} = do
      let (i,r) = f rtParcels
      case r of
        Just pq -> unGetTQueue q RepoTask
          { rtRepo = rtRepo
          , rtParcels = pq
          }
        Nothing -> return ()
      return Parcel
        { parcelRepo = rtRepo
        , parcelTask = task
        , parcelIndex = i
        }
