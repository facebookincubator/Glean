-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.Database.Work.Heartbeat (
  Heartbeats,
  newHeartbeats,
  expectHeartbeat,
  deleteHeartbeat,
  checkHeartbeats
) where

import Control.Concurrent.STM
import Control.Monad
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import qualified Glean.Types as Thrift
import Glean.Util.Time

newtype Heartbeats = Heartbeats
  (TVar (HashMap Thrift.WorkHandle (Thrift.Work, TVar TimePoint)))

newHeartbeats :: IO Heartbeats
newHeartbeats = Heartbeats <$> newTVarIO mempty

expectHeartbeat
  :: Heartbeats -> Thrift.Work -> TimePoint -> STM (TVar TimePoint)
expectHeartbeat (Heartbeats hbs) work time = do
  xs <- readTVar hbs
  case HashMap.lookup (Thrift.work_handle work) xs of
    Just (_, v) -> do
      writeTVar v time
      return v
    Nothing -> do
      v <- newTVar time
      writeTVar hbs $ HashMap.insert (Thrift.work_handle work) (work, v) xs
      return v

deleteHeartbeat :: Heartbeats -> Thrift.Work -> STM ()
deleteHeartbeat (Heartbeats hbs) work = modifyTVar' hbs $
  HashMap.delete (Thrift.work_handle work)

checkHeartbeats :: Heartbeats -> TimePoint -> STM [Thrift.Work]
checkHeartbeats (Heartbeats hbs) now = do
  xs <- readTVar hbs
  ys <- forM (HashMap.elems xs) $ \(work,v) -> (,) work <$> readTVar v
  let expired = [work | (work, time) <- ys, time <= now]
  writeTVar hbs $ foldr (HashMap.delete . Thrift.work_handle) xs expired
  return expired
