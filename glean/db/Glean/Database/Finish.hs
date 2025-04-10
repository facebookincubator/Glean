{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Finish
  ( workFinished
  , unfinishDatabase
  , finalizeDatabase
  , finalizeWait
  ) where

import Control.Monad.Catch
import Control.Monad
import Util.STM
import Data.Default
import qualified Data.HashMap.Strict as HashMap

import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Meta
import Glean.Types as Thrift
import Glean.Database.Types
import Glean.Internal.Types as Thrift

import qualified Data.HashSet as HashSet
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Util.Observed as Observed

-- | A worker has finished work on a particular parcel with the given
-- 'Outcome'.
workFinished :: Env -> Thrift.WorkFinished -> IO ()
workFinished Env{..} Thrift.WorkFinished{..} = do
  atomically $ do
    let repo = Thrift.work_repo workFinished_work
    let task = Thrift.work_task workFinished_work
    case workFinished_outcome of
      Thrift.Outcome_success{} ->
        void $ Catalog.modifyMeta envCatalog repo $ \oldmeta ->
          return oldmeta { Thrift.metaCompleteness = Thrift.Finalizing def }
      Thrift.Outcome_failure (Thrift.Failure reason) ->
        void $ Catalog.modifyMeta envCatalog repo $ \oldmeta ->
          return oldmeta { Thrift.metaCompleteness =
            Thrift.Broken (DatabaseBroken task reason) }
      Thrift.Outcome_EMPTY{} ->
        void $ Catalog.modifyMeta envCatalog repo $ \oldmeta ->
          return oldmeta { Thrift.metaCompleteness =
            Thrift.Broken (DatabaseBroken task "invalid outcome") }
    makeReadOnly repo
  where
    -- When a DB is complete, make it read-only to prevent further
    -- writes. It is an error to call workFinished on the final task
    -- if there are outstanding writes in the queue.
    makeReadOnly repo = do
      mdb <- HashMap.lookup repo <$> readTVar envActive
      forM_ mdb $ \db -> do
        st <- readTVar (dbState db)
        case st of
          Open odb@OpenDB { odbWriting = Just Writing{..} } -> do
            -- NB. check the active counter as well as the queue,
            -- because this will tell us if there are writes currently
            -- in progress.
            active <- readTVar (writeQueueActive wrQueue)
            empty <- isEmptyTQueue (writeQueue wrQueue)
            -- If there are outstanding writes then the client is
            -- either broken or is intentionally trying to complete
            -- the DB early. But we can't complete the DB with
            -- outstanding writes, so we'll ask the client to retry
            -- the request later.
            when (active /= 0 || not empty) $
              throwM $ Exception
                "workFinished called but there are queued writes"
            writeTVar (dbState db) $ Open odb { odbWriting = Nothing }
          _ -> return ()

-- | Change a database's state from Complete to Incomplete.
-- WARNING! This is for testing only, and should
-- never be used on a production database.
unfinishDatabase :: Env -> Repo -> IO ()
unfinishDatabase Env{..} repo  = do
  backupPolicy <- ServerConfig.config_backup <$> Observed.get envServerConfig
  let isBackupAllowed = repo_name repo `HashSet.member`
        ServerConfig.databaseBackupPolicy_allowed backupPolicy
  if isBackupAllowed
    then do
      throwM $ Thrift.Exception
        "The backup is enabled for this Repo so we cannot unfinish it"
    else atomically $ do
      void $ Catalog.modifyMeta envCatalog repo $ \oldmeta ->
        case Thrift.metaCompleteness oldmeta of
          Thrift.Incomplete{} -> return oldmeta
          Thrift.Complete{} -> return oldmeta
            { Thrift.metaCompleteness = Thrift.Incomplete def }
          Thrift.Finalizing{} -> return oldmeta
            { Thrift.metaCompleteness = Thrift.Incomplete def }
          someState -> throwM $ Thrift.Exception
            (  "Cannot unfinish a database in state: "
            <> showCompleteness someState)

-- | Poll for finalization of a database
--   Throws Exception if the database is broken or incomplete.
finalizeDatabase :: Env -> Repo -> IO FinalizeResponse
finalizeDatabase env repo = do
  atomically $ do
    meta <- Catalog.readMeta (envCatalog env) repo
    case metaCompleteness meta of
      Finalizing{} -> throwM $ Retry 1.0
      Incomplete{} -> throwM $ Exception "incomplete database"
      Broken b -> throwM $ Exception $ databaseBroken_reason b
      _ -> return ()
  return FinalizeResponse{}

-- | Wait for finalization of a database. Use for local DBs only.
--   Throws Exception if the database is broken or incomplete.
finalizeWait :: Env -> Repo -> IO ()
finalizeWait env repo =
  atomically $ do
    meta <- Catalog.readMeta (envCatalog env) repo
    case metaCompleteness meta of
      Finalizing{} -> retry
      Incomplete{} -> throwM $ Exception "incomplete database"
      Broken b -> throwM $ Exception $ databaseBroken_reason b
      _ -> return ()
