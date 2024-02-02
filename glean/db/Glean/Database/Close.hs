{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Close (
  closeDatabase,
  closeDatabases,
  closeIdleDatabase,
  closeIdleDatabases,
  closeOpenDB,
) where

import Control.Exception hiding(handle)
import Control.Monad.Extra
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import qualified Data.Text.Encoding as Text

import ServiceData.GlobalStats as Stats
import Util.STM

import Glean.Database.Open
import qualified Glean.Database.Storage as Storage
import Glean.Database.Types
import Glean.Database.Writes
import qualified Glean.RTS.Foreign.LookupCache as LookupCache
import Glean.Types hiding (Database)
import qualified Glean.Types as Thrift
import Glean.Util.Mutex
import Glean.Util.Time


closeDatabases :: Env -> IO ()
closeDatabases env = do
  dbs <- readTVarIO $ envActive env
  forM_ (HashMap.keys dbs) $ closeDatabase env

isIdle :: (TimePoint -> Bool) -> DB -> OpenDB -> STM Bool
isIdle long_enough db odb = and <$> sequence
  [ (== 1) <$> readTVar (dbUsers db)  -- we are the only user
  , long_enough <$> readTVar (odbIdleSince odb)
  , case odbWriting odb of
      Just Writing{..} -> do
        writeQueueSize <- readTVar (writeQueueSize wrQueue)
        commit <- readTVar wrCommit
        return $ writeQueueSize == 0 && isNothing commit
      Nothing -> return True
  ]

closeIf :: (DB -> DBState -> STM (Maybe OpenDB)) -> Env -> Repo -> IO ()
closeIf should_close env repo = usingActiveDatabase env repo $ \r ->
  forM_ r $ \db -> mask_ $ do
    r <- atomically $ do
      state <- readTVar $ dbState db
      o <- should_close db state
      case o of
        Just odb -> do
          idle <- isIdle (const True) db odb
          when (not idle) retry
          writeTVar (dbState db) Closing
          return $ Just (db,odb)
        Nothing -> return Nothing

    forM_ r $ \(db, odb) ->
      -- the actual closing of the DB must be uninterruptible
      uninterruptibleMask_ (closeOpenDB env odb)
        `finally` atomically (writeTVar (dbState db) Closed)

closeDatabase :: Env -> Repo -> IO ()
closeDatabase env = closeIf
  (\_ state -> case state of
    Opening -> retry
    Open odb -> do
      deleteWriteQueues env odb
      case odbWriting odb of
        Just Writing{..} -> do
          r <- readTVar wrCommit
          when (isJust r) retry
        Nothing -> return ()
      return $ Just odb
    Closing -> retry
    Closed -> return Nothing)
  env

-- | Synchronously close a database if it has been idle for more than
-- `duration`.
closeIdleDatabase :: Env -> Repo -> DiffTimePoints -> IO ()
closeIdleDatabase env repo duration = do
  now <- getTimePoint
  closeIf
    (\db state -> case state of
        Opening -> return Nothing
        Open odb -> do
          idle <- isIdle
            (\last_use -> diffTimePoints last_use now >= duration)
            db
            odb
          return $ if idle then Just odb else Nothing
        Closing -> return Nothing
        Closed -> return Nothing)
    env
    repo

closeIdleDatabases :: Env -> DiffTimePoints -> [Repo] -> IO ()
closeIdleDatabases env duration blocklist = do
  dbs <- readTVarIO $ envActive env
  let notBlocklisted = filter (not . (`elem` blocklist)) (HashMap.keys dbs)
  forM_ notBlocklisted $ \repo -> closeIdleDatabase env repo duration
  exportOpenDBStats env

-- | set a counter glean.db.<repo>.open to the number of currently
-- open DBs for that particular repo name.
exportOpenDBStats :: Env -> IO ()
exportOpenDBStats env = do
  opens <- atomically $ do
    dbs <- readTVar $ envActive env
    forM (HashMap.toList dbs) $ \(repo, db) -> do
      state <- readTVar (dbState db)
      case state of
        Open{} -> return [(Thrift.repo_name repo, 1)]
        Opening{} -> return [(Thrift.repo_name repo, 1)]
        _ -> return []
  let repoOpenCounts = HashMap.fromListWith (+) (concat opens)
  forM_ (HashMap.toList repoOpenCounts) $ \(repoNm,count) -> do
    setCounter ("glean.db." <> Text.encodeUtf8 repoNm <> ".open") count

closeOpenDB :: Env -> OpenDB -> IO ()
closeOpenDB env OpenDB{..} = do
  case odbWriting of
    Just Writing{..} -> do
      -- free memory and update counters
      withMutex wrLock $ const $ LookupCache.clear wrLookupCache
      updateLookupCacheStats env
    Nothing -> return ()
  Storage.close odbHandle
