{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE CPP #-}
module Glean.Database.Env ( withDatabases ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Extra
import Data.Default
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Time
import System.Clock (TimeSpec(..))
import System.Timeout

import Data.RateLimiterMap
import ServiceData.GlobalStats
import Util.EventBase
import Util.Log

import qualified Glean.RTS.Foreign.LookupCache as LookupCache
import Glean.Database.Backup (backuper)
import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Config
import Glean.Database.Close
import Glean.Database.Janitor
import qualified Glean.Database.Stats as Stats
import Glean.Database.Open
import qualified Glean.Database.Storage as Storage
import Glean.Database.Types
import Glean.Database.Work
import Glean.Database.Work.Heartbeat
import Glean.Database.Work.Queue (newWorkQueue)
import Glean.Database.Writes
import qualified Glean.Recipes.Types as Recipes
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Util.ConfigProvider
import Glean.Util.Observed as Observed
import Glean.Util.Periodic
import Glean.Util.ShardManager (SomeShardManager)
import Glean.Util.ThriftSource as ThriftSource
import Glean.Util.Time
import qualified Glean.Util.Warden as Warden

-- | Initialize an Env for working with Glean databases
withDatabases
  :: ConfigProvider conf
  => EventBaseDataplane
  -> Config
  -> conf
  -> (Env -> IO a)
  -> IO a
withDatabases evb cfg cfgapi act =
  ThriftSource.withValue cfgapi (cfgSchemaSource cfg) $ \schema_source ->
  ThriftSource.withValue
    cfgapi
    (if cfgReadOnly cfg then ThriftSource.value def else cfgRecipeConfig cfg)
    $ \recipe_config ->
  ThriftSource.withValue cfgapi (cfgServerConfig cfg) $ \server_config -> do
  server_cfg <- Observed.get server_config
  dataStoreCreate (cfgDataStore cfg) server_cfg $ \catalog storage -> do
    envCatalog <- Catalog.open catalog
    cfgShardManager cfg envCatalog server_config $ \shardManager ->
      bracket
        (initEnv
          evb
          storage
          envCatalog
          shardManager
          cfg
          schema_source
          recipe_config
          server_config)
        closeEnv
        $ \env -> do
            resumeWork env
            spawnThreads env
            act env

initEnv
  :: Storage.Storage storage
  => EventBaseDataplane
  -> storage
  -> Catalog.Catalog
  -> SomeShardManager
  -> Config
  -> Observed SchemaIndex
  -> Observed Recipes.Config
  -> Observed ServerConfig.Config
  -> IO Env
initEnv evb envStorage envCatalog shardManager cfg
  envSchemaSource envRecipeConfig envServerConfig = do
    ServerConfig.Config{..} <- Observed.get envServerConfig

    envActive <- newTVarIO mempty
    envDeleting <- newTVarIO mempty
    envStats <- Stats.new (TimeSpec 10 0)
    envLookupCacheStats <- LookupCache.newStats
    envWarden <- Warden.create
    envDatabaseJanitor <- newTVarIO Nothing
    envDatabaseJanitorPublishedCounters <- newTVarIO mempty
    envCachedRestorableDBs <- newTVarIO Nothing

    envLoggerRateLimit <-
      newRateLimiterMap (fromIntegral config_logging_rate_limit) 600

    envWorkQueue <- newWorkQueue
    envHeartbeats <- newHeartbeats

    envWrites <- newTVarIO HashMap.empty
    envDerivations <- newTVarIO HashMap.empty
    envWriteQueues <- WriteQueues
      <$> newTQueueIO
      <*> newTVarIO 0

    envTailers <- newTVarIO HashMap.empty

    envSchemaUpdateSignal <- atomically newEmptyTMVar

    envCompleting <- newTVarIO HashMap.empty

    return Env
      { envEventBase = evb
      , envServerLogger = cfgServerLogger cfg
      , envDatabaseLogger = cfgDatabaseLogger cfg
      , envReadOnly = cfgReadOnly cfg
      , envMockWrites = cfgMockWrites cfg
      , envTailerOpts = cfgTailerOpts cfg
      , envListener = cfgListener cfg
      , envGetCreationTime = getCurrentTime
      , envSchemaVersion = cfgSchemaVersion cfg
      , envUpdateSchema = cfgUpdateSchema cfg
      , envSchemaId = cfgSchemaId cfg
      , envShardManager = shardManager
      , envBackupBackends = cfgBackupBackends cfg
      , .. }

spawnThreads :: Env -> IO ()
spawnThreads env@Env{..} = do
  ServerConfig.Config{..} <- Observed.get envServerConfig

  case config_janitor_period of
    Just secs -> Warden.spawn_ envWarden
      $ doPeriodically (seconds (fromIntegral secs))
        -- a conservative timeout in case the janitor deadlocks for
        -- some reason.
      $ withTimeout (fromIntegral secs * 20)
      $ runDatabaseJanitor env
      where
        withTimeout t act = do
          r <- timeout (t * 1000*1000) act
          when (isNothing r) $ logError "janitor timeout"
    Nothing -> do
      t <- getCurrentTime
      atomically $ writeTVar envDatabaseJanitor $ Just t

  Warden.spawn_ envWarden $ backuper env

  Warden.spawn_ envWarden $ reapHeartbeats env

  replicateM_ (fromIntegral config_db_writer_threads)
    $ Warden.spawn_ envWarden
    $ writerThread envWriteQueues

  when envUpdateSchema $ do
    Warden.spawnDaemon envWarden "schema updater" $ do
      void $ atomically $ takeTMVar envSchemaUpdateSignal
      schemaUpdated env Nothing
    doOnUpdate envSchemaSource $
      atomically $ void $ tryPutTMVar envSchemaUpdateSignal ()

  -- Disk usage counters
  Warden.spawn_ envWarden $ doPeriodically (seconds 600) $ do

    diskSize <- Storage.getTotalCapacity envStorage
    used <- Storage.getUsedCapacity envStorage
    void $ setCounter "glean.db.disk.capacity_bytes" diskSize
    void $ setCounter "glean.db.disk.used_bytes" used
    void $
      setCounter "glean.db.disk.used_percentage" (100 * used `div` diskSize)

-- Todo: this needs a lot more work.
-- * We shouldn't just cancel the janitor, we should let it finish the
--   current job if there is one.
-- * We should wait for in-progress backups or restores (or cancel them safely)
closeEnv :: Env -> IO ()
closeEnv env@Env{..} = do
  closeDatabases env
  Warden.shutdown envWarden
  Catalog.close envCatalog
