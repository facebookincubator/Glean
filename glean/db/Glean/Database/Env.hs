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
import System.IO.Temp

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
import Glean.Database.Types
import Glean.Database.Work
import Glean.Database.Work.Heartbeat
import Glean.Database.Work.Queue (newWorkQueue)
import Glean.Database.Writes
import qualified Glean.Recipes.Types as Recipes
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Util.ConfigProvider
import Glean.Util.Disk
import Glean.Util.Observed as Observed
import Glean.Util.Periodic
import Glean.Util.ShardManager (SomeShardManager)
import Glean.Util.Some
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
    let
      withRoot Nothing io = withSystemTempDirectory "glean" $ \tmp -> do
        logInfo $ "Storing temporary DBs in " <> tmp
        io tmp
      withRoot (Just dir) io = io dir
    withRoot (cfgRoot cfg) $ \dbRoot -> do
      envCatalog <- do
        Some store <- cfgCatalogStore cfg dbRoot
        Catalog.open store
      cfgShardManager cfg envCatalog server_config $ \shardManager ->
        bracket
          (initEnv
            evb
            dbRoot
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
  :: EventBaseDataplane
  -> FilePath
  -> Catalog.Catalog
  -> SomeShardManager
  -> Config
  -> Observed SchemaIndex
  -> Observed Recipes.Config
  -> Observed ServerConfig.Config
  -> IO Env
initEnv evb dbRoot envCatalog shardManager cfg
  envSchemaSource envRecipeConfig envServerConfig = do
    server_cfg@ServerConfig.Config{..} <- Observed.get envServerConfig

    Some envStorage <- cfgStorage cfg dbRoot server_cfg
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
      , envRoot = dbRoot
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
spawnThreads env = do
  ServerConfig.Config{..} <- Observed.get $ envServerConfig env

  case config_janitor_period of
    Just secs -> Warden.spawn_ (envWarden env)
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
      atomically $ writeTVar (envDatabaseJanitor env) $ Just t

  Warden.spawn_ (envWarden env) $ backuper env

  Warden.spawn_ (envWarden env) $ reapHeartbeats env

  replicateM_ (fromIntegral config_db_writer_threads)
    $ Warden.spawn_ (envWarden env)
    $ writerThread
    $ envWriteQueues env

  when (envUpdateSchema env) $ do
    Warden.spawnDaemon (envWarden env) "schema updater" $ do
      void $ atomically $ takeTMVar (envSchemaUpdateSignal env)
      schemaUpdated env Nothing
    doOnUpdate (envSchemaSource env) $
      atomically $ void $ tryPutTMVar (envSchemaUpdateSignal env) ()

  -- Disk usage counters
  Warden.spawn_ (envWarden env) $ doPeriodically (seconds 600) $ do

    diskSize <- getDiskSize (envRoot env)
    used <- getUsedDiskSpace (envRoot env)
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
