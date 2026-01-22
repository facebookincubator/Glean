{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Test
  ( Setting
  , setRoot
  , setSchemaLocation
  , setSchemaIndex
  , setSchemaPath
  , setSchemaId
  , disableStrictSchemaId
  , setMemoryStorage
  , setLMDBStorage
  , allStorage
  , setDBVersion
  , setCompactOnCompletion
  , setMaxSetSize
  , enableTcDebug
  , enableQueryDebug
  , withTestEnv
  , kickOffTestDB
  , waitUntilComplete
  , completeTestDB
  , withEmptyTestDB
  , writeFactsIntoDB
  ) where

import Util.STM
import Data.Default
import Data.Functor
import Data.Int
import Data.List (foldl')
import qualified Data.Text as Text

import Util.EventBase

import Glean.Backend.Local ( {- instance Backend Env -} )
import Glean.Backend.Types
import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Config
import Glean.Database.Env
import Glean.Database.Write.Batch
import Glean.Database.Storage (DBVersion(..), currentVersion, writableVersions)
import Glean.Database.Types
import qualified Glean.Internal.Types as Thrift
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Typed
import qualified Glean.Types as Thrift
import Glean.Util.ConfigProvider
import qualified Glean.Util.ThriftSource as ThriftSource

type Setting = Config -> Config

setRoot :: FilePath -> Setting
setRoot path cfg = cfg{ cfgDataStore = fileDataStore path }

setSchemaLocation :: SchemaLocation -> Setting
setSchemaLocation loc cfg = cfg{ cfgSchemaLocation = Just loc }

setSchemaPath :: FilePath -> Setting
setSchemaPath = setSchemaLocation . SchemaLocation_file . Text.pack

setSchemaIndex :: FilePath -> Setting
setSchemaIndex = setSchemaLocation . SchemaLocation_index . Text.pack

-- | Set the schema that will be used for queries, otherwise defaults
-- to the highest all.N in the latest schema.
setSchemaId :: Thrift.SchemaId -> Setting
setSchemaId id cfg = cfg { cfgSchemaId = Just id }

disableStrictSchemaId :: Setting
disableStrictSchemaId cfg = cfg {
  cfgServerConfig = cfgServerConfig cfg <&> \scfg -> scfg
    { ServerConfig.config_strict_query_schema_id = False }
  }

setMemoryStorage :: Setting
setMemoryStorage cfg = cfg{ cfgDataStore = memoryDataStore }

setLMDBStorage :: Setting
setLMDBStorage cfg =
  cfg{ cfgDataStore = tmpDataStore { defaultStorage = lmdbName }}

allStorage :: [(String, [Setting])]
allStorage =
  [
    ("rocksdb", []),
    ("lmdb", [setLMDBStorage]),
    ("memory", [setMemoryStorage])
  ]
  ++
  [ ("rocksdb-" ++ show (unDBVersion v), [setDBVersion v])
    | v <- writableVersions, v /= currentVersion ]

setDBVersion :: ServerConfig.DBVersion -> Setting
setDBVersion ver cfg = cfg
  { cfgServerConfig = cfgServerConfig cfg <&> \scfg -> scfg
      { ServerConfig.config_db_create_version = Just ver } }

setCompactOnCompletion :: Setting
setCompactOnCompletion cfg = cfg
  { cfgServerConfig = cfgServerConfig cfg <&> \scfg -> scfg
      { ServerConfig.config_compact_on_completion = True } }

setMaxSetSize :: Int64 -> Setting
setMaxSetSize i cfg = cfg
  { cfgServerConfig = cfgServerConfig cfg <&> \scfg -> scfg
      { ServerConfig.config_max_set_size_bytes = Just i } }

enableTcDebug :: Setting
enableTcDebug cfg = cfg
  { cfgDebug = (cfgDebug cfg) { tcDebug = True } }

enableQueryDebug :: Setting
enableQueryDebug cfg = cfg
  { cfgDebug = (cfgDebug cfg) { queryDebug = True } }

withTestEnv
  :: [Setting]
  -> (Env -> IO a)
  -> IO a
withTestEnv settings action =
  withEventBaseDataplane $ \evb ->
  withConfigProvider defaultConfigOptions $
      \(cfgAPI :: NullConfigProvider) -> do
    let
      dbConfig = foldl' (\acc f -> f acc)
        def
          { cfgDataStore = tmpDataStore
          , cfgSchemaLocation = Just schemaLocationFiles
          , cfgServerConfig = ThriftSource.value def
              { ServerConfig.config_db_rocksdb_cache_mb = 0 }
          }
        settings

    withDatabases evb dbConfig cfgAPI action

kickOffTestDB
  :: Env -> Thrift.Repo -> (Thrift.KickOff -> Thrift.KickOff) -> IO ()
kickOffTestDB env repo update = do
  void $ kickOffDatabase env $ update def
    { Thrift.kickOff_repo = repo
    }

writeFactsIntoDB
  :: Env
  -> Thrift.Repo
  -> [SchemaPredicates]
  -> (forall m. NewFact m => m ())
  -> IO ()
writeFactsIntoDB env repo allPredicates facts = do
  !predicates <- loadPredicates env repo allPredicates
  batch <- buildBatch predicates Nothing facts
  void $ syncWriteDatabase env repo batch

waitUntilComplete :: Env -> Thrift.Repo -> IO ()
waitUntilComplete Env{..} repo = atomically $ do
  meta <- Catalog.readMeta envCatalog repo
  case Thrift.metaCompleteness meta of
    Thrift.Complete{} -> return ()
    Thrift.Broken{} -> errorWithoutStackTrace "database failed unexpectedly"
    _ -> retry

completeTestDB :: Env -> Thrift.Repo -> IO ()
completeTestDB env repo = do
  void $ finishDatabase env repo
  waitUntilComplete env repo

withEmptyTestDB
  :: [Setting]
  -> (Env -> Thrift.Repo -> IO a) -- ^ action
  -> IO a
withEmptyTestDB settings action = withTestEnv settings $ \env -> do
  kickOffTestDB env repo id
  action env repo
  where
    repo = Thrift.Repo "dbtest-repo" "f00baa"
