{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Test
  ( Setting
  , setRoot
  , setRecipes
  , setSchemaSource
  , setSchemaIndex
  , setSchemaPath
  , setSchemaId
  , disableStrictSchemaId
  , setMemoryStorage
  , setDBVersion
  , setCompactOnCompletion
  , withTestEnv
  , kickOffTestDB
  , waitUntilComplete
  , completeTestDB
  , withEmptyTestDB
  , writeFactsIntoDB
  ) where

import Control.Concurrent.STM
import Data.Default
import Data.Functor
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import Util.EventBase

import Glean.Backend.Local ( {- instance Backend Env -} )
import Glean.Backend.Types
import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Config
import Glean.Database.Env
import Glean.Database.Write.Batch
import Glean.Database.Types
import qualified Glean.Internal.Types as Thrift
import Glean.Recipes.Types (Recipes)
import qualified Glean.Recipes.Types as Recipes
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Typed
import qualified Glean.Types as Thrift
import Glean.Util.ConfigProvider
import Glean.Util.Observed as Observed
import qualified Glean.Util.ThriftSource as ThriftSource
import Glean.Util.ThriftSource (ThriftSource)

type Setting = Config -> Config

setRoot :: FilePath -> Setting
setRoot path cfg = cfg{ cfgDataStore = fileDataStore path }

setRecipes :: Map Text Recipes -> Setting
setRecipes recipes cfg = cfg
  { cfgRecipeConfig = ThriftSource.value Recipes.Config
        { config_recipes = recipes }
  }

setSchemaSource :: ThriftSource SchemaIndex -> Setting
setSchemaSource source cfg = cfg{ cfgSchemaSource = source }

setSchemaPath :: FilePath -> Setting
setSchemaPath = setSchemaSource . schemaSourceFile

setSchemaIndex :: FilePath -> Setting
setSchemaIndex = setSchemaSource . schemaSourceIndexFile

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

setDBVersion :: ServerConfig.DBVersion -> Setting
setDBVersion ver cfg = cfg
  { cfgServerConfig = cfgServerConfig cfg <&> \scfg -> scfg
      { ServerConfig.config_db_create_version = Just ver } }

setCompactOnCompletion :: Setting
setCompactOnCompletion cfg = cfg
  { cfgServerConfig = cfgServerConfig cfg <&> \scfg -> scfg
      { ServerConfig.config_compact_on_completion = True } }

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
          , cfgRecipeConfig = ThriftSource.value Recipes.Config
              { config_recipes = mempty }
          , cfgSchemaSource = schemaSourceFiles
          , cfgServerConfig = ThriftSource.value def
              { ServerConfig.config_db_rocksdb_cache_mb = 0 }
          }
        settings

    withDatabases evb dbConfig cfgAPI action

kickOffTestDB
  :: Env -> Thrift.Repo -> (Thrift.KickOff -> Thrift.KickOff) -> IO ()
kickOffTestDB env repo update = do
  recipes <- Observed.get $ envRecipeConfig env
  void $ kickOffDatabase env $ update def
    { Thrift.kickOff_repo = repo
    , Thrift.kickOff_fill = Just $
        if Thrift.repo_name repo `Map.member` Recipes.config_recipes recipes
          then Thrift.KickOffFill_recipes $ Thrift.repo_name repo
          else Thrift.KickOffFill_writeHandle ""
    }

writeFactsIntoDB
  :: Env
  -> Thrift.Repo
  -> [SchemaPredicates]
  -> (forall m. NewFact m => m ())
  -> IO ()
writeFactsIntoDB env repo allPredicates facts = do
  predicates <- loadPredicates env repo allPredicates
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
  workFinished env $ Thrift.WorkFinished
    { workFinished_work =
        def { Thrift.work_repo = repo, Thrift.work_handle = "" }
    , workFinished_outcome = Thrift.Outcome_success def
    }
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
