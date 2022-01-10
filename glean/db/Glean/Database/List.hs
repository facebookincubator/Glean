{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.List (
  listDatabases,
  listRestorable,
) where

import Control.Concurrent.STM
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Text (Text)

import Util.Control.Exception
import Util.Log

import qualified Glean.Database.Backup.Backend as Backup
import qualified Glean.Database.Backup.Locator as Backup
import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Meta
import Glean.Database.Types
import Glean.Types hiding (Database)
import qualified Glean.Types as Thrift


listDatabases :: Env -> Thrift.ListDatabases -> IO Thrift.ListDatabasesResult
listDatabases env@Env{..} Thrift.ListDatabases{..} = do
  backups <-
    if listDatabases_includeBackups
      then do
        sites <- atomically $ Backup.getAllSites env
        restorables <- mapM
          (\(prefix, site, _) -> listRestorable prefix site) sites
        return $ reposToResults $ HashMap.unions restorables
      else
        return mempty
  local <- atomically $ Catalog.getLocalDatabases envCatalog
  return Thrift.ListDatabasesResult
    { listDatabasesResult_databases =
        map Thrift.getDatabaseResult_database
        $ HashMap.elems
        $ HashMap.union local backups
    }
  where
    reposToResults = HashMap.mapWithKey
      (\repo meta -> Thrift.GetDatabaseResult
        { getDatabaseResult_database = metaToThriftDatabase
            Thrift.DatabaseStatus_Restorable
            Nothing
            repo
            meta
        , getDatabaseResult_tasks = Nothing
        })

listRestorable :: Backup.Site site => Text -> site -> IO (HashMap Repo Meta)
listRestorable prefix site =
  (HashMap.fromList . mapMaybe restorable <$> Backup.enumerate site)
  `catchAll` \exc -> do
    logError $ "couldn't list restorable databases: " ++ show exc
    return mempty
  where
    restorable (repo, props)
      | Right meta <-
          metaFromProps (Backup.toRepoLocator prefix site repo) props =
            Just (repo, meta)
      | otherwise = Nothing
