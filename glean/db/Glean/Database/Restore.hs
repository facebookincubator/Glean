{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Restore (
  restoreDatabase,
  restoreDatabaseFromSite,
  forRestoreSitesM, ifRestoreRepo, restorable,
) where

import Control.Exception hiding(handle)
import qualified Data.Set as Set
import Data.Text (Text)

import Util.STM

import qualified Glean.Database.Backup.Backend as Backup
import qualified Glean.Database.Backup.Locator as Backup
import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Types
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Types hiding (Database)
import qualified Glean.Types as Thrift
import Glean.Util.Observed as Observed


forRestoreSitesM
  :: Env
  -> a
  -> (forall site. Backup.Site site => Text -> site -> IO a)
  -> IO [a]
forRestoreSitesM env@Env{..} none inner = do
  ServerConfig.DatabaseRestorePolicy{..} <-
    ServerConfig.config_restore <$> Observed.get envServerConfig
  r <- atomically $ Backup.getAllSites env
  case r of
    sites@(_:_)
      | databaseRestorePolicy_enabled
        || not (Set.null databaseRestorePolicy_override) ->
      mapM (uncurry inner) sites
    _ -> return [none]

ifRestoreRepo
  :: Env
  -> a
  -> Repo
  -> IO a
  -> IO a
ifRestoreRepo Env{..} none repo inner = do
  policy <- ServerConfig.config_restore <$> Observed.get envServerConfig
  if restorable policy repo
    then inner
    else return none

restorable :: ServerConfig.DatabaseRestorePolicy -> Repo -> Bool
restorable ServerConfig.DatabaseRestorePolicy{..} repo =
  (repoName `Set.member` databaseRestorePolicy_override)
     /= databaseRestorePolicy_enabled
  where repoName = Thrift.repo_name repo

restoreDatabase :: Env -> Text -> IO ()
restoreDatabase env loc
  | Just (_, site, repo) <-
      Backup.fromRepoLocator (envBackupBackends env) loc =
        atomically =<< restoreDatabaseFromSite env site repo
  | otherwise = throwIO $
      Thrift.InvalidLocator $ "invalid locator '" <> loc <>  "'"

restoreDatabaseFromSite
  :: Backup.Site site
  => Env
  -> site
  -> Repo
  -> IO (STM ())
restoreDatabaseFromSite Env{..} site repo = do
  props <- Backup.inspect site repo
  return $ Catalog.startRestoring envCatalog repo props
