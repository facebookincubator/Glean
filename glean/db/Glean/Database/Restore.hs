{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Restore (
  restoreDatabase,
  restoreDatabaseFromSite,
  forRestoreSitesM, ifRestoreRepo,
) where

import Control.Concurrent.STM
import Control.Exception hiding(handle)
import Control.Monad (when)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import qualified Glean.Database.Backup.Backend as Backup
import qualified Glean.Database.Backup.Locator as Backup
import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Exception
import Glean.Database.Meta
import Glean.Database.Types
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Types hiding (Database)
import qualified Glean.Types as Thrift
import Glean.Util.Observed as Observed
import Control.Monad.IO.Class (MonadIO(..))


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
      mapM (\(prefix, site, _) -> inner prefix site) sites
    _ -> return [none]

ifRestoreRepo
  :: MonadIO m
  => Env
  -> Repo
  -> m ()
  -> m ()
ifRestoreRepo Env{..} repo inner = do
  let repoName = Thrift.repo_name repo
  ServerConfig.DatabaseRestorePolicy{..} <-
    liftIO $ ServerConfig.config_restore <$> Observed.get envServerConfig
  when ((repoName `Set.member` databaseRestorePolicy_override)
          /= databaseRestorePolicy_enabled)
    inner

restoreDatabase :: Env -> Text -> IO ()
restoreDatabase env loc
  | Just (prefix, site, repo) <-
      Backup.fromRepoLocator (envBackupBackends env) loc =
        atomically =<< restoreDatabaseFromSite env prefix site repo
  | otherwise = throwIO $
      Thrift.InvalidLocator $ "invalid locator '" <> loc <>  "'"

restoreDatabaseFromSite
  :: Backup.Site site
  => Env
  -> Text
  -> site
  -> Repo
  -> IO (STM ())
restoreDatabaseFromSite Env{..} prefix site repo = do
  props <- Backup.inspect site repo
  case metaFromProps loc props of
    Right meta -> return $ Catalog.startRestoring envCatalog repo meta
    Left err -> dbError repo $ concat
      ["invalid metadata in backup '", Text.unpack loc, "': ", err]
  where
    loc = Backup.toRepoLocator prefix site repo
