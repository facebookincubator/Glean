-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.Database.Restore (
  restoreDatabase,
  restoreDatabaseFromSite,
  forRestoreSitesM, ifRestoreRepo,
) where

import Control.Concurrent.STM
import Control.Exception hiding(handle)
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
  :: Env
  -> a
  -> Repo
  -> (forall site. Backup.Site site => Text -> site -> IO a)
  -> IO a
ifRestoreRepo env@Env{..} none repo inner = do
  let repoName = Thrift.repo_name repo
  ServerConfig.DatabaseRestorePolicy{..} <-
    ServerConfig.config_restore <$> Observed.get envServerConfig
  r <- atomically $ Backup.getSite env repoName
  case r of
    Just (prefix, site, _)
      | (repoName `Set.member` databaseRestorePolicy_override)
          /= databaseRestorePolicy_enabled -> inner prefix site
    _ -> return none

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
