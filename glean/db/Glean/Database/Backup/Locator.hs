{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Backup.Locator
  ( fromSiteLocator
  , fromRepoLocator
  , toSiteLocator
  , toRepoLocator
  , getSite
  , getAllSites
  ) where

import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text

import Util.List
import Util.STM

import Glean.Database.Backup.Backend as Backend
import Glean.Database.Types
import Glean.Repo.Text
import Glean.ServerConfig.Types (DatabaseBackupPolicy(..))
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Types
import Glean.Util.Some
import Glean.Util.Observed as Observed

fromSiteLocator :: Backends -> Text -> Maybe (Text, Some Site)
fromSiteLocator backends s = do
  let (prefix,colon_path) = Text.breakOn ":" s
  Some backend <- HashMap.lookup prefix backends
  (_,path) <- Text.uncons colon_path
  site <- fromPath backend path
  return (prefix, site)

-- A repo locator has the following form:
--  backend_name:some/path/repo.name.hash
fromRepoLocator :: Backends -> Text -> Maybe (Text, Some Site, Repo)
fromRepoLocator backends loc = do
  let (site_slash, repo_text) = Text.breakOnEnd "/" loc
  (site_text, _) <- Text.unsnoc site_slash
  (prefix, site) <- fromSiteLocator backends site_text
  repo <- parseRepoTextSep "." repo_text
  return (prefix, site, repo)

toSiteLocator :: Site site => Text -> site -> Text
toSiteLocator prefix site = prefix <> ":" <> toPath site

toRepoLocator :: Site site => Text -> site -> Repo -> Text
toRepoLocator prefix site repo =
  toSiteLocator prefix site <> "/" <> repoToTextSep "." repo

getSite :: Env -> Text -> STM (Maybe (Text, Some Site))
getSite Env{..} repoName = do
  policy <- ServerConfig.config_backup <$> Observed.get envServerConfig
  let locator = Map.findWithDefault
        (databaseBackupPolicy_location policy)
        repoName
        (Map.map ServerConfig.backup_location $
          databaseBackupPolicy_repos policy)
  return $ fromSiteLocator envBackupBackends locator

getAllSites :: Env -> STM [(Text, Some Site)]
getAllSites Env{..} = do
  policy <- ServerConfig.config_backup <$> Observed.get envServerConfig
  return $
    mapMaybe (fromSiteLocator envBackupBackends) $
    uniq $ -- don't forget to remove duplicates
      databaseBackupPolicy_location policy :
      map ServerConfig.backup_location
        (Map.elems $ databaseBackupPolicy_repos policy)
