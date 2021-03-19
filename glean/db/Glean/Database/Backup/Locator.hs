module Glean.Database.Backup.Locator
  ( fromSiteLocator
  , fromRepoLocator
  , toSiteLocator
  , toRepoLocator
  , getSite
  , getAllSites
  ) where

import Control.Concurrent.STM
import Data.Functor
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (maybeToList, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import Glean.Database.Backup.Backend as Backend
import Glean.Database.Repo
import Glean.Database.Types
import Glean.ServerConfig.Types (DatabaseBackupPolicy(..))
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Util.Some
import Glean.Util.Observed as Observed

fromSiteLocator :: Backends -> Text -> Maybe (Text, Some Site)
fromSiteLocator backends s = do
  let (prefix,colon_path) = Text.breakOn ":" s
  Some backend <- HashMap.lookup prefix backends
  (_,path) <- Text.uncons colon_path
  site <- fromPath backend path
  return (prefix, site)

fromRepoLocator :: Backends -> Text -> Maybe (Text, Some Site, Repo)
fromRepoLocator backends loc = do
  let (site_slash, repo_text) = Text.breakOnEnd "/" loc
  (site_text, _) <- Text.unsnoc site_slash
  (prefix, site) <- fromSiteLocator backends site_text
  repo <- parseRepo "." repo_text
  return (prefix, site, repo)

toSiteLocator :: Site site => Text -> site -> Text
toSiteLocator prefix site = prefix <> ":" <> toPath site

toRepoLocator :: Site site => Text -> site -> Repo -> Text
toRepoLocator prefix site repo =
  toSiteLocator prefix site <> "/" <> showtRepo "." repo

getSite
  :: Env
  -> Text
  -> STM (Maybe (Text, Some Site, DatabaseBackupPolicy))
getSite Env{..} repoName = do
  policy <- ServerConfig.config_backup <$> Observed.get envServerConfig
  let locator = Map.findWithDefault
        (databaseBackupPolicy_location policy)
        repoName
        (Map.map ServerConfig.backup_location $
          databaseBackupPolicy_repos policy)
  return $
    fromSiteLocator envBackupBackends locator
    <&> \(prefix, site) -> (prefix, site, policy)

getAllSites
  :: Env
  -> STM [(Text, Some Site, DatabaseBackupPolicy)]
getAllSites Env{..} = do
  policy <- ServerConfig.config_backup <$> Observed.get envServerConfig
  let defaultSite = maybeToList $
        fromSiteLocator envBackupBackends (databaseBackupPolicy_location policy)
      perRepoLocations = map ServerConfig.backup_location $ Map.elems $
        databaseBackupPolicy_repos policy
      perRepoSites = mapMaybe (fromSiteLocator envBackupBackends)
        perRepoLocations
  return $ map (\(prefix, site) -> (prefix, site, policy)) $
    defaultSite ++ perRepoSites
