module Glean.Database.Backup.Locator
  ( fromSiteLocator
  , fromRepoLocator
  , toSiteLocator
  , toRepoLocator
  , getSite
  ) where

import Control.Concurrent.STM
import Data.Functor
import qualified Data.HashMap.Strict as HashMap
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
  -> STM (Maybe (Text, Some Site, DatabaseBackupPolicy))
getSite Env{..} = do
  policy <- ServerConfig.config_backup <$> Observed.get envServerConfig
  return $
    fromSiteLocator envBackupBackends (databaseBackupPolicy_location policy)
    <&> \(prefix, site) -> (prefix, site, policy)
