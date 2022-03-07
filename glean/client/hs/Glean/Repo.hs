{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Repo
  ( -- * Listing repos
    getLatestRepo
  , getLatestRepos
  , LatestRepos(..)
  , NoDatabase(..)
    -- * Database util
  , dbShard
  ) where

import Control.Exception
import Data.Function
import Data.List
import Data.List.Extra (groupSortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)

import Util.Log

import Glean.Repo.Text
import Glean.Backend.Remote hiding (dbShard)
import qualified Glean.Backend.Remote as Backend
import Glean.Types


newtype NoDatabase = NoDatabase Text
  deriving Show
instance Control.Exception.Exception NoDatabase

-- | Collecting 'getLatestRepo' for every available repo in the backend
newtype LatestRepos = LatestRepos{ latestRepos :: Map Text Repo }

-- | Ask the server which databases are available, and select the
-- latest complete database for all repo names selected by the predicate.
getLatestRepos
  :: Backend be
  => be
  -> (Text -> Bool)  -- ^ Predicate to choose repo names to include
  -> IO LatestRepos
getLatestRepos backend keepName = do
  let
    -- If we're connecting to a tier and using shards, then we can
    -- assume that DBs in the Restoring state are usable, because
    -- hopefully some other server will have the DB. The idea is
    -- to get a consistent view of the available DBs even if we
    -- happen to contact a server that has recently come up and is
    -- still downloading DBs.

    ok Database{..} =
      (database_status == DatabaseStatus_Complete
        || usingShards backend &&
           database_status == DatabaseStatus_Restoring) &&
      keepName (repo_name database_repo) &&
      isNothing database_expire_time
  xss <- groupSortOn (repo_name . database_repo) . filter ok <$>
    localDatabases backend
  let pickRepo xs =
        let
          sorted = sortBy (flip compare `on` database_created_since_epoch) xs
        in checkRestorableAvailable backend sorted
      assemble rs = LatestRepos $ Map.fromList [ (repo_name r, r) | r <- rs ]
  assemble . catMaybes <$> mapM pickRepo xss

-- | Ask the server which databases are available, and select the
-- latest complete database for the given repo name.
--
-- Otherwise throw 'NoDatabase' exception
getLatestRepo
  :: Backend be
  => be
  -> Text  -- ^ The name of the repo, e.g. "fbsource"
  -> IO Repo
getLatestRepo backend name = do
  lr <- getLatestRepos backend (name ==)
  case Map.lookup name (latestRepos lr) of
    Nothing -> throwIO $ NoDatabase $ "no database found for: " <> name
    Just x -> return x

-- | For each DB that is "restorable" on the host that returned the
-- listDatabases result, check whether we can route to a host that has
-- the DB. If not, we'll pick the next available DB in the list. This
-- logic only kicks in when the service is a Tier and supports shards,
-- otherwise we ignore restorable DBs when filtering above.
--
-- Otherwise return Nothing.
checkRestorableAvailable :: Backend be => be -> [Database] -> IO (Maybe Repo)
checkRestorableAvailable _ [] = return Nothing
checkRestorableAvailable backend (Database{..}:dbs)
  | database_status /= DatabaseStatus_Restoring =
    return (Just database_repo)
  | not (usingShards backend) =
    checkRestorableAvailable backend dbs
  | otherwise = do
    vlog 1 $ "Repo " <> showRepo database_repo <>
      " is restoring, checking for shard availability..."
    avail <- hasDatabase backend database_repo
    vlog 1 $ "Repo " <> showRepo database_repo <>
       ": " <> (if avail then "some" else "no") <> " hosts have it"
    if not avail
      then checkRestorableAvailable backend dbs
      else return (Just database_repo)

dbShard :: Database -> Text
dbShard = Backend.dbShard . database_repo
