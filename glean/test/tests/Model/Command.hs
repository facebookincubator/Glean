{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Model.Command where

import Data.Containers.ListUtils (nubOrd)
import Data.Int (Int64)
import Data.Text (pack, Text)
import qualified Glean.Database.Storage as Storage
import Glean.Internal.Types (Completeness (..), Meta (..))
import Glean.Types (
  DatabaseComplete (..),
  Dependencies (..),
  PosixEpochTime (PosixEpochTime),
  Repo (Repo, repo_hash, repo_name),
  Stacked (..)
 )
import Model.Model (ShardId, numberOfShards)
import Test.QuickCheck (
  Arbitrary (arbitrary),
  Arbitrary1 (liftArbitrary),
  Gen,
  choose,
  elements,
  oneof,
 )
import Text.Printf (printf)
import TextShow (showt)

data Command
  = NewRemoteDB Repo Meta
  | TimeElapsed Int64
  | ShardingAssignmentChange ShardingAssignmentChange
  | DBDownloaded

commandType :: Command -> String
commandType NewRemoteDB {} = "NewRemoteDB"
commandType TimeElapsed {} = "TimeElapsed"
commandType (ShardingAssignmentChange ShardAdded {}) = "ShardAdded"
commandType (ShardingAssignmentChange ShardRemoved {}) = "ShardRemoved"
commandType DBDownloaded {} = "DBDownloaded"

instance Show Command where
  show (NewRemoteDB Repo {..} _) =
    printf "DB %s %s" repo_name repo_hash
  show (TimeElapsed n) = printf "T %d" n
  show (ShardingAssignmentChange x) = show x
  show DBDownloaded = "DBDownloaded"

data ShardingAssignmentChange
  = ShardAdded ShardId
  | ShardRemoved ShardId
  deriving (Show)

instance Arbitrary Command where
  arbitrary =
    oneof
      [ ShardingAssignmentChange <$> arbitrary
      , TimeElapsed <$> choose (1, 100)
      , pure DBDownloaded
      , do
          repo_name <- elements ["repo1", "repo2"]
          shard <- choose (0, numberOfShards)
          databaseComplete_bytes <- liftArbitrary genDatabaseSize
          let databaseComplete_time = PosixEpochTime (fromIntegral shard)
              meta =
                defMeta
                  { metaCompleteness = Complete $ DatabaseComplete {..}
                  }
              repo_hash = pack $ show shard

          return $ NewRemoteDB Repo {..} meta
      ]

instance Arbitrary ShardingAssignmentChange where
  arbitrary =
    oneof
      [ ShardAdded . showt <$> choose (0, numberOfShards)
      , ShardRemoved . showt <$> choose (0, numberOfShards)
      ]

defDependency :: Text -> Text -> Dependencies
defDependency name hash = Dependencies_stacked $ Stacked name hash Nothing

defMeta :: Meta
defMeta = defMetaWithDependency Nothing

defMetaWithDependency :: Maybe Dependencies -> Meta
defMetaWithDependency dependencies =
  Meta
    { metaVersion = Storage.currentVersion
    , metaCreated = PosixEpochTime 0
    , metaCompleteness =
        Complete
          DatabaseComplete
            { databaseComplete_time = PosixEpochTime 0
            , databaseComplete_bytes = Just 1
            }
    , metaBackup = Nothing
    , metaProperties = mempty
    , metaDependencies = dependencies
    , metaCompletePredicates = mempty
    , metaAxiomComplete = False
    , metaRepoHashTime = Nothing
    }

genDatabaseSize :: Gen Int64
genDatabaseSize = elements [1000, 2000]

-- Fix the DB creation times so that they are in the present
fixCreationTimes :: PosixEpochTime -> [Command] -> [Command]
fixCreationTimes (PosixEpochTime accTime) (TimeElapsed t : rest) =
  fixCreationTimes (PosixEpochTime $ t + accTime) rest
fixCreationTimes t (NewRemoteDB repo meta : rest) =
  NewRemoteDB repo meta {metaCreated = t} : fixCreationTimes t rest
fixCreationTimes t (other : rest) = other : fixCreationTimes t rest
fixCreationTimes _ [] = []

-- | Ensures that time always passes
insertTimeLapses :: [Command] -> [Command]
insertTimeLapses (c@TimeElapsed {} : c' : rest) = c : c' : insertTimeLapses rest
insertTimeLapses (c : rest) = TimeElapsed 1 : c : insertTimeLapses rest
insertTimeLapses other = other

-- | True only if the command sequence does not lead to duplicate DB instances
noDuplicateDBs :: [Command] -> Bool
noDuplicateDBs commands = length (nubOrd dbs) == length dbs
  where
    dbs = [repo | NewRemoteDB repo _ <- commands]

-- | A 'good enough for testing' sequence of commands
good :: [Command] -> Bool
good cmds = not (null cmds) && noDuplicateDBs cmds && checkDownloads cmds

{- | Checks that every DBDownload commands has a matching DB.
   This is only an approximation since the shard assignment is not considered
-}
checkDownloads :: [Command] -> Bool
checkDownloads = go (0 :: Int)
  where
    go (-1) _ = False
    go _ [] = True
    go dbsAvailable (NewRemoteDB {} : rest) = go (dbsAvailable + 1) rest
    go dbsAvailable (DBDownloaded {} : rest) = go (dbsAvailable - 1) rest
    go dbsAvailable (_other : rest) = go dbsAvailable rest
