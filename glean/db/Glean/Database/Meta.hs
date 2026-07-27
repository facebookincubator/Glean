{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Glean.Database.Meta
  ( Meta(..)
  , DBTimestamp(..)
  , newMeta
  , showCompleteness
  , showCompletenessFull
  , completenessStatus
  , dbAge
  , dbTime
  , dbCompletedOrCreated
  , metaToThriftDatabase
  , metaToProps
  , metaFromProps
  , utcTimeToPosixEpochTime
  , posixEpochTimeToUTCTime
  , posixEpochTimeToTime
  -- ACL mode detection from database properties
  , ACLMode(..)
  , getACLMode
  , showACLMode
  , isACLEnabled
  , isACLEnforced
  , isACLPermissive
  ) where

import qualified Data.ByteString.Char8 as B
import Data.Functor
import qualified Data.HashMap.Strict as HashMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import Data.Time (UTCTime, NominalDiffTime, diffUTCTime)
import Data.Time.Clock.POSIX

import Thrift.Protocol.JSON
import Util.TimeSec

import Glean.ServerConfig.Types (DBVersion(..))
import Glean.Internal.Types
import Glean.Types

data DBTimestamp = DBTimestamp
  { timestampCreated :: UTCTime
  , timestampRepoHash :: Maybe UTCTime
  }

-- | Produce DB metadata
newMeta
  :: StorageName
  -> DBVersion -- ^ DB version
  -> DBTimestamp -- ^ creation time and repo hash time
  -> Completeness -- ^ write status
  -> DatabaseProperties -- ^ user properties
  -> Maybe Dependencies -- ^ stacked
  -> Meta
newMeta storage version timestamp completeness properties deps = Meta
  { metaVersion = version
  , metaCreated = utcTimeToPosixEpochTime $ timestampCreated timestamp
  , metaRepoHashTime = utcTimeToPosixEpochTime <$> timestampRepoHash timestamp
  , metaCompleteness = completeness
  , metaProperties = properties
  , metaBackup = Nothing
  , metaDependencies = deps
  , metaCompletePredicates = mempty
  , metaAxiomComplete = False
  , metaStorage = storage
  }

showCompleteness :: Completeness -> Text
showCompleteness Incomplete{} = "incomplete"
showCompleteness Complete{} = "complete"
showCompleteness Broken{} = "broken"
showCompleteness Finalizing{} = "finalizing"

showCompletenessFull :: Completeness -> Text
showCompletenessFull (Broken (DatabaseBroken task reason)) =
    "broken at task \"" <> task <> "\": " <> reason
showCompletenessFull x = showCompleteness x

completenessStatus :: Meta -> DatabaseStatus
completenessStatus meta = case metaCompleteness meta of
  Incomplete{} -> DatabaseStatus_Incomplete
  Complete{} -> DatabaseStatus_Complete
  Broken{} -> DatabaseStatus_Broken
  Finalizing{} -> DatabaseStatus_Finalizing

dbAge :: UTCTime -> Meta -> NominalDiffTime
dbAge now meta = now `diffUTCTime` posixEpochTimeToUTCTime (metaCreated meta)

-- | We sort DBs by metaRepoHashTime if available, or otherwise metaCreated
dbTime :: Meta -> PosixEpochTime
dbTime meta = fromMaybe (metaCreated meta) (metaRepoHashTime meta)

-- | The time indexing completed, or the creation time for DBs that are not
-- complete. Used as a stable tiebreaker between DBs that share a 'dbTime',
-- e.g. multiple schema-id builds of the same repo revision.
dbCompletedOrCreated :: Meta -> PosixEpochTime
dbCompletedOrCreated meta = case metaCompleteness meta of
  Complete DatabaseComplete{databaseComplete_time = t} -> t
  _ -> metaCreated meta

metaToThriftDatabase
  :: DatabaseStatus
  -> Maybe UTCTime  -- time of expiry, if any
  -> Repo
  -> Meta
  -> Database
metaToThriftDatabase status expire repo Meta{..} = Database
  { database_repo = repo
  , database_status = status
  , database_location = metaBackup
  , database_created_since_epoch = metaCreated
  , database_expire_time = utcTimeToPosixEpochTime <$> expire
  , database_properties = metaProperties
  , database_completed = case metaCompleteness of
      Complete DatabaseComplete{databaseComplete_time=t} -> Just t
      _ -> Nothing
  , database_repo_hash_time = metaRepoHashTime
  , database_dependencies = metaDependencies
  , database_broken = case metaCompleteness of
      Broken broken -> Just broken
      _ -> Nothing
  , database_complete = case metaCompleteness of
      Complete complete -> Just complete
      _ -> Nothing
  }

metaToProps :: Meta -> Map String String
metaToProps meta = Map.fromList [("meta", B.unpack (serializeJSON meta))]

metaFromProps :: Text -> Map String String -> Either String Meta
metaFromProps loc ps = case Map.lookup "meta" ps of
  Just str ->
    deserializeJSON (B.pack str) <&> \meta -> meta { metaBackup = Just loc }
  Nothing -> Left "missing property 'meta'"

-- Time functions

utcTimeToPosixEpochTime :: UTCTime -> PosixEpochTime
utcTimeToPosixEpochTime = PosixEpochTime . round . utcTimeToPOSIXSeconds

posixEpochTimeToUTCTime :: PosixEpochTime -> UTCTime
posixEpochTimeToUTCTime = toUTCTime . posixEpochTimeToTime

posixEpochTimeToTime :: PosixEpochTime -> Time
posixEpochTimeToTime = Time . fromIntegral . unPosixEpochTime

-- | ACL mode for database operations
data ACLMode
  = ACLDisabled     -- ^ ACLs are disabled (no glean.acl property)
  | ACLActive       -- ^ Basic ACL support (glean.acl = "enabled")
  | ACLEnforced     -- ^ Strict mode (error on facts without units)
  | ACLPermissive   -- ^ Permissive mode (no directory inheritance)
  | ACLBroken       -- ^ ACL property set but column family is missing/broken
  deriving (Eq, Show)

-- | Convert ACL mode to a human-readable description for logging
showACLMode :: ACLMode -> String
showACLMode ACLDisabled = "ACL: disabled"
showACLMode ACLActive = "ACL: active"
showACLMode ACLEnforced = "ACL: enforced (strict mode)"
showACLMode ACLPermissive = "ACL: permissive (no directory inheritance)"
showACLMode ACLBroken = "ACL: missing (column family not yet written)"

-- | Get the ACL mode from database properties
getACLMode :: DatabaseProperties -> ACLMode
getACLMode props = case HashMap.lookup "glean.acl" props of
  Just "enabled" -> ACLActive
  Just "enforced" -> ACLEnforced
  Just "permissive" -> ACLPermissive
  Just "broken" -> ACLBroken
  _ -> ACLDisabled

-- | Check if ACLs are enabled (any mode except disabled)
isACLEnabled :: DatabaseProperties -> Bool
isACLEnabled props = getACLMode props /= ACLDisabled

-- | Check if ACLs are in enforced mode
isACLEnforced :: DatabaseProperties -> Bool
isACLEnforced props = getACLMode props == ACLEnforced

-- | Check if ACLs are in permissive mode
isACLPermissive :: DatabaseProperties -> Bool
isACLPermissive props = getACLMode props == ACLPermissive
