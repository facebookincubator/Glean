-- Copyright (c) Facebook, Inc. and its affiliates.

{-# OPTIONS_GHC -Wno-orphans #-}
module Glean.Database.Meta
  ( Meta(..)
  , newMeta
  , showCompleteness
  , completenessStatus
  , completenessTasks
  , dbAge
  , metaToThriftDatabase
  , metaToProps
  , metaFromProps
  , utcTimeToPosixEpochTime
  , posixEpochTimeToUTCTime
  ) where

import qualified Data.ByteString.Char8 as B
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Time (UTCTime, NominalDiffTime, diffUTCTime)
import Data.Time.Clock.POSIX

import Thrift.Protocol.JSON
import Util.TimeSec

import Glean.ServerConfig.Types (DBVersion(..))
import Glean.Internal.Types
import Glean.Types

-- | Produce DB metadata
newMeta
  :: DBVersion -- ^ DB version
  -> UTCTime -- ^ creation time
  -> Completeness -- ^ write status
  -> DatabaseProperties -- ^ user properties
  -> Maybe Dependencies -- ^ stacked
  -> Meta
newMeta version created completeness properties deps = Meta
  { metaVersion = version
  , metaCreated = utcTimeToPosixEpochTime created
  , metaCompleteness = completeness
  , metaProperties = properties
  , metaBackup = Nothing
  , metaDependencies = deps
  , metaCompletePredicates = mempty
  , metaAxiomComplete = False
  }

showCompleteness :: Completeness -> Text
showCompleteness Incomplete{} = "incomplete"
showCompleteness Complete{} = "complete"
showCompleteness Broken{} = "broken"
showCompleteness Finalizing{} = "finalizing"

completenessStatus :: Meta -> DatabaseStatus
completenessStatus meta = case metaCompleteness meta of
  Incomplete{} -> DatabaseStatus_Incomplete
  Complete{} -> DatabaseStatus_Complete
  Broken{} -> DatabaseStatus_Broken
  Finalizing{} -> DatabaseStatus_Complete
    -- Report Finalizing as Complete: the DB is ready to be queried,
    -- the finalize steps are administrative only and don't change
    -- anything observable.

completenessTasks :: Meta -> Maybe (HashMap Text Task)
completenessTasks meta = case metaCompleteness meta of
  Incomplete (DatabaseIncomplete_tasks tasks) -> Just tasks
  _ -> Nothing

dbAge :: UTCTime -> Meta -> NominalDiffTime
dbAge now meta = now `diffUTCTime` posixEpochTimeToUTCTime (metaCreated meta)

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
      Complete (DatabaseComplete time) -> Just time
      _ -> Nothing
  , database_repo_hash_time = Nothing
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
posixEpochTimeToUTCTime = toUTCTime . Time . fromIntegral . unPosixEpochTime
