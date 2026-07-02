{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Data
  ( storeSchema
  , retrieveSchema
  , storeUnits
  , retrieveUnits
  , storeSlices
  , retrieveSlices
  , storeACLGroupMapping
  , retrieveACLGroupMapping
  , storeFirstACLID
  , retrieveFirstACLID
  , storePathACLConfig
  , retrievePathACLConfig
  ) where

import Data.Binary
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict, fromStrict)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)

import Thrift.Protocol.Compact

import Util.Log

import Glean.Database.Exception
import Glean.Database.Storage (DatabaseOps)
import qualified Glean.Database.Storage as Storage
import Glean.RTS.Foreign.Ownership
import Glean.Types (Repo)
import Glean.Internal.Types (StoredSchema)

sCHEMA_KEY :: ByteString
sCHEMA_KEY = "schema"

-- | Stores the units that are excluded (or included) from the base DB
uNITS_KEY :: ByteString
uNITS_KEY = "units"

-- | Stores the slices for the base DBs in a stack
sLICES_KEY :: ByteString
sLICES_KEY = "slices"

-- | ACL group name-to-UsetId mapping JSON blob key
aCL_GROUP_MAPPING_KEY :: ByteString
aCL_GROUP_MAPPING_KEY = "acl_group_mapping"

-- | Path ACL config storage key (path -> ACL ID string)
pATH_ACL_CONFIG_KEY :: ByteString
pATH_ACL_CONFIG_KEY = "path_acl_config"

-- | First ACL ID boundary key
fIRST_ACL_ID_KEY :: ByteString
fIRST_ACL_ID_KEY = "first_acl_id"

storeSchema :: DatabaseOps db => db -> StoredSchema -> IO ()
storeSchema db = Storage.store db sCHEMA_KEY . serializeCompact

retrieveSchema :: DatabaseOps db => Repo -> db -> IO (Maybe StoredSchema)
retrieveSchema repo db = do
  value <- Storage.retrieve db sCHEMA_KEY
  case deserializeCompact <$> value of
    Just (Right info) -> return $ Just info
    Just (Left msg) -> dbError repo $ "invalid schema: " ++ msg
    Nothing -> return Nothing

storeUnits :: DatabaseOps db => db -> [ByteString] -> IO ()
storeUnits db = Storage.store db uNITS_KEY . toStrict . encode

retrieveUnits :: DatabaseOps db => Repo -> db -> IO (Maybe [ByteString])
retrieveUnits repo db = do
  value <- Storage.retrieve db uNITS_KEY
  case decodeOrFail . fromStrict <$> value of
    Just (Right (_, _, units)) -> return $ Just units
    Just (Left (_, _, msg)) -> dbError repo $ "invalid units: " ++ msg
    Nothing -> return Nothing

-- Slices are each serialized using the RTS Slice::serialize(), and then
-- the list of serialized slices :: [ByteString] is serialized with
-- the Haskell Binary encoder.
storeSlices :: DatabaseOps db => db -> [Slice] -> IO ()
storeSlices db slices = do
  bytestrings <- mapM serializeSlice slices
  Storage.store db sLICES_KEY $ toStrict $ encode bytestrings

retrieveSlices :: DatabaseOps db => Repo -> db -> IO (Maybe [Slice])
retrieveSlices repo db = do
  value <- Storage.retrieve db sLICES_KEY
  case decodeOrFail . fromStrict <$> value of
    Just (Right (_, _, bytestrings)) ->
      Just <$> mapM deserializeSlice bytestrings
    Just (Left (_, _, msg)) -> dbError repo $ "invalid slices: " ++ msg
    Nothing -> return Nothing

-- | Store the ACL group name-to-UsetId mapping as a JSON blob.
-- Format: {"group_name": uset_id, ...} where uset_id is a number.
storeACLGroupMapping :: DatabaseOps db => db -> ByteString -> IO ()
storeACLGroupMapping db = Storage.store db aCL_GROUP_MAPPING_KEY

-- | Retrieve and parse the ACL group name-to-UsetId mapping.
-- Returns 'Nothing' when no mapping is stored. If a value is stored but fails
-- to parse as valid JSON, logs a warning (so corrupted data is not silently
-- mistaken for an empty mapping) and returns 'Nothing'.
retrieveACLGroupMapping
  :: DatabaseOps db => db -> IO (Maybe (HashMap.HashMap Text Word32))
retrieveACLGroupMapping db = do
  mBytes <- Storage.retrieve db aCL_GROUP_MAPPING_KEY
  case mBytes of
    Nothing -> return Nothing
    Just bytes -> case parseACLGroupMappingJson bytes of
      Just mapping -> return (Just mapping)
      Nothing -> do
        logWarning
          "corrupted acl_group_mapping, ignoring stored ACL group mapping"
        return Nothing

-- | Store the first ACL ID boundary (UsetId where ACL IDs start).
-- This marks the boundary between regular ownership IDs and ACL IDs.
storeFirstACLID :: DatabaseOps db => db -> UsetId -> IO ()
storeFirstACLID db (UsetId uid) =
  Storage.store db fIRST_ACL_ID_KEY (toStrict $ encode uid)

-- | Retrieve the first ACL ID boundary.
-- Returns 'Nothing' when no boundary is stored. If a value is stored but
-- fails to decode, logs an error (the boundary is a security-relevant marker,
-- so silent corruption must be surfaced to operators) and returns 'Nothing'.
retrieveFirstACLID :: DatabaseOps db => db -> IO (Maybe UsetId)
retrieveFirstACLID db = do
  mBytes <- Storage.retrieve db fIRST_ACL_ID_KEY
  case mBytes of
    Nothing -> return Nothing
    Just bs -> case decodeOrFail (fromStrict bs) of
      Right (_, _, uid) -> return $ Just (UsetId uid)
      Left (_, _, errMsg) -> do
        logError $ "corrupted first_acl_id, ignoring boundary: " ++ errMsg
        return Nothing

-- | Store the path ACL config (path -> list of ACL group ID strings).
-- Format: JSON object {"path1": ["id1", "id2"], "path2": ["id3"], ...}
storePathACLConfig
  :: DatabaseOps db
  => db -> HashMap.HashMap Text [Text] -> IO ()
storePathACLConfig db config =
  Storage.store db pATH_ACL_CONFIG_KEY (toStrict (Aeson.encode config))

-- | Retrieve the path ACL config (path -> list of ACL group ID strings).
-- Returns 'Nothing' when no config is stored. If a value is stored but fails
-- to parse as valid JSON, logs a warning (so corrupted data is not silently
-- mistaken for an empty config) and returns 'Nothing'.
retrievePathACLConfig
  :: DatabaseOps db
  => db
  -> IO (Maybe (HashMap.HashMap Text [Text]))
retrievePathACLConfig db = do
  mBytes <- Storage.retrieve db pATH_ACL_CONFIG_KEY
  case mBytes of
    Nothing -> return Nothing
    Just bytes -> case Aeson.decodeStrict' bytes of
      Just config -> return (Just config)
      Nothing -> do
        logWarning
          "corrupted path_acl_config, ignoring stored path ACL config"
        return Nothing

-- | Parse the ACL group mapping JSON format.
-- Parses JSON of the form: {"group_name": unitId, ...}
-- where unitId is an integer. Returns 'Nothing' on parse failure so callers
-- can distinguish corrupted data from a legitimately empty mapping, rather
-- than silently treating malformed ACL data as "no groups".
parseACLGroupMappingJson :: ByteString -> Maybe (HashMap.HashMap Text Word32)
parseACLGroupMappingJson = Aeson.decodeStrict'
