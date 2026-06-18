{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | ACL configuration validation and processing.
--
-- This module handles the path→ACL group IDs configuration that is sent
-- with each batch write. Each path maps to a list of ACL group ID strings.
-- Paths are directory paths. It is assumed that the map has a small number
-- of entries (less than 100).
--
-- Flow:
-- 1. Indexer sends path→[ACL group ID strings] mapping with each batch
-- 2. Server parses group ID strings into integers
-- 3. Server builds ACL ownership using the integer group IDs directly

module Glean.Database.ACLConfig
  ( -- * Path Config (from batch)
    Path(..)
  , ACL(..)
  , PathACLConfig
  , emptyPathConfig
  , pathConfigToList
  , getAllGroupIds
  , computePathConfigHash

  ) where

import Data.Aeson (encode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.List (sort, sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

import qualified Crypto.Hash as Crypto
import qualified Data.ByteArray as BA

-- | A directory path used as a key in the ACL configuration.
newtype Path = Path Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (Hashable)

-- | An ACL group ID string (e.g. "1", "2").
newtype ACL = ACL Text
  deriving stock (Show, Eq)
  deriving newtype (Hashable)

-- | Path ACL configuration from batch: maps paths to lists of ACL group ID
-- strings. Example: {"src/internal/": ["1", "2"], "src/public/": ["3"]}
type PathACLConfig = HashMap Path [ACL]

-- | Empty path configuration.
emptyPathConfig :: PathACLConfig
emptyPathConfig = HashMap.empty

-- | List the @(path, ACLs)@ entries of a path ACL config.
pathConfigToList :: PathACLConfig -> [(Path, [ACL])]
pathConfigToList = HashMap.toList

-- | Get all unique ACL group ID strings from the path config.
getAllGroupIds :: PathACLConfig -> [ACL]
getAllGroupIds = HashSet.toList . HashSet.fromList . concat . HashMap.elems

-- | Compute deterministic SHA-256 hash of path ACL config.
--
-- Uses canonical JSON serialization (sorted keys, sorted ACL lists,
-- compact format) for consistency across languages and implementations.
computePathConfigHash :: PathACLConfig -> Text
computePathConfigHash config =
  let -- Build canonical JSON with sorted keys
      sortedPairs = sortBy (comparing fst) $ HashMap.toList config
      jsonObj = Aeson.Object $ KeyMap.fromList
        [ (Key.fromText path, Aeson.toJSON (sort [ acl | ACL acl <- acls ]))
        | (Path path, acls) <- sortedPairs
        ]
      canonical = LBS.toStrict $ encode jsonObj
      -- Compute SHA-256 and convert to hex
      digest :: Crypto.Digest Crypto.SHA256
      digest = Crypto.hash canonical
      hexBytes = Base16.encode (BA.convert digest :: BS.ByteString)
  in Text.decodeUtf8 hexBytes
