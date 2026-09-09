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
-- This module handles the path→ACL group IDs configuration supplied at
-- @glean create@. Each path maps to a list of ACL group ID strings.
-- Paths are directory paths. It is assumed that the map has a small number
-- of entries (less than 100).
--
-- Flow:
-- 1. @glean create --acl-config@ sends the path→[ACL group ID strings]
--    mapping on 'Glean.Types.KickOff'; the server persists it in DB metadata
-- 2. At @glean complete@ the server reads it back and registers each group
--    as an @acl:\<name\>@ ownership unit
-- 3. Server builds ACL ownership from those unit IDs

module Glean.Database.ACLConfig
  ( Path(..)
  , ACL(..)
  , PathACLConfig
  , pathConfigToList
  , getAllGroupIds

  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.Text (Text)

-- | A directory path used as a key in the ACL configuration.
newtype Path = Path Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (Hashable)

-- | An ACL group ID string (e.g. "1", "2").
newtype ACL = ACL Text
  deriving stock (Show, Eq)
  deriving newtype (Hashable)

-- | Path ACL configuration: maps paths to lists of ACL group ID strings.
-- Example: {"src/internal/": ["1", "2"], "src/public/": ["3"]}
type PathACLConfig = HashMap Path [ACL]

-- | List the @(path, ACLs)@ entries of a path ACL config.
pathConfigToList :: PathACLConfig -> [(Path, [ACL])]
pathConfigToList = HashMap.toList

-- | Get all unique ACL group ID strings from the path config.
getAllGroupIds :: PathACLConfig -> [ACL]
getAllGroupIds = HashSet.toList . HashSet.fromList . concat . HashMap.elems
