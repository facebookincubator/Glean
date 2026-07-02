{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | ACL ownership augmentation at database finalization time.
--
-- At 'syncCompletePredicates', after conventional ownership is computed
-- but before it is stored, this module augments the ownership with ACL
-- constraints from the accumulated 'PathACLConfig'.
--
-- Algorithm:
-- 1. Receive the accumulated 'PathACLConfig' (passed in as a parameter)
-- 2. Look up, via 'Storage.getUnitId', the UnitId for each ACL group
--    (registered as "acl:<name>" units just before computeOwnership)
-- 3. For each ownership unit, match against ACL config directory prefixes
-- 4. Build CNF per unit: for each matching directory level, OR all its
--    groups; AND across levels
-- 5. AND the ACL CNF with each fact's existing ownership expression
--    (via C++ FFI)
-- 6. Return the group->UnitId mapping for storage

module Glean.Database.ACLOwnership
  ( augmentOwnershipWithACL
  ) where

import Control.Monad (when)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word

import Util.Log (logInfo)

import Glean.Database.ACLConfig
  ( PathACLConfig
  , Path(..)
  , ACL(..)
  , getAllGroupIds
  , pathConfigToList
  )
import qualified Glean.Database.Storage as Storage
import Glean.RTS.Foreign.Ownership
  ( ComputedOwnership
  , UnitId(..)
  , augmentWithACL
  )

-- | Augment computed ownership with ACL constraints.
--
-- Returns @groupMapping@, mapping each group name to its UnitId. The
-- groups were registered as "acl:<name>" ownership units just before
-- 'Storage.computeOwnership' (see 'syncCompletePredicates').
augmentOwnershipWithACL
  :: Storage.DatabaseOps db
  => db
  -> ComputedOwnership
  -> PathACLConfig
  -> IO (HashMap ACL UnitId)
augmentOwnershipWithACL dbHandle own config = do
  -- Collect unique group names and look up their UnitIds. The groups
  -- were registered as "acl:<name>" units just before computeOwnership
  -- (see syncCompletePredicates).
  let allGroupNames = getAllGroupIds config
  groupUnitIds <- catMaybes <$> mapM (lookupGroupUnitId dbHandle) allGroupNames

  when (length groupUnitIds /= length allGroupNames) $
    logInfo $ "ACL augmentation WARNING: some groups not found as units. "
      ++ "Expected " ++ show (length allGroupNames)
      ++ ", found " ++ show (length groupUnitIds)

  logInfo $ "ACL augmentation: " ++ show (length groupUnitIds)
    ++ " groups resolved to UnitIds"

  -- Build group name → UnitId mapping
  let groupMapping = HashMap.fromList groupUnitIds
      resolveGroupUnitId name = HashMap.lookup name groupMapping

  -- Build per-unit ACL assignments by matching unit names against
  -- ACL config directory prefixes.
  assignments <- buildUnitAssignments
    dbHandle (pathConfigToList config) resolveGroupUnitId

  logInfo $ "ACL augmentation: " ++ show (length assignments)
    ++ " units matched ACL prefixes"

  -- Call C++ augmentation
  augmentWithACL own assignments

  return groupMapping

-- | Look up the 'UnitId' for an ACL group. Each group is registered in the DB
-- as an @"acl:<name>"@ ownership unit just before computeOwnership. Returns
-- 'Nothing' (and logs) when the group has no corresponding unit.
lookupGroupUnitId
  :: Storage.DatabaseOps db
  => db
  -> ACL
  -> IO (Maybe (ACL, UnitId))
lookupGroupUnitId dbHandle acl@(ACL name) = do
  let unitName = "acl:" <> Text.encodeUtf8 name
  mUnitId <- Storage.getUnitId dbHandle unitName
  case mUnitId of
    Just uid -> return $ Just (acl, uid)
    Nothing -> do
      logInfo $ "ACL augmentation: group unit not found: " ++ Text.unpack name
      return Nothing

-- | Build per-unit ACL assignments using prefix scans.
-- For each config entry, perform a RocksDB prefix scan on the
-- ownershipUnits column family to find matching units, then
-- group results by UnitId.
buildUnitAssignments
  :: Storage.DatabaseOps db
  => db
  -> [(Path, [ACL])]  -- ^ ACL config: (prefix, [group_names])
  -> (ACL -> Maybe UnitId)  -- ^ group name → UnitId resolver
  -> IO [(Word32, [[Word32]])]  -- ^ (UnitId, [[group_UnitIds_per_level]])
buildUnitAssignments dbHandle configEntries resolveGroup = do
    allPairs <- concat <$> mapM processEntry configEntries
    let grouped = HashMap.fromListWith (++)
          [(uid, [gids]) | (uid, gids) <- allPairs]
    return $ HashMap.toList grouped
    where
      processEntry (Path prefix, groupNames) = do
        let groupUnitIds =
              [ w | UnitId w <- mapMaybe resolveGroup groupNames ]
        if null groupUnitIds
          then return []
          else do
            let normPrefix = fromMaybe prefix
                  (Text.stripPrefix "fbcode/" prefix)
            matchingUnits <-
              findMatchingUnits normPrefix
            return
              [ (uid, groupUnitIds)
              | uid <- matchingUnits
              ]

      findMatchingUnits normPrefix = do
        units1 <- scanPrefix normPrefix
        units2 <-
          scanPrefix ("fbcode/" <> normPrefix)
        return $ units1 ++ units2

      scanPrefix prefix = do
        let prefixBS = Text.encodeUtf8 prefix
            prefixSlash = prefix <> "/"
        matches <-
          Storage.getUnitsByPrefix dbHandle prefixBS
        return
          [ uid
          | (nameBS, UnitId uid) <- matches
          , let name = Text.decodeUtf8 nameBS
          , not (Text.isPrefixOf "acl:" name)
          , name == prefix
            || Text.isPrefixOf prefixSlash name
          ]
