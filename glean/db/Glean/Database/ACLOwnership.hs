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
--    (registered as "acl:<name>" units during batch writes)
-- 3. For each ownership unit, match against ACL config directory prefixes
-- 4. Build CNF per unit: for each matching directory level, OR all its
--    groups; AND across levels
-- 5. AND the ACL CNF with each fact's existing ownership expression
--    (via C++ FFI)
-- 6. Return firstACLID and group→UnitId mapping for storage

module Glean.Database.ACLOwnership
  ( augmentOwnershipWithACL
  ) where

import Control.Monad (when)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import Data.Text (Text)
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
  , UsetId(..)
  , computedOwnershipUnitCount
  , augmentWithACL
  )

-- | Augment computed ownership with ACL constraints.
--
-- Returns @(firstACLID, groupMapping)@ where:
-- * @firstACLID@ is the first UsetId used as an ACL boundary marker
-- * @groupMapping@ maps each group name to its UnitId (registered
--   during batch writes as "acl:<name>" ownership units)
augmentOwnershipWithACL
  :: Storage.DatabaseOps db
  => db
  -> ComputedOwnership
  -> PathACLConfig
  -> IO (UsetId, HashMap ACL UnitId)
augmentOwnershipWithACL dbHandle own config = do
  unitCount <- computedOwnershipUnitCount own
  logInfo $ "ACL augmentation: unit count = " ++ show unitCount

  -- Collect unique group names and look up their actual UnitIds.
  -- These were registered as "acl:<name>" units during batch writes.
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

  -- Find the minimum ACL group UnitId as the firstACLID boundary.
  -- All UnitIds < firstACLID are regular ownership units.
  -- All UnitIds >= firstACLID are ACL group units.
  let aclFirstId = case [ w | (_, UnitId w) <- groupUnitIds ] of
        [] -> UsetId unitCount
        ids -> UsetId (minimum ids)

  -- Build per-unit ACL assignments by matching unit names against
  -- ACL config directory prefixes.
  assignments <- buildUnitAssignments
    dbHandle unitCount (pathConfigToList config) resolveGroupUnitId

  logInfo $ "ACL augmentation: " ++ show (length assignments)
    ++ " units matched ACL prefixes"

  -- Call C++ augmentation
  augmentWithACL own assignments

  logInfo $ "ACL augmentation complete: firstACLID=" ++ show aclFirstId

  return (aclFirstId, groupMapping)

-- | Look up the 'UnitId' for an ACL group. Each group is registered in the DB
-- as an @"acl:<name>"@ ownership unit during batch writes. Returns 'Nothing'
-- (and logs) when the group has no corresponding unit.
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

-- | Iterate all units and build ACL assignments.
-- For each unit, match its name against ACL config directory prefixes
-- and group matches by prefix level.
buildUnitAssignments
  :: Storage.DatabaseOps db
  => db
  -> Word32  -- ^ number of units
  -> [(Path, [ACL])]  -- ^ ACL config: (prefix, [group_names])
  -> (ACL -> Maybe UnitId)  -- ^ group name → UnitId resolver
  -> IO [(Word32, [[Word32]])]  -- ^ (UnitId, [[group_UnitIds_per_level]])
buildUnitAssignments dbHandle numUnits configEntries resolveGroup
  -- Guard against numUnits == 0: 'numUnits - 1' on a Word32 would underflow to
  -- maxBound and iterate ~4 billion times.
  | numUnits == 0 = return []
  | otherwise = catMaybes <$> mapM processUnit [0 .. numUnits - 1]
  where
    processUnit unitId = do
      mName <- Storage.getUnit dbHandle (UnitId unitId)
      case mName of
        Nothing -> return Nothing
        Just nameBytes -> do
          let name = Text.decodeUtf8 nameBytes
          -- ACL group units ("acl:*") are themselves ownership units, so they
          -- appear in this iteration. They are not source paths, so skip them
          -- (this is expected, not an error).
          if Text.isPrefixOf "acl:" name
            then return Nothing
            else do
              -- Normalize the unit name the same way as the config prefixes
              -- (strip the "fbcode/" repo prefix) so the two are compared on
              -- equal footing.
              let normName = fromMaybe name (Text.stripPrefix "fbcode/" name)
                  matches = findMatchingLevels normName
              return $ if null matches
                then Nothing
                else Just (unitId, matches)

    -- Find ACL prefix matches grouped by directory level.
    -- Returns one group-UnitId list per matching config prefix.
    findMatchingLevels :: Text -> [[Word32]]
    findMatchingLevels unitName =
      [ groupUnitIds
      | (normPrefix, groupNames) <- normalizedEntries
      , normPrefix == unitName
        || Text.isPrefixOf (normPrefix <> "/") unitName
      , let groupUnitIds = [ w | UnitId w <- mapMaybe resolveGroup groupNames ]
      , not (null groupUnitIds)
      ]

    -- Pre-normalize config prefixes once (stripping the "fbcode/" repo prefix)
    -- so the per-unit matching above does not repeat the work. The config has
    -- few entries (< 100), so a linear scan per unit is acceptable.
    normalizedEntries =
      [ (fromMaybe prefix (Text.stripPrefix "fbcode/" prefix), groupNames)
      | (Path prefix, groupNames) <- configEntries
      ]
