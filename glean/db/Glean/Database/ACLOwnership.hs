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
-- 1. Read PathACLConfig from DB metadata
-- 2. Look up UnitIds for each ACL group (registered as "acl:<name>"
--    units during batch writes)
-- 3. For each ownership unit, match against ACL config directory prefixes
-- 4. Build CNF per unit: for each matching directory level, OR all its
--    groups; AND across levels
-- 5. AND the ACL CNF with each fact's existing ownership expression
--    (via C++ FFI)
-- 6. Return firstACLID and group→UnitId mapping for storage

module Glean.Database.ACLOwnership
  ( augmentOwnershipWithACL
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (nub)
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word

import Util.Log (logInfo)

import Glean.Database.ACLConfig (PathACLConfig)
import qualified Glean.Database.Storage as Storage
import Glean.RTS.Foreign.Ownership
  ( ComputedOwnership
  , UnitId(..)
  , UsetId(..)
  , computedOwnershipFirstSetId
  , makeACLCnf
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
  -> IO (UsetId, HashMap Text Word32)
augmentOwnershipWithACL dbHandle own config = do
  firstSetId <- computedOwnershipFirstSetId own
  logInfo $ "ACL augmentation: firstSetId (unit count) = "
    ++ show firstSetId

  -- Collect unique group names and look up their actual UnitIds.
  -- These were registered as "acl:<name>" units during batch writes.
  let allGroupNames = nub $ concat $ HashMap.elems config
  groupUnitIds <- catMaybes <$> mapM lookupGroupUnitId allGroupNames

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
  let aclFirstId = case map snd groupUnitIds of
        [] -> UsetId firstSetId
        ids -> UsetId (minimum ids)

  -- Build per-unit ACL assignments by matching unit names against
  -- ACL config directory prefixes, and construct CNF UsetIds.
  let configEntries = HashMap.toList config
  assignments <- buildUnitAssignments
    dbHandle own firstSetId configEntries resolveGroupUnitId

  logInfo $ "ACL augmentation: " ++ show (length assignments)
    ++ " units matched ACL prefixes"

  -- Augment fact ownership with prebuilt CNF UsetIds
  augmentWithACL own assignments

  logInfo $ "ACL augmentation complete: firstACLID="
    ++ show aclFirstId

  return (aclFirstId, fmap fromIntegral groupMapping)

  where
    when True action = action
    when False _ = return ()

    lookupGroupUnitId :: Text -> IO (Maybe (Text, Word32))
    lookupGroupUnitId name = do
      let unitName = "acl:" <> Text.encodeUtf8 name
      mUnitId <- Storage.getUnitId dbHandle unitName
      case mUnitId of
        Just (UnitId uid) -> return $ Just (name, uid)
        Nothing -> do
          logInfo $ "ACL augmentation: group unit not found: "
            ++ Text.unpack name
          return Nothing

-- | Iterate all units and build ACL assignments.
-- For each unit, match its name against ACL config directory prefixes,
-- build the CNF expression via FFI, and return (UnitId, CNF UsetId) pairs.
buildUnitAssignments
  :: Storage.DatabaseOps db
  => db
  -> ComputedOwnership
  -> Word32  -- ^ firstSetId (= number of units)
  -> [(Text, [Text])]  -- ^ ACL config: (prefix, [group_names])
  -> (Text -> Maybe Word32)  -- ^ group name → UnitId resolver
  -> IO [(Word32, UsetId)]  -- ^ (UnitId, CNF UsetId)
buildUnitAssignments dbHandle own numUnits configEntries resolveGroup = do
  results <- mapM processUnit [0 .. numUnits - 1]
  return $ concat results
  where
    processUnit unitId = do
      mName <- Storage.getUnit dbHandle (UnitId unitId)
      case mName of
        Nothing -> return []
        Just nameBytes -> do
          let name = Text.decodeUtf8 nameBytes
          -- Skip "acl:*" units (they're ACL groups, not source units)
          if Text.isPrefixOf "acl:" name
            then return []
            else do
              let normName = fromMaybe name
                    (Text.stripPrefix "fbcode/" name)
                  matches = findMatchingLevels normName
              if null matches
                then return []
                else do
                  cnfId <- makeACLCnf own matches
                  return [(unitId, cnfId)]

    -- Find ACL prefix matches grouped by directory level.
    -- Returns list of group-UnitId-lists, one per matching prefix.
    findMatchingLevels :: Text -> [[Word32]]
    findMatchingLevels unitName =
      [ groupUnitIds
      | (prefix, groupNames) <- configEntries
      , let normPrefix = fromMaybe prefix
              (Text.stripPrefix "fbcode/" prefix)
      , normPrefix == unitName
        || Text.isPrefixOf (normPrefix <> "/") unitName
      , let groupUnitIds = mapMaybe resolveGroup groupNames
      , not (null groupUnitIds)
      ]
