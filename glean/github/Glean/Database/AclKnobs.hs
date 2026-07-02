{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Open-source stub for the Facebook-internal ACL JustKnobs. The
-- Facebook build provides the real implementation (backed by JustKnobs)
-- via the module of the same name under @glean/facebook@. In the
-- open-source build both knobs default to 'False', so ACL handling is
-- disabled.
module Glean.Database.AclKnobs
  ( aclCalculateEnabled
  , aclStoreEnabled
  , aclCheckEnabled
  , aclLocalTestingAllowed
  ) where

-- | Whether ACL ownership augmentation runs at @glean complete@.
aclCalculateEnabled :: IO Bool
aclCalculateEnabled = return False

-- | Whether per-batch ACL config is validated and persisted during writes.
aclStoreEnabled :: IO Bool
aclStoreEnabled = return False

-- | Whether the query server filters results by ACL group membership.
aclCheckEnabled :: IO Bool
aclCheckEnabled = return False

-- | Whether the @--acl-groups@ CLI option may be used against a local DB.
aclLocalTestingAllowed :: IO Bool
aclLocalTestingAllowed = return False
