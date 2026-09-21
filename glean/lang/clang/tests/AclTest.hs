{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Run the data-driven ACL snapshot tests with the recursive Clang indexer.
module AclTest (main) where

import Util.IO (listDirectoryRecursive)

import Glean.Regression.Snapshot.Acl (aclTestMain)
import Glean.Regression.Snapshot.Driver
import qualified Glean.Clang.Test as Clang

main :: IO ()
main = aclTestMain aclDriver

aclDriver :: Driver Clang.Options
aclDriver = recursiveDriver
  { driverGroups = take 1 . driverGroups recursiveDriver
  }
  where
  recursiveDriver = Clang.driverWithSourceFiles listDirectoryRecursive
