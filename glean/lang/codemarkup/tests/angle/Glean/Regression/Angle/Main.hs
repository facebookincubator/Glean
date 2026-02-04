{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Regression.Angle.Main ( main ) where

import System.Environment
import Glean.Indexer.Angle as Angle
import Glean.Regression.Snapshot
import Glean.Regression.Snapshot.Driver

main :: IO ()
main = getArgs >>= \args -> withArgs (args ++["--root", path]) $ testMain driver
  where
      driver = driverFromIndexer Angle.indexer
      path = "glean/lang/codemarkup/tests/angle/cases"
