{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Driver
  ( main
  ) where

import Glean.Indexer.HackWithDeriver (indexer)
import Glean.Regression.Snapshot (testMain)
import Glean.Regression.Snapshot.Driver

main :: IO ()
main = testMain (driverFromIndexer indexer)
