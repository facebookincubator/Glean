{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Run a snapshot test with an external indexer

module Glean.Regression.Driver.External (main) where

import Glean.Regression.Snapshot
import Glean.Regression.Snapshot.Driver

main :: IO ()
main = testMain externalDriver
