{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.Flow.Main (main) where

import Glean.Indexer.Flow as Flow ( indexer )
import Glean.Glass.Regression.Snapshot ( mainGlassSnapshotCLI )

main :: IO ()
main = mainGlassSnapshotCLI Flow.indexer (const [])
