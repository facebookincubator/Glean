{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Driver
  ( main
  ) where

import Derive.Env (withEnv)
import Derive.HackDeclarationTarget (deriveHackDeclarationTarget)
import qualified Derive.Types as DT
import Glean.Regression.Indexer
import Glean.Regression.Indexer.External
import Glean.Regression.Snapshot (testMain)
import Glean.Regression.Snapshot.Driver

indexer :: Indexer Ext
indexer =
  externalIndexer `indexerThen` \env repo _params ->
    withEnv (DT.defaultConfig repo) env
      deriveHackDeclarationTarget

main :: IO ()
main = testMain (externalDriver { driverIndexer = indexer })
