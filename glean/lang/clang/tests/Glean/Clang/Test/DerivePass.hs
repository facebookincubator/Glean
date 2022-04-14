{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Regression testing, see "Glean.Regression.Test" for Derive.hs
module Glean.Clang.Test.DerivePass (testDeriver, driver) where

import Control.Monad

import qualified Glean.Clang.Test as Clang
import Glean.Regression.Indexer
import Glean.Regression.Snapshot.Driver
import Glean.Regression.Snapshot

import Derive.Env (withEnv)
import Derive.Lib (dispatchDerive, DerivePass, allPredicates)
import Derive.Types (testConfig)

-- | Run the Clang indexer followed by the specified deriving passes
driver :: [DerivePass] -> Driver Clang.Options
driver passes = Clang.driver { driverIndexer = indexer }
  where
  indexer = driverIndexer Clang.driver `indexerThen` derive

  derive backend repo _params =
    forM_ passes $ \thisPass ->
      -- withTestWriter completes before next pass
      withEnv (testConfig repo) allPredicates backend $ \env ->
        dispatchDerive env thisPass

testDeriver :: [DerivePass] -> IO ()
testDeriver passes = testMain (driver passes)
