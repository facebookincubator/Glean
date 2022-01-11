{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Regression testing, see "Glean.Regression.Test" for Derive.hs
module Glean.Clang.Test.DerivePass (testDeriver, indexer) where

import Control.Monad

import qualified Glean.Clang.Test as Clang
import Glean.Regression.Config
import Glean.Regression.Options
import Glean.Regression.Indexer
import Glean.Regression.Test

import Derive.Env (withEnv)
import Derive.Lib (dispatchDerive, DerivePass, allPredicates)
import Derive.Types (testConfig)

-- | Run the Clang indexer followed by the specified deriving passes
indexer :: [DerivePass] -> Indexer Clang.Options
indexer passesIn = Clang.indexer `indexerThen` derive
  where
  derive test backend =
    forM_ passesIn $ \thisPass ->
      -- withTestWriter completes before next pass
      withEnv (testConfig (testRepo test)) allPredicates backend $ \env ->
        dispatchDerive env thisPass

testDeriver :: [DerivePass] -> IO ()
testDeriver passes = testMain (Clang.driver { driverIndexer = indexer passes })
