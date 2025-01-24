{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Regression testing, see "Glean.Regression.Test" for Derive.hs
module Glean.Clang.Test.DerivePass (testDeriver, driver, derivePasses) where

import Control.Monad
import Data.Default

import Glean
import Glean.Indexer
import qualified Glean.Clang.Test as Clang
import Glean.Regression.Snapshot.Driver
import Glean.Regression.Snapshot

import Derive.Env (withEnv)
import Derive.Lib (dispatchDerive, DerivePass, allPredicates)
import Derive.Types (testConfig, Config(..))

-- | Run the Clang indexer followed by the specified deriving passes
driver :: [DerivePass] -> Driver Clang.Options
driver passes = Clang.driver { driverIndexer = indexer }
  where
  indexer = driverIndexer Clang.driver `indexerThen` derivePasses passes

derivePasses
  :: Backend backend => [DerivePass] -> Clang.Options -> backend -> Repo -> p -> IO ()
derivePasses passes opts backend repo _params = do
    completePredicates backend repo (CompletePredicates_axiom def)
    forM_ passes $ \thisPass -> do
      -- withTestWriter completes before next pass
      let conf = (testConfig repo) { cfgIncremental = Clang.clangIncremental opts }
      withEnv conf allPredicates backend $ \env ->
        dispatchDerive env thisPass

testDeriver :: [DerivePass] -> IO ()
testDeriver passes = testMain (driver passes)
