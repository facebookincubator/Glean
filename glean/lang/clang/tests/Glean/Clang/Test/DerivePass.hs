{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Regression testing, see "Glean.Regression.Test" for Derive.hs
module Glean.Clang.Test.DerivePass (testDeriver, driver) where

import Control.Monad

import qualified Glean.Clang.Test as Clang
import Glean.Init (withUnitTestOptions)
import Glean.Regression.Config
import Glean.Regression.Test

import Derive.Env (withEnv)
import Derive.Lib (dispatchDerive, DerivePass, allPredicates)
import Derive.Types (testConfig)

-- | This runs the 'Glean.Clang.Test.generateBatch' and then
-- the 'Derive.Lib.dispatchDerive' to test the derived pass. This works
-- well for the passes that query the output of the Clang indexer.
--
-- Note: Both ThriftMangleWWW and ThriftManglePy testing are done
-- via "Derive.ThriftMangle.RegressionTest" instead.
driver :: Clang.Options -> [DerivePass] -> Glean.Regression.Config.Driver
driver opts passesIn = (Clang.driver opts)
  { Glean.Regression.Config.driverGenerator = \test backend repo -> do
      _ <- Glean.Regression.Config.driverGenerator (Clang.driver opts)
        test
        backend
        repo
      forM_ passesIn $ \thisPass -> do
        -- withTestWriter completes before next pass
        withEnv (testConfig repo) allPredicates backend $ \env ->
          dispatchDerive env thisPass
  }

testDeriver :: [DerivePass] -> IO ()
testDeriver passes =
  withUnitTestOptions (optionsWith Clang.extOptions) $ \ (mkcfg, ext) -> do
    cfg <- mkcfg
    testAll cfg (driver ext passes)
