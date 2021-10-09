-- Copyright (c) Facebook, Inc. and its affiliates.

-- | Regression testing, see "Glean.Regression.Test" for Derive.hs
module Glean.Clang.Test.DerivePass (driver) where

import Control.Monad

import qualified Glean.Clang.Test as Clang
import qualified Glean.Regression.Config (Driver(..))

import Derive.Env (withEnv)
import Derive.Lib (dispatchDerive, DerivePass, allPredicates)
import Derive.Types (testConfig)

-- | This runs the 'Glean.Clang.Test.generateBatch' and then
-- the 'Derive.Lib.dispatchDerive' to test the derived pass. This works
-- well for the passes that query the output of the Clang indexer.
--
-- Note: Both ThriftMangleWWW and ThriftManglePy testing are done
-- via "Derive.ThriftMangle.RegressionTest" instead.
driver :: [DerivePass] -> Glean.Regression.Config.Driver
driver passesIn = Clang.driver
  { Glean.Regression.Config.driverGenerator = \test backend repo -> do
      _ <- Glean.Regression.Config.driverGenerator Clang.driver
        test
        backend
        repo
      forM_ passesIn $ \thisPass -> do
        -- withTestWriter completes before next pass
        withEnv (testConfig repo) allPredicates backend $ \env ->
          dispatchDerive env thisPass
  }
