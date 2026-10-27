{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.CppSidlXLang (main) where

import Control.Monad ( forM_ )

import qualified Glean
import qualified Glean.Clang.Test.DerivePass as DerivePass
import Glean.Regression.Config ( TestConfig(..) )
import Glean.Regression.Snapshot.Driver ( Driver(..) )

import qualified Derive.Lib as DeriveLib
import Glean.Glass.Regression.Snapshot

main :: IO ()
main = mainGlassSnapshotGeneric testName testPath driver (const [])
  where
    baseDriver = DerivePass.driver DeriveLib.allCxxPasses
    driver = baseDriver { driverCreateDatabase = createDBs }
    -- test-xlang has two C++ DBs, so queries check SIDL refs are
    -- not duplicated.
    createDBs opts backend indexer testConfig = do
      let Glean.Repo _ hash = testRepo testConfig
      forM_ ["test", "test-xlang-target", "test-xlang-source"] $ \name ->
        driverCreateDatabase baseDriver opts backend indexer
          testConfig { testRepo = Glean.Repo name hash }
    testName = "glass-regression-cpp-sidl-xlang"
    testPath = "glean/glass/test/regression/tests/cpp-sidl-xlang"
