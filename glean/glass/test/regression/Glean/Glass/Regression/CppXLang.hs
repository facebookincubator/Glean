{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.CppXLang (main) where

import qualified Glean.Clang.Test.DerivePass as DerivePass

import qualified Glean.Regression.Driver.DeriveForCodemarkup as Code
import Glean.Indexer.Fbthrift as Fbthrift (indexer)
import Glean.Glass.Regression.Snapshot (mainGlassSnapshotXLang)

main :: IO ()
main = mainGlassSnapshotXLang
  testName
    testPath (d, "test-xlang-source") (Fbthrift.indexer, "test-xlang-target")
  where
    d = DerivePass.driver Code.codemarkupDerivePasses
    testName = "glass-regression-cpp-xlang"
    testPath = "glean/glass/test/regression/tests/cpp-xlang"
