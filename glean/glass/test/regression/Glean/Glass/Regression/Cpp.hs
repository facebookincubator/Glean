{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.Cpp (main) where

import Test.HUnit

import Derive.Lib (DerivePass(..))
import qualified Glean.Clang.Test.DerivePass as DerivePass
import Glean.Regression.Test (mainTestIndexGeneric)

import Glean.Glass.Types
import Glean.Glass.Regression.Tests

main :: IO ()
main = do
  let driver = DerivePass.driver [DeriveTargetUses, DeriveDeclFamilies]
  mainTestIndexGeneric driver "glass-regression-cpp" $ \_ _ _ _ get ->
    TestList
      [ testDocumentSymbolListX (Path "test.cpp") get
      ]
