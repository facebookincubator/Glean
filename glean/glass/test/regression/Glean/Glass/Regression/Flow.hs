{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.Flow (main) where

import Test.HUnit

import Glean.Regression.Test

import Glean.Glass.Types
import Glean.Glass.Regression.Tests

main :: IO ()
main = mainTestIndexExternal "glass-regression-flow" $ \get -> TestList
  [ testDocumentSymbolListX (Path "test/imports.js") get
  ]
