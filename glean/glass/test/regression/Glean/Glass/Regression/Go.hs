{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.Go (main) where

import Test.HUnit

import Glean.Indexer.Go as Go
import Glean.Regression.Test

import Glean.Glass.Types
import Glean.Glass.Regression.Tests

main :: IO ()
main = mainTestIndex "glass-regression-go" Go.indexer $ \get -> TestList
  [ testDocumentSymbolListX
      (Path "glean/lang/go/tests/cases/xrefs/leaphash.go") get
  ]
