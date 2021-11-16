{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module TestShard (main) where

import Test.HUnit

import TestRunner

import Glean.Backend.Remote
import Glean.Types

main :: IO ()
main = testRunner $ TestList
  [ TestLabel "testShard" $ TestCase $
      assertEqual "shard"
        (dbShard (Repo "test" "1234"))
        "6825422262439797754"
  ]
