{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Angle.StringTest (main) where

import Test.HUnit
import TestRunner
import Util.String.Quasi

import Glean.Init
import Glean.Query.Thrift as Thrift
import qualified Glean.Schema.GleanTest.Types as Glean.Test

import TestDB

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "string" $ angleString id
  ]

angleString :: (forall a . Query a -> Query a) -> Test
angleString modify = TestList
  [ TestLabel "reverse" $ stringReverse modify
  ]


stringReverse :: (forall a . Query a -> Query a) -> Test
stringReverse modify = dbTestCase $ \env repo -> do
  -- fetch all elements of an array
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate
    [s|
        glean.test.Predicate {
            string_ = X
        } where
        X = prim.reverse (prim.reverse X)
    |]
  print results
  assertEqual "angle - array generator 1" 4 (length results)
