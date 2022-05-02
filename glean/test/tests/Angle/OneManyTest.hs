{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Angle.OneManyTest (main) where

import Data.Default
import Test.HUnit

import TestRunner
import Util.String.Quasi

import Glean.Init
import Glean.Query.Thrift as Thrift
import qualified Glean.Schema.GleanTest.Types as Glean.Test

import TestData
import TestDB

ignorePredK :: Glean.Test.KitchenSink_1 -> Glean.Test.KitchenSink_1
ignorePredK k = k { Glean.Test.kitchenSink_1_pred = def }

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "one" $ dbTestCase $ \env repo -> do
    -- match one result of many
    results <- runQuery_ env repo $ angle @Glean.Test.Predicate_1
      [s|
         glean.test.Predicate.1 { named_sum_ = { tue = 37 } }
      |]
    case results of
      [Glean.Test.Predicate_1{Glean.Test.predicate_1_key = Just k}] -> do
        print k
        print kitchenSink1
        assertBool "angle - glean.test.Predicate 1" $
          ignorePredK k == ignorePredK kitchenSink1
      _ -> do
        print results
        putStrLn "other branch"
        assertBool "angle - glean.test.Predicate 1" False
  ]
