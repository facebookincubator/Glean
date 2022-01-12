{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
module TimeoutTest (main) where

import Test.HUnit

import TestRunner
import Util.String.Quasi

import Glean.Init
import Glean.Query.Thrift
import qualified Glean.Schema.GleanTest.Types as Glean.Test

import BenchDB

timeoutTest :: Test
timeoutTest = TestCase $ withBenchDB 10000 $ \env repo -> do
  -- The test spends most of its time building the DB (withBenchDB above),
  -- so to keep things reasonable in debug mode we limit the number of
  -- facts to 10K and set the time limit to 1ms which is as low as we
  -- can go. This query hits the time budget ~25 times on my devserver
  -- (smarlow 2020-04-14).
  r <- runQuery_ env repo $ limitTime 1 $ angle
    [s| glean.test.Predicate { string_ = "x1" } |]
  assertEqual "angle - timeout Test" 1 (length (r :: [Glean.Test.Predicate]))

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "timeout" timeoutTest
  ]
