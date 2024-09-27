{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Angle.StoredTest (main) where

import Test.HUnit

import TestRunner
import Util.String.Quasi

import Glean.Init
import Glean.Query.Thrift as Thrift
import qualified Glean.Schema.GleanTest.Types as Glean.Test

import TestDB

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "stored" $ angleStored id
  , TestLabel "stored/page" $ angleStored (limit 1)
  ]

-- Test storing derived facts
angleStored :: (forall a . Query a -> Query a) -> Test
angleStored modify = dbTestCaseWritable $ \env repo -> do
  -- initially zero facts
  results <- runQuery_ env repo $ modify $
    angle @Glean.Test.StoredRevStringPair $
    [s|
      glean.test.StoredRevStringPair _
    |]
  assertEqual "angle - stored 0" 0 (length results)

  -- still zero facts since StoredRevStringPair is not derived yet
  results <- runQuery_ env repo $ modify $ store $
    angle @Glean.Test.StoredRevStringPairWithA $
    [s|
      glean.test.StoredRevStringPairWithA _
    |]
  assertEqual "angle - stored 1" 0 (length results)

  -- no facts derived due to the above
  results <- runQuery_ env repo $ modify $
    angle @Glean.Test.StoredRevStringPair $
    [s|
      glean.test.StoredRevStringPair _
    |]
  assertEqual "angle - stored 2" 0 (length results)

  -- compute and store, returns 6 facts
  results <- runQuery_ env repo $ modify $ store $
    angle @Glean.Test.StoredRevStringPair $
    [s|
      glean.test.StoredRevStringPair _
    |]
  assertEqual "angle - stored 3" 6 (length results)

  -- now there should be 6 facts stored in the DB
  results <- runQuery_ env repo $ modify $
    angle @Glean.Test.StoredRevStringPair $
    [s|
      glean.test.StoredRevStringPair _
    |]
  assertEqual "angle - stored 4" 6 (length results)

  -- StoredRevStringPair is now derived
  results <- runQuery_ env repo $ modify $ store $
    angle @Glean.Test.StoredRevStringPairWithA $
    [s|
      glean.test.StoredRevStringPairWithA _
    |]
  assertEqual "angle - stored 5" 1 (length results)

  results <- runQuery_ env repo $ modify $
    angle @Glean.Test.StoredRevStringPairWithA $
    [s|
      glean.test.StoredRevStringPairWithA _
    |]
  assertEqual "angle - stored 6" 1 (length results)

  results <- runQuery_ env repo $ modify $ store $
    angle @Glean.Test.EdgeFromNotA $
    [s|
      glean.test.EdgeFromNotA _
    |]
  assertEqual "angle - stored 7" 2 (length results)

  results <- runQuery_ env repo $ modify $
    angle @Glean.Test.EdgeFromNotA $
    [s|
      glean.test.EdgeFromNotA _
    |]
  assertEqual "angle - stored 7" 2 (length results)
