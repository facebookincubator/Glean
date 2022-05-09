{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Angle.DataTest (main) where

import Data.List
import Test.HUnit

import TestRunner
import Util.String.Quasi
import Data.Text ( Text )

import Glean.Init
import Glean.Query.Thrift as Thrift
import Glean.Types

import TestDB

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "angleData" $ angleDataTest id
  , TestLabel "angleData/page" $ angleDataTest (limit 1)
  ]

angleDataTest :: (forall a . Query a -> Query a) -> Test
angleDataTest modify = dbTestCase $ \env repo -> do
  results <- runQuery_ env repo $ modify $ angleData
    [s| { 3, false } : { x : nat, y : bool } |]
  assertEqual "angleData 1" [(Nat 3, False)] results

  results :: [Text] <- runQuery_ env repo $ modify $ angleData
    [s| X where Y = cxx1.Name "ab"..; Y = cxx1.Name X |]
  assertEqual "angleData 2" ["abba", "abcd"] (sort results)
