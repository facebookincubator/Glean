{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Angle.NestedTest (main) where

import Test.HUnit
import TestRunner
import Util.String.Quasi
import Glean.Init
import Glean.Query.Thrift as Thrift
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.GleanTest.Types as Glean.Test
import TestDB

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "nested" $ angleNested id
  , TestLabel "nested/page" $ angleNested (limit 1)
  ]

-- nested patterns
angleNested :: (forall a . Query a -> Query a) -> Test
angleNested modify = dbTestCase $ \env repo -> do
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.Predicate $
    [s|
      glean.test.Predicate { pred = "hello" }
    |]
  assertEqual "angle - nested 0" 2 (length results)

  results <- runQuery_ env repo $ modify $ angle @Cxx.FunctionName
    [s|
      cxx1.FunctionName { name = cxx1.Name "an".. }
    |]
  print results
  assertEqual "angle - nested 1" 2 (length results)

  -- the predicate name is optional, since the type tells us what it is:
  results <- runQuery_ env repo $ modify $ angle @Cxx.FunctionName
    [s|
      cxx1.FunctionName { name = "an".. }
    |]
  print results
  assertEqual "angle - nested 2" 2 (length results)

  results <- runQuery_ env repo $ modify $ angle @Glean.Test.DualStringPair
    [s|
      glean.test.DualStringPair {{"a",_},_}
    |]
  print results
  assertEqual "angle - nested 4" 1 (length results)

  results <- runQuery_ env repo $ modify $ angle @Glean.Test.RevStringPairs
    [s|
      glean.test.RevStringPairs { x = "a", r = {_,"a"} }
    |]
  -- this test is on slightly shaky ground, because it generates
  -- derived RevStringPair facts in two places (nested in the query,
  -- and again in the body of RevStringPairs), which get de-duped by
  -- the fact Environment in the query. But paging the query breaks
  -- de-duping because we use a new Environment each time we restart
  -- the query, so we can get a different number of results when
  -- executing this query paged vs. unpaged. To see this, remove
  -- the x = "a" part of the pattern above. (TODO)
  print results
  assertEqual "angle - nested 5" 1 (length results)

  -- or-patterns
  results <- runQuery_ env repo $ modify $ angle @Glean.Test.StringPair
    [s|
      glean.test.StringPair {"a" | "b", _}
    |]
  print results
  assertEqual "angle - nested 6" 2 (length results)

  -- nested generator in an expression
  r <- runQuery_ env repo $ angle @Cxx.Name
    [s|
      (cxx1.Name "foo" where _ = "")
    |]
  print r
  assertEqual "angle - nested predicate pattern in expression" 1 (length r)

  -- nested generator with a type signature
  results <- runQuery_ env repo $ modify $ angle @Cxx.FunctionName
    [s|
      cxx1.FunctionName { name = "an".. : cxx1.Name }
    |]
  print results
  assertEqual "angle - nested type sig" 2 (length results)

  -- we can force a variable to match the key, not the fact ID
  results <- runQuery_ env repo $ modify $ angle @Cxx.Name
    [s|
      cxx1.FunctionName { name = X : string };
      cxx1.Name X
    |]
  print results
  assertEqual "angle - nested type sig 2" 11 (length results)
