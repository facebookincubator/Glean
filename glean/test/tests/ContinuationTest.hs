-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE TypeApplications #-}
module ContinuationTest (main) where

import Data.Proxy
import Test.HUnit

import TestRunner

import Glean.Init
import Glean.Query.Thrift
import qualified Glean.Schema.Cxx1.Types as Cxx
import Glean.Types

import Glean.Test.HUnit
import TestDB

-- | Tests for derived predicates using Angle syntax. See
-- JSONQueryTest.jsonDerivedTest for the Thrift-query-type versions of
-- these tests.
continuationCheckTest :: (UserQueryCont -> UserQueryCont) -> Test
continuationCheckTest f = dbTestCase $ \env repo -> do
  -- Several tests with the trivial derived predicate: IsThree ::= 3
  (r, Just cont) <-
    runQueryPage env repo Nothing $ limit 1 $ allFacts @Cxx.Name
  assertEqual "first result" (length r) 1

  -- check that the continuation works
  (r, _) <-
    runQueryPage env repo (Just cont) $ limit 1 $ allFacts @Cxx.Name
  assertEqual "second result" (length r) 1

  -- check that fiddling with the continuation makes it not work
  assertThrowsType "bad cont" (Proxy :: Proxy BadQuery)
      $ runQueryPage env repo (Just $ f cont) $ allFacts @Cxx.Name

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "version" $ continuationCheckTest $ \c -> c
      { userQueryCont_version = 0 }
  , TestLabel "bytes" $ continuationCheckTest $ \c -> c
      { userQueryCont_continuation = "helloworld" }
  ]
