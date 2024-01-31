{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

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

-- | Tests for derived predicates using Angle syntax.
continuationCheckTest :: (UserQueryCont -> UserQueryCont) -> Test
continuationCheckTest f = dbTestCase $ \env repo -> do
  -- Several tests with the trivial derived predicate: IsThree ::= 3
  (io, Just cont) <-
    runQueryPage env repo Nothing $ limit 1 $ allFacts @Cxx.Name
  r <- io
  assertEqual "first result" (length r) 1

  -- check that the continuation works
  (io, _) <-
    runQueryPage env repo (Just cont) $ limit 1 $ allFacts @Cxx.Name
  r <- io
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
