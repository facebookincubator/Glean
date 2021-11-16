{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module LookupTest (main) where

import Foreign.C.String (CString)
import Test.HUnit

import TestRunner

import Glean.Database.Open (readDatabase)
import Glean.FFI
import Glean.Init
import Glean.RTS.Foreign.Lookup

import TestDB

checkLookupInvariants :: CanLookup a => a -> IO ()
checkLookupInvariants x = withLookup x $ invoke . glean_check_lookup_invariants

foreign import ccall safe glean_check_lookup_invariants
  :: Lookup -> IO CString

invariantsTest :: Test
invariantsTest = dbTestCase $ \env repo ->
    readDatabase env repo $ \_ -> checkLookupInvariants

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "invariants" invariantsTest
  ]
