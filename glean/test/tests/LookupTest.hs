-- Copyright (c) Facebook, Inc. and its affiliates.

module LookupTest (main) where

import Foreign.C.String (CString)
import Test.HUnit

import TestRunner

import Glean.Database.Stuff (readDatabase)
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
