{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
module CppCatchTest (main) where

import Control.Exception
import qualified Data.Text as Text
import Test.HUnit

import TestRunner

import Glean.Backend as Backend
import Glean.Init
import Glean.Query.Thrift as Thrift
import Glean.Query.Thrift.Internal
import qualified Glean.Schema.Cxx1.Types as Cxx
import Glean.Typed hiding (end)
import Glean.Types

import TestData
import TestDB

catchTest :: Test
catchTest = dbTestCase $ \env repo -> do

  -- Pick some fact ids of type Cxx.Name
  names <- runQuery_ env repo $ allFacts @Cxx.Name
  let factId x = Text.pack (show (fromFid (idOf (getId x))))

  -- Now query for it but assert the wrong type. Show raise
  r <- try $ runQuery_ env repo $ angle @Cxx.Type $
      "$cxx1.Type " <> factId (head names)
  print (r :: Either SomeException [Cxx.Type])
    -- should be caught and have a nice trace

{-
Correct trace looks like:

Left fact has the wrong type
Stack trace:
  facebook::glean::rts::raiseError(std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > const&)
  std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > facebook::glean::rts::error<>(folly::Range<char const*>)
  facebook::glean::rts::Subroutine::execute(unsigned long const*) const
-}

main :: IO ()
main = withUnitTest $ testRunner $
  TestLabel "catch from C++" catchTest
