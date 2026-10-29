{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Clang.Regression (main) where

import System.Environment

import qualified Glean.Regression.Driver.Clang as Clang

main :: IO ()
main = getArgs >>= \args ->
  withArgs (args ++ ["--root", path] ++ expectedFailures) Clang.main

  where
    path = "glean/lang/clang/tests/regression"

    -- tests known to fail in opensource environment
    expectedFailures = map ("--omit="++)
      [ "declarations/typeAlias1"
      , "declarations/objc-property1"
      , "xrefs/coro1"             -- Needs Clang 15
      , "xrefs/using-directive4"  -- Needs Clang 15
      , "index_failure/cpp"
        -- Upstream clang 19.1.7 reports these global variables as
        -- non-definitions; the goldens come from platform010-clang-19.
      , "declarations/var1"
      , "record_definitions/class1"
      , "xrefs/var1"
      ]
