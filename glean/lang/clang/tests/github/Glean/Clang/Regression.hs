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
      , "xrefs/using-directive4"
      , "index_failure/cpp"
      ]
