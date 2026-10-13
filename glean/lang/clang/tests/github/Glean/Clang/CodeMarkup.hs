{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Clang.CodeMarkup where

import System.Environment

import qualified Glean.Regression.Driver.DeriveForCodemarkup as D

main :: IO ()
main = getArgs >>= \args ->
  withArgs (args ++ extraArgs) D.main
  where
    extraArgs = [
      "--root", path,
      "--omit", "declarations/typeAlias1"
        -- Bug fixed upstream in LLVM:
        --  https://github.com/llvm/llvm-project/commit/d9c979ef33ff83b49ba14c2eecdf499bae4565f4
        -- TODO: re-enable when using a fixed LLVM
      ] ++ concatMap (\test -> ["--omit", test])
      -- golden outputs are from Clang 17, which differ from 15
      [ "declarations/var1"
      , "xrefs/template-member-var1"
      , "xrefs/template3"
      , "xrefs/using-decl3"
      , "xrefs/using-decl4"
      , "xrefs/using-decl6"
      , "xrefs/using-directive6"
      , "xrefs/var1"
      ]
    path = "glean/lang/codemarkup/tests/clang"
