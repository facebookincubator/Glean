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
      -- Upstream clang 19.1.7 reports these global variables as
      -- non-definitions; the goldens come from platform010-clang-19.
      [ "declarations/var1"
      , "xrefs/var1"
      ]
    path = "glean/lang/codemarkup/tests/clang"
