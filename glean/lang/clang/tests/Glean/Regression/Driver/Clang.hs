{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Regression.Driver.Clang (main) where

import System.Environment

import qualified Glean.Clang.Test as Clang
import Glean.Regression.Snapshot

main :: IO ()
main = getArgs >>= \args -> withArgs (args ++ ["--root", path]) $
    testMain Clang.driver
  where
    path = "glean/lang/clang/tests/regression"
