{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module TestRunner (module TestRunner) where

import Control.Monad
import System.Exit
import Test.HUnit

testRunner :: Test -> IO ()
testRunner t = do
  Counts{..} <- runTestTT t
  when (errors + failures > 0) $ exitWith (ExitFailure 1)
