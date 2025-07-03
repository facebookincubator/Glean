{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module TestRunner (module TestRunner) where

import System.Environment (withArgs)
import Test.HUnit
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit.Adapter as Tasty

-- | Wraps the HUnit test using tasty, so we get parallel test runs,
-- test listing, and test selection.
testRunner :: Test -> IO ()
testRunner t =
  Tasty.defaultMain $
  Tasty.testGroup "test" $
  Tasty.hUnitTestToTestTree t

data TestAction = TestAction

-- | This entry point is designed for combining the test runner's
-- option parser with the program's own options. Ideally we would use
-- this to combine Tasty's option parser with the program's parser,
-- but Tasty's option parser depends on the `Test`, and we don't have
-- that when parsing options (it might depend on the options), so
-- there's a circular dependency.
--
-- Therefore we run Tasty's CLI parser with empty arguments. We can
-- still use the environment variables to set Tasty options, e.g.
--
-- > TASTY_NUM_THREADS=12 cabal run my-test
--
testRunnerAction :: TestAction -> Test -> IO ()
testRunnerAction _ t =
  withArgs [] $
  Tasty.defaultMain $
  Tasty.testGroup "test" $
  Tasty.hUnitTestToTestTree t
