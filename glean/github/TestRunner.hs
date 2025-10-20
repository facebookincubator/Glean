{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module TestRunner (module TestRunner) where

import Control.Exception
import System.Environment (withArgs)
import Test.HUnit
import qualified Test.Tasty as Tasty
import qualified Test.HUnit.Base  as HUB
import qualified Test.Tasty.HUnit as TFH
import Test.HUnit.Lang as HUnitInternals (HUnitFailure(..), formatFailureReason)

-- | Wraps the HUnit test using tasty, so we get parallel test runs,
-- test listing, and test selection.
testRunner :: Test -> IO ()
testRunner t =
  Tasty.defaultMain $
  Tasty.testGroup "test" $
  hUnitTestToTestTree t

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
  hUnitTestToTestTree t

-- | Convert existing HUnit test to a TestTree list that can be used
-- with tasty.  This is modified from the tasty-hunit-adapter package,
-- adding conversion of the original HUnitFailure exception into
-- Tasty's version, so that we get better diagnostics.
hUnitTestToTestTree :: HUB.Test -> [Tasty.TestTree]
hUnitTestToTestTree = go ""
  where
    go desc (HUB.TestCase a)    = [TFH.testCase desc (rethrow a)]
    go desc (HUB.TestLabel s t)
      | null desc = go s t
      | otherwise = go (desc ++ ":" ++ s) t
    go desc (HUB.TestList ts)
        -- If the list occurs at the top level (with no description above it),
        -- just return that list straightforwardly
      | null desc = concatMap (go "") ts
        -- If the list occurs with a description, turn that into a honest-to-god
        -- test group. This is heuristic, but likely to give good results
      | otherwise = [Tasty.testGroup desc (concatMap (go "") ts)]

    rethrow t = --recomp
      t `catch` \(HUnitInternals.HUnitFailure loc reason) ->
        throwIO $
          TFH.HUnitFailure loc (HUnitInternals.formatFailureReason reason)
