{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module TestDB (
  WithDB,
  withTestDB, withWritableTestDB, withStackedTestDB,
  dbTestCase, dbTestCaseWritable, dbTestCaseSettings, createTestDB,
  withDbTests,
) where

import Data.Default
import Data.Either
import Foreign.Marshal.Utils
import Test.HUnit

import Util.IO

import Glean.Database.Test
import Glean.Database.Types
import Glean.Backend.Types as Backend
import Glean.Typed
import Glean.Schema.Resolve
import qualified Glean.Types as Thrift

import qualified Glean.Schema.Cxx1 as Cxx
import qualified Glean.Schema.GleanTest as Glean.Test
import qualified Glean.Schema.Sys as Sys

import TestData

-- | An action that runs on a test DB
type WithDB a = Env -> Thrift.Repo -> IO a

afterComplete :: WithDB a -> WithDB a
afterComplete action env repo = do
  completeTestDB env repo
  action env repo

withTestDB :: [Setting] -> WithDB a -> IO a
withTestDB settings = withWritableTestDB settings . afterComplete

createTestDB :: WithDB ()
createTestDB env repo = do
  kickOffTestDB env repo id
  writeTestDB env repo testFacts

withWritableTestDB :: [Setting] -> WithDB a -> IO a
withWritableTestDB settings action = withEmptyTestDB settings $ \env repo -> do
  writeTestDB env repo testFacts
  action env repo

withStackedTestDB :: [Setting] -> WithDB a -> IO a
withStackedTestDB settings action = withTestEnv settings $ \env -> do
  kickOffTestDB env repo1 id
  writeTestDB env repo1 testFacts1
  completeTestDB env repo1
  kickOffTestDB env repo2 $ \x -> x
    { Thrift.kickOff_dependencies = Just $ stacked repo1 }
  writeTestDB env repo2 testFacts2
  action env repo2
  where
    stacked (Thrift.Repo name hash) =
      Thrift.Dependencies_stacked $ Thrift.Stacked name hash Nothing
    repo1 = Thrift.Repo "dbtest-repo" "1"
    repo2 = Thrift.Repo "dbtest-repo" "2"

newtype RunTest = RunTest (forall a . WithDB a -> IO a)

toTestCases :: [(String, RunTest)] -> WithDB () -> Test
toTestCases testCases action = TestList
  [ TestLabel label $ TestCase $ with action
    | (label, RunTest with) <- testCases
  ]

dbFlavours :: [Setting] -> [(String, RunTest)]
dbFlavours testCaseSettings =
  [ (label1 ++ label2, run)
    | (label2, settings) <- allStorage
    , let allSettings = settings <> testCaseSettings
    , (label1, run) <-
        [ ("", RunTest (withWritableTestDB allSettings))
        , ("stacked/", RunTest (withStackedTestDB allSettings)) ]
  ]

-- | Run a test on several flavour of test DB. For a test suite
-- with multiple tests, use 'withDbTests' instead.
dbTestCase :: WithDB () -> Test
dbTestCase = toTestCases (dbFlavours []) . afterComplete

-- | Like dbTestCase, but the test DBs are shared between multiple
-- tests, rather than being created afresh for each test. Useful
-- for speeding up test suites that have many individual tests.
withDbTests :: ((WithDB () -> Test) -> IO a) -> IO a
withDbTests fn =
  withMany lazify (dbFlavours []) $ fn . toTestCases
  where
  lazify :: (String, RunTest) -> ((String, RunTest) -> IO a) -> IO a
  lazify (label, RunTest run) fn =
    withLazy (run . afterComplete . curry) $ \get ->
      fn (label, RunTest (\with -> do (env, repo) <- get; with env repo))

dbTestCaseWritable :: WithDB () -> Test
dbTestCaseWritable = toTestCases (dbFlavours [])

dbTestCaseSettings :: [Setting] -> WithDB () -> Test
dbTestCaseSettings settings action =
  toTestCases (dbFlavours settings) (afterComplete action)

writeTestDB :: Env -> Thrift.Repo -> (forall m. NewFact m => m ()) -> IO ()
writeTestDB env repo facts = do
  backend_schema <- parseAndResolveSchema . Thrift.schemaInfo_schema <$>
    Backend.getSchemaInfo env (Just repo) def
  assertBool "schema1" (isRight backend_schema)

  let allPredicates =
        [ Cxx.allPredicates
        , Glean.Test.allPredicates
        , Sys.allPredicates
        ]
  writeFactsIntoDB env repo allPredicates facts
