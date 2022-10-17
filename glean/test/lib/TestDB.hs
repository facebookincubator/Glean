{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
module TestDB (
  withTestDB, withWritableTestDB, dbTestCase, dbTestCaseWritable, createTestDB
) where

import Data.Default
import Data.Either
import Test.HUnit

import Glean.Database.Storage (DBVersion(..), currentVersion, writableVersions)
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

afterComplete :: (Env -> Thrift.Repo -> IO a) -> Env -> Thrift.Repo -> IO a
afterComplete action env repo = do
  completeTestDB env repo
  action env repo

withTestDB :: [Setting] -> (Env -> Thrift.Repo -> IO a) -> IO a
withTestDB settings = withWritableTestDB settings . afterComplete

createTestDB :: Env -> Thrift.Repo -> IO ()
createTestDB env repo = do
  kickOffTestDB env repo id
  writeTestDB env repo testFacts

withWritableTestDB :: [Setting] -> (Env -> Thrift.Repo -> IO a) -> IO a
withWritableTestDB settings action = withEmptyTestDB settings $ \env repo -> do
  writeTestDB env repo testFacts
  action env repo

withStackedTestDB :: [Setting] -> (Env -> Thrift.Repo -> IO a) -> IO a
withStackedTestDB settings action = withTestEnv settings $ \env -> do
  kickOffTestDB env repo1 id
  writeTestDB env repo1 testFacts1
  completeTestDB env repo1
  kickOffTestDB env repo2 $ \x -> x
    { Thrift.kickOff_dependencies = Just $ Thrift.Dependencies_stacked repo1 }
  writeTestDB env repo2 testFacts2
  action env repo2
  where
    repo1 = Thrift.Repo "dbtest-repo" "1"
    repo2 = Thrift.Repo "dbtest-repo" "2"

testCases :: (Env -> Thrift.Repo -> IO ()) -> Test
testCases action = TestList
  [ TestLabel (label1 ++ label2) $ TestCase $ with settings action
    | (label1, with) <-
        [ ("", withWritableTestDB)
        , ("stacked/", withStackedTestDB) ]
    , (label2, settings) <-
        [ ("memory", [setMemoryStorage])
        , ("rocksdb", [])
        ]
        ++
        [ ("rocksdb-" ++ show (unDBVersion v), [setDBVersion v])
          | v <- writableVersions, v /= currentVersion ]
  ]

dbTestCase :: (Env -> Thrift.Repo -> IO ()) -> Test
dbTestCase = testCases . afterComplete

dbTestCaseWritable :: (Env -> Thrift.Repo -> IO ()) -> Test
dbTestCaseWritable = testCases

writeTestDB :: Env -> Thrift.Repo -> (forall m. NewFact m => m ()) -> IO ()
writeTestDB env repo facts = do
  backend_schema <- parseAndResolveSchema . Thrift.schemaInfo_schema <$>
    Backend.getSchemaInfo env repo def
  assertBool "schema1" (isRight backend_schema)

  let allPredicates =
        [ Cxx.allPredicates
        , Glean.Test.allPredicates
        , Sys.allPredicates
        ]
  writeFactsIntoDB env repo allPredicates facts
