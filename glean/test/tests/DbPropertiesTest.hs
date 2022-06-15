{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module DbPropertiesTest (main) where

import Control.Monad
import Data.Default
import qualified Data.HashMap.Strict as HashMap
import Test.HUnit

import TestRunner

import Glean
import Glean.Init
import Glean.Database.Test

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "propertiesTest" propertiesTest
  ]

propertiesTest :: Test
propertiesTest = TestCase $ withTestEnv [] $ \env -> do
  let repo = Repo "dbtest-repo" "f00baa"
      props = HashMap.fromList [("key","value")]
  void $ kickOffDatabase env def
    { kickOff_repo = repo
    , kickOff_fill = Just (KickOffFill_writeHandle "")
    , kickOff_properties = props
    }

  let getDatabase = do
        ListDatabasesResult{..} <- listDatabases env def
        case listDatabasesResult_databases of
          [db] -> return db
          [] -> assertFailure "no database found"
          _ -> assertFailure "found more databases than expected"

  Database{..} <- getDatabase
  assertBool "includes kickOff properties" $
    all (`elem` HashMap.toList database_properties) (HashMap.toList props)

  void $ updateProperties env repo (HashMap.fromList [("key", "newValue")]) []
  Database{..} <- getDatabase
  assertBool "can overwrite values" $
    HashMap.lookup "key" database_properties == Just "newValue"

  void $ updateProperties env repo
    (HashMap.fromList [("newKey", "newValue")]) ["key"]
  Database{..} <- getDatabase
  assertBool "can remove keys" $
    HashMap.lookup "newKey" database_properties == Just "newValue" &&
    not (HashMap.member "key" database_properties)

  Database{..} <- getDatabase
  assertBool "sets glean.schema_version by default" $
    all (`elem` HashMap.keys database_properties) ["glean.schema_version"]
  assertBool "sets glean.schema_id by default" $
    all (`elem` HashMap.keys database_properties) ["glean.schema_id"]
