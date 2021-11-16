{-
  Copyright (c) Facebook, Inc. and its affiliates.
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
  ListDatabasesResult{..} <- listDatabases env def
  assertBool "propertiesTest" $ case listDatabasesResult_databases of
    [Database{..}] ->
      all (`elem` HashMap.toList database_properties) (HashMap.toList props)
    _ -> False

  void $ updateProperties env repo (HashMap.fromList [("key", "newValue")]) []
  ListDatabasesResult{..} <- listDatabases env def
  assertBool "propertiesTest2" $ case listDatabasesResult_databases of
    [Database{..}] ->
      HashMap.lookup "key" database_properties == Just "newValue"
    _ -> False

  void $ updateProperties env repo
    (HashMap.fromList [("newKey", "newValue")]) ["key"]
  ListDatabasesResult{..} <- listDatabases env def
  assertBool "propertiesTest3" $ case listDatabasesResult_databases of
    [Database{..}] ->
      HashMap.lookup "newKey" database_properties == Just "newValue" &&
      not (HashMap.member "key" database_properties)
    _ -> False
