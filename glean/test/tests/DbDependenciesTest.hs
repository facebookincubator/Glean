{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module DbDependenciesTest (main) where

import Control.Monad
import Data.Default
import qualified Data.HashMap.Strict as HashMap
import Test.HUnit

import TestRunner

import Glean
import Glean.Init
import Glean.Database.Test

repo1 :: Repo
repo1 = Repo "dbtest-repo" "f00001"

repo2 :: Repo
repo2 = Repo "dbtest-repo" "f00002"

dependenciesTest :: Test
dependenciesTest = TestCase $ withTestEnv [] $ \env -> do
  let props = HashMap.fromList [("key","value")]

  void $ kickOffDatabase env def
    { kickOff_repo = repo1
    , kickOff_fill = Just (KickOffFill_writeHandle "")
    , kickOff_properties = props
    }

  void $ completeTestDB env repo1

  void $ kickOffDatabase env def
    { kickOff_repo = repo2
    , kickOff_dependencies = Just $ Dependencies_stacked repo1
    , kickOff_fill = Just (KickOffFill_writeHandle "")
    , kickOff_properties = props
    }
  ListDatabasesResult{..} <- listDatabases env def
  let db = filter (\db -> database_repo db == repo2)
            listDatabasesResult_databases

  assertEqual "dependencyTest" (Just repo1) $ case db of
    [Database{database_dependencies=Just (Dependencies_stacked d)}] -> Just d
    _ -> Nothing

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "dependenciesTest" dependenciesTest ]
