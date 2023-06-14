{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module DbOpenFail
  ( main
  ) where

import Control.Exception.Safe
import Data.Default
import Data.List
import qualified Data.Text as Text
import System.Directory
import System.FilePath
import System.IO.Temp
import Test.HUnit

import TestRunner

import Glean
import Glean.Database.Test
import Glean.Init

-- | Test that after failing to open a DB we report the DB as broken
-- in the result of listDatabases
dbOpenFail :: Test
dbOpenFail = TestCase $ do
  withSystemTempDirectory "glean-dbtest" $ \root -> do
    let base = Repo "dbfail-base" "1"
        stacked = Repo "dbfail-stacked" "1"
    withTestEnv [setRoot root] $ \env -> do
      kickOffTestDB env base id
      completeTestDB env base
      kickOffTestDB env stacked $ \x -> x
        { kickOff_dependencies = Just $ Dependencies_stacked $
            Stacked (repo_name base) (repo_hash base) Nothing }
      completeTestDB env stacked
    dbs <- withTestEnv [setRoot root] $ \env -> listDatabases env def
    assertEqual "exists" [DatabaseStatus_Complete, DatabaseStatus_Complete]
      (map database_status (listDatabasesResult_databases dbs))
    -- now delete the DB contents for the base DB so it will fail to open
    withCurrentDirectory (root </> Text.unpack (repo_name base) </>
        Text.unpack (repo_hash base) </> "db") $
      listDirectory "." >>= mapM_ removeFile
    withTestEnv [setRoot root] $ \env -> do
      r <- try $ predicateStats env base ExcludeBase
      print r
      assertBool "open failed" $ case r of
        Left (e :: SomeException) -> "No such file" `isInfixOf` show e
        _otherwise -> False
      dbs <- listDatabases env def
      -- stacked DB should be broken too
      assertEqual "broken" [DatabaseStatus_Broken, DatabaseStatus_Broken]
        (map database_status (listDatabasesResult_databases dbs))

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "dbOpenFail" dbOpenFail
  ]
