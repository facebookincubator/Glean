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
import TestDB

-- | Test that after failing to open a DB we report the DB as broken
-- in the result of listDatabases
dbOpenFail :: Test
dbOpenFail = TestCase $ do
  withSystemTempDirectory "glean-dbtest" $ \root -> do
    repo <- withTestDB [setRoot root] $ \_ repo -> return repo
    dbs <- withTestEnv [setRoot root] $ \env -> listDatabases env def
    assertEqual "exists" [DatabaseStatus_Complete]
      (map database_status (listDatabasesResult_databases dbs))
    -- now delete the DB contents so it will fail to open
    withCurrentDirectory (root </> Text.unpack (repo_name repo) </>
        Text.unpack (repo_hash repo) </> "db") $
      listDirectory "." >>= mapM_ removeFile
    withTestEnv [setRoot root] $ \env -> do
      r <- try $ predicateStats env repo ExcludeBase
      print r
      assertBool "open failed" $ case r of
        Left (e :: SomeException) -> "No such file" `isInfixOf` show e
        _otherwise -> False
      dbs <- listDatabases env def
      assertEqual "broken" [DatabaseStatus_Broken]
        (map database_status (listDatabasesResult_databases dbs))

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "dbOpenFail" dbOpenFail
  ]
