{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BackupTestS3 (main) where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.HashMap.Strict as HashMap
import Data.Time (fromGregorian)
import Data.Time.Clock
import System.IO (hClose)
import qualified System.IO.Temp as Temp
import Test.HUnit

import TestRunner

import qualified Glean.Database.Backup.Backend as Backup
import Glean.Database.Backup.S3
import Glean.Database.Meta
import Glean.Init
import Glean.Internal.Types (Completeness (Broken), StorageName (..))
import Glean.ServerConfig.Types as ServerTypes
import Glean.Types as Thrift
import Glean.Util.IO (withTempFileContents)
import Glean.Util.Some

meta :: Meta
meta =
  newMeta
    (StorageName "rocksdb")
    (DBVersion 3)
    (DBTimestamp (UTCTime (fromGregorian 1 1 1) 0) Nothing)
    (Broken (DatabaseBroken "test" "test"))
    (HashMap.fromList [("a", "b")])
    Nothing

testDbRepo :: Repo
testDbRepo = Repo "foo" "123"

testSite :: (MonadIO m) => m (Some Backup.Site)
testSite = do
  backend <- fakeS3Backend
  let Just site = Backup.fromPath backend "s3:testbucket/base/base2"
  pure site

restoresTest :: Test
restoresTest = TestCase $ do
  site <- testSite

  withTempFileContents ("abcd" :: String) $ \path -> do
    void $ Backup.backup site testDbRepo meta Nothing path

  Temp.withSystemTempFile "glean" $ \path h -> do
    hClose h
    meta' <- Backup.restore site testDbRepo path
    assertEqual "restored database meta is the same" meta' meta
    content <- readFile path
    assertEqual "restored database content is the same" content "abcd"

metadatasTest :: Test
metadatasTest = TestCase $ do
  site <- testSite

  dbs <- Backup.enumerate site
  assertEqual "no databases" dbs []

  withTempFileContents ("abcd" :: String) $ \path -> do
    void $ Backup.backup site testDbRepo meta Nothing path

  db <- Backup.inspect site testDbRepo
  assertEqual "database inspectable" db meta

  dbs' <- Backup.enumerate site
  assertEqual "databases exists after" dbs' [(testDbRepo, meta)]

  Backup.delete site testDbRepo

  dbs'' <- Backup.enumerate site
  assertEqual "no databases after deleting it" dbs'' []

main :: IO ()
main =
  withUnitTest $
    testRunner $
      TestList
        [ TestLabel "restores" restoresTest
        , TestLabel "metadata" metadatasTest
        ]
