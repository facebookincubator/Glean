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
import Control.Concurrent

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

  tryAgain 3 "includes kickOff properties" $ do
    ListDatabasesResult{..} <- listDatabases env def
    return $ case listDatabasesResult_databases of
      [Database{..}] ->
        all (`elem` HashMap.toList database_properties) (HashMap.toList props)
      _ -> False

  void $ updateProperties env repo (HashMap.fromList [("key", "newValue")]) []
  ListDatabasesResult{..} <- listDatabases env def
  assertBool "can overwrite values" $
    case listDatabasesResult_databases of
      [Database{..}] ->
        HashMap.lookup "key" database_properties == Just "newValue"
      _ -> False

  void $ updateProperties env repo
    (HashMap.fromList [("newKey", "newValue")]) ["key"]
  ListDatabasesResult{..} <- listDatabases env def
  assertBool "can remove keys" $
    case listDatabasesResult_databases of
      [Database{..}] ->
        HashMap.lookup "newKey" database_properties == Just "newValue" &&
        not (HashMap.member "key" database_properties)
      _ -> False

  ListDatabasesResult{..} <- listDatabases env def
  assertBool "sets glean.schema_version by default" $
    case listDatabasesResult_databases of
      [Database{..}] ->
        all (`elem` HashMap.keys database_properties) ["glean.schema_version"]
      _ -> False

tryAgain :: Int -> String -> IO Bool -> IO ()
tryAgain n str io = loop n
  where
    loop 0 = assertFailure str
    loop i = do
      good <- io
      when (not good) $ do
        putStrLn "WARNING: test failed, retrying with delay..."
        threadDelay (10^6)
        loop (i-1)