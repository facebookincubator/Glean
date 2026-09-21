{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE OverloadedStrings #-}

-- | Data-driven ACL snapshot tests for any Glean indexer.
--
-- Each snapshot case has a @scenario.yaml@ describing the caller's groups and
-- optional incremental pruning. The nearest ancestor @acl.yaml@ supplies the
-- database properties and path ACLs; its directory is the indexer root.
module Glean.Regression.Snapshot.Acl
  ( aclTestMain
  ) where

import Control.Exception (finally)
import Control.Monad (when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.UTF8 as UTF8
import Data.Default (def)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import Data.Text (Text)
import System.Directory (doesFileExist)
import System.Exit (die)
import System.FilePath
import qualified Data.Yaml as Yaml

import qualified Glean
import Glean.Database.Test (Setting, setAclTesting)
import Glean.Indexer (RunIndexer)
import Glean.LocalOrRemote (LocalOrRemote)
import Glean.Regression.Config
import Glean.Regression.Indexer
import Glean.Regression.Snapshot (testMainWithSettings)
import Glean.Regression.Snapshot.Driver
import qualified Glean.Types as Thrift
import Glean.Util.Some (Some)

data RepoConfig = RepoConfig
  { repoProperties :: HashMap.HashMap Text Text
  , repoAcl :: HashMap.HashMap Text [Text]
  }

instance Aeson.FromJSON RepoConfig where
  parseJSON = Aeson.withObject "ACL repository config" $ \value ->
    RepoConfig
      <$> value Aeson..:? "properties" Aeson..!= HashMap.empty
      <*> value Aeson..: "acl"

data PrunedConfig = PrunedConfig
  { prunedUnits :: [FilePath]
  , prunedExclude :: Bool
  }

instance Aeson.FromJSON PrunedConfig where
  parseJSON = Aeson.withObject "ACL pruned database config" $ \value ->
    PrunedConfig
      <$> value Aeson..: "units"
      <*> value Aeson..: "exclude"

data ScenarioConfig = ScenarioConfig
  { scenarioGroups :: [Text]
  , scenarioPruned :: Maybe PrunedConfig
  }

instance Aeson.FromJSON ScenarioConfig where
  parseJSON = Aeson.withObject "ACL test scenario" $ \value ->
    ScenarioConfig
      <$> value Aeson..:? "groups" Aeson..!= []
      <*> value Aeson..:? "pruned"

aclTestMain :: Driver opts -> IO ()
aclTestMain driver =
  testMainWithSettings aclSettings driver
    { driverCreateDatabase = createAclDatabase }

aclSettings :: TestConfig -> IO [Setting]
aclSettings test = do
  scenario <- readScenarioConfig test
  let grantedGroups = Set.fromList $ scenarioGroups scenario
  pure
    [ setAclTesting $ \candidates ->
        pure $ filter (`Set.member` grantedGroups) candidates
    ]

createAclDatabase
  :: opts
  -> Some LocalOrRemote
  -> (opts -> RunIndexer)
  -> TestConfig
  -> IO ()
createAclDatabase opts backend indexer test = do
  (sourceRoot, repoConfig) <- readRepoConfig $ testRoot test
  scenario <- readScenarioConfig test
  let sourceTest = test { testRoot = sourceRoot }
  case scenarioPruned scenario of
    Nothing -> createBaseDatabase repoConfig opts backend indexer sourceTest
    Just pruned ->
      createIncrementalDatabase
        repoConfig pruned opts backend indexer sourceTest

createBaseDatabase
  :: RepoConfig
  -> opts
  -> Some LocalOrRemote
  -> (opts -> RunIndexer)
  -> TestConfig
  -> IO ()
createBaseDatabase config opts backend indexer test = do
  createAclDatabaseForRepo config backend $ testRepo test
  (do
    runIndexerForTest backend (indexer opts) test
    Glean.finish backend $ testRepo test
    ) `finally` closeTestDatabaseIfLocal backend (testRepo test)

createIncrementalDatabase
  :: RepoConfig
  -> PrunedConfig
  -> opts
  -> Some LocalOrRemote
  -> (opts -> RunIndexer)
  -> TestConfig
  -> IO ()
createIncrementalDatabase config pruned opts backend indexer test = do
  let base = aclBaseRepo $ testRepo test
  createAclDatabaseForRepo config backend base
  (do
    runIndexerForTest backend (indexer opts) test { testRepo = base }
    Glean.finish backend base
    ) `finally` closeTestDatabaseIfLocal backend base

  response <- Glean.kickOffDatabase backend def
    { Thrift.kickOff_repo = testRepo test
    , Thrift.kickOff_dependencies = Just $ Thrift.Dependencies_pruned def
        { Thrift.pruned_base = base
        , Thrift.pruned_units = map UTF8.fromString $ prunedUnits pruned
        , Thrift.pruned_exclude = prunedExclude pruned
        }
    }
  when (Thrift.kickOffResponse_alreadyExists response) $
    die $ "incremental repo already exists: " <> show (testRepo test)
  Glean.finish backend (testRepo test) `finally`
    closeTestDatabaseIfLocal backend (testRepo test)

createAclDatabaseForRepo
  :: RepoConfig
  -> Some LocalOrRemote
  -> Thrift.Repo
  -> IO ()
createAclDatabaseForRepo config backend repo = do
  response <- Glean.kickOffDatabase backend def
    { Thrift.kickOff_repo = repo
    , Thrift.kickOff_properties =
        HashMap.insert "glean.acl" "enabled" $ repoProperties config
    , Thrift.kickOff_acl_config = Just $ repoAcl config
    }
  when (Thrift.kickOffResponse_alreadyExists response) $
    die $ "repo already exists: " <> show repo

aclBaseRepo :: Thrift.Repo -> Thrift.Repo
aclBaseRepo repo = repo
  { Thrift.repo_name = Thrift.repo_name repo <> "-base" }

readRepoConfig :: FilePath -> IO (FilePath, RepoConfig)
readRepoConfig root = do
  path <- findRepoConfig root
  config <- readYaml path
  pure (takeDirectory path, config)

findRepoConfig :: FilePath -> IO FilePath
findRepoConfig root = do
  let path = root </> "acl.yaml"
      parent = takeDirectory root
  exists <- doesFileExist path
  if exists
    then pure path
    else if equalFilePath root parent
      then die $ "no acl.yaml found above ACL test scenario: " <> root
      else findRepoConfig parent

readScenarioConfig :: TestConfig -> IO ScenarioConfig
readScenarioConfig test = readYaml $ testRoot test </> "scenario.yaml"

readYaml :: Aeson.FromJSON a => FilePath -> IO a
readYaml path = do
  result <- Yaml.decodeFileEither path
  case result of
    Left err -> die $ path <> ": " <> Yaml.prettyPrintParseException err
    Right value -> pure value
