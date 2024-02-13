{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Glean.Glass.Test.RequestOptions ( main) where

import Control.Exception (try)
import Data.Coerce
import Data.Default
import qualified Data.Map.Strict as Map
import qualified Options.Applicative as O
import System.Environment (getEnv)
import System.IO.Temp (withSystemTempDirectory)
import Test.HUnit
import TestRunner ( testRunnerAction )

import JustKnobs (evalKnob)

import Glean.Glass.SnapshotBackend
  (SnapshotStatus (..), SnapshotBackend (getSnapshot))
import Glean.Glass.Types (
  DocumentSymbolListXResult (..),
  DocumentSymbolsRequest (..),
  GlassExceptionReason (..),
  RequestOptions (..),
  GlassException (..), RepoName (RepoName)
 )
import Glean.Indexer (Indexer(indexerOptParser, indexerRun))
import Glean.Init ( withUnitTestOptions )
import Glean.Regression.Config (TestConfig(..))
import Glean.Regression.Indexer (withTestBackend, runIndexerForTest)
import Glean.Util.Some
import qualified Glean
import qualified Glean.Glass.Env as Glass
import qualified Glean.Glass.Handler as Glass
import qualified Glean.Glass.Regression.Util as Glass
import qualified Glean.Glass.Types as Glass
import Glean.Test.Mock ( Mock, call, implement )
import qualified Glean.Clang.Test.DerivePass as Cxx
import qualified Glean.Clang.Test as Cxx

cxxRepo :: RepoName
cxxRepo = RepoName "fbsource.fbcode.cxx.incr"

examplePath, newPath :: Glass.Path
examplePath = Glass.Path "main.cpp"
newPath = Glass.Path "NewFile.cpp"

parse :: O.ParserInfo Cxx.Options
parse = O.info p mempty where
  p = indexerOptParser Cxx.indexer

main :: IO ()
main = withSystemTempDirectory "glass" $ \ outDir ->
  withUnitTestOptions parse $ \action cxxDriverOpts -> do
  clangSrcs <- getEnv "CLANG_SRCS"
  let
    mkTestConfig repo rev = TestConfig
      { testRepo = Glean.Repo repo rev
      , testOutput = outDir
      , testRoot = clangSrcs
      , testProjectRoot = clangSrcs
      , testGroup = "platform010"
      , testSchemaVersion = Nothing
      }

    indexCxx =
      indexerRun Cxx.indexer cxxDriverOpts <> Cxx.derivePasses [] cxxDriverOpts

    testConfigCxxIncr1 = mkTestConfig (coerce cxxRepo) "1"
    testConfigCxxIncr2 = mkTestConfig (coerce cxxRepo) "2"

  withTestBackend testConfigCxxIncr1 $ \backend -> do
    let createTestDatabase index testConfig =
          Glean.fillDatabase backend (testRepo testConfig) "" Nothing
            (error "repo already exists") $
              runIndexerForTest
                backend index testConfig

    createTestDatabase indexCxx testConfigCxxIncr1
    createTestDatabase indexCxx testConfigCxxIncr2

    useJK <- evalKnob "code_indexing/glean:glass_use_fbcode_cxx_incr"

    Glass.withTestEnv backend $ \env -> do
      testRunnerAction action $ TestList
        [ testBaselineDB env
        , testBaselineSnapshot env
        , testExactRevision env
        , TestLabel "use-revision" $ TestList $
           [testUseRevision env] <>
           [testUseRevisionJK env | useJK == Right True]
        ]

--------------------------------------------------------------------------------
-- tests
--
-- All tests start with a DB root containing two DBs for revisions 1 and 2
-- The snapshot backend is empty, but tests can mock it to add snapshots

testBaselineDB :: Glass.Env -> Test
testBaselineDB env = TestLabel "DBs" $ TestCase $ do

  result <- symbolsList env def{revision = Glass.Revision "3"}
  assertEqual "should return the latest DB available"
    (SimpleSymbolsListXResult (Glass.Revision "2") False)
    result

  result <- symbolsList env def{revision = Glass.Revision "1"}
  assertEqual "should ignore exact DB match, just return latest"
    (SimpleSymbolsListXResult (Glass.Revision "2") False)
    result

testBaselineSnapshot :: Glass.Env -> Test
testBaselineSnapshot env = TestLabel "snaphots" $ TestCase $ do
  sb <- mockSnapshotBackendSimple
    [(examplePath, Glass.Revision "1")
    ,(newPath, Glass.Revision "3")
    ]
  let env' :: Glass.Env = env {Glass.snapshotBackend = Some sb}

  resultOlderMatch <-
    symbolsList env' def{revision = Glass.Revision "1", exact = True}
  assertEqual "Older snapshot match is honored"
    (SimpleSymbolsListXResult (Glass.Revision "1") True)
    resultOlderMatch

  resultNewFile <-
    symbolsList env' def{revision = Glass.Revision "4", path = newPath}
  assertEqual "Latest snapshot available is used for a new file"
    (SimpleSymbolsListXResult (Glass.Revision "3") True)
    resultNewFile

testExactRevision :: Glass.Env -> Test
testExactRevision env = TestLabel "exact-revision" $ TestList
  [ TestLabel "DB match" $ TestCase $ do
      result <- symbolsList env def{revision = Glass.Revision "2", exact = True}
      assertEqual "DB match"
        (SimpleSymbolsListXResult (Glass.Revision "2") False)
        result
  , TestLabel "no match" $ TestCase $ do
      result <- try $
        symbolsList env def{revision = Glass.Revision "3", exact = True}
      assertGlassException
        "exact revision throws if no match"
        (GlassExceptionReason_exactRevisionNotAvailable "Requested exactly 3")
        result
  , TestLabel "snapshot match" $ TestCase $ do
      sb <- mockSnapshotBackendSimple
            [(examplePath, Glass.Revision "2"),
             (examplePath, Glass.Revision "3")]
      let env' :: Glass.Env = env { Glass.snapshotBackend = Some sb}

      result' <-
        symbolsList env' def{revision = Glass.Revision "3", exact = True}
      assertEqual "Snapshot match"
        (SimpleSymbolsListXResult (Glass.Revision "3") True)
        result'

      result' <-
        symbolsList env' def{revision = Glass.Revision "2", exact = True}
      assertEqual "Snapshot match has priority over DB match"
        (SimpleSymbolsListXResult (Glass.Revision "2") True)
        result'
  ]

testUseRevision :: Glass.Env -> Test
testUseRevision env = TestList
  [ TestLabel "exact" $ TestCase $ do
      result <- try $ symbolsList env def{
        revision = Glass.Revision "3",
        exact = True,
        useRevision = Just (Just True)}
      assertGlassException
        "exact revision throws if no match"
        (GlassExceptionReason_exactRevisionNotAvailable "Requested exactly 3")
        result
  , TestLabel "default" $ TestCase $ do
      result <- symbolsList env def{
        revision = Glass.Revision "3",
        exact = False,
        useRevision = Just (Just True)}
      assertEqual "Pick latest if missing"
        (SimpleSymbolsListXResult (Glass.Revision "2") False)
        result
  ]

testUseRevisionJK :: Glass.Env -> Test
testUseRevisionJK env = TestLabel "incr" $ TestList
  [ TestLabel "default" $ TestCase $ do
      result <- symbolsList env def{
        revision = Glass.Revision "1",
        exact = False,
        useRevision = Just (Just True)}
      assertEqual "Expected matching revision"
        (SimpleSymbolsListXResult (Glass.Revision "1") False)
        result
  , TestLabel "exact" $ TestCase $ do
      result <- symbolsList env def{
        revision = Glass.Revision "1",
        exact = True,
        useRevision = Just (Just True)}
      assertEqual "Expected matching revision"
        (SimpleSymbolsListXResult (Glass.Revision "1") False)
        result
  , TestLabel "snapshot and stale DB" $ TestCase $ do
      sb <- mockSnapshotBackendSimple [(examplePath, Glass.Revision "3")]
      let env' :: Glass.Env = env { Glass.snapshotBackend = Some sb}
      result <- symbolsList env' def{
        revision = Glass.Revision "3",
        exact = True,
        useRevision = Just (Just True)}
      assertEqual "Expected snapshot"
        (SimpleSymbolsListXResult (Glass.Revision "3") True)
        result
  , TestLabel "snapshot and exact DB" $ TestCase $ do
      sb <- mockSnapshotBackendSimple [(examplePath, Glass.Revision "1")]
      let env' :: Glass.Env = env { Glass.snapshotBackend = Some sb}
      result <- symbolsList env' def{
        revision = Glass.Revision "1",
        exact = True,
        useRevision = Just (Just True)}
      assertEqual "Expected snapshot"
        (SimpleSymbolsListXResult (Glass.Revision "1") True)
        result
  , TestLabel "snapshot only" $ TestCase $ do
      sb <- mockSnapshotBackendSimple [(newPath, Glass.Revision "3")]
      let env' :: Glass.Env = env { Glass.snapshotBackend = Some sb}
      result <- symbolsList env' def{
        path = newPath,
        revision = Glass.Revision "3",
        exact = True,
        useRevision = Just (Just True)}
      assertEqual "Expected snapshot"
        (SimpleSymbolsListXResult (Glass.Revision "3") True)
        result
  ]
--------------------------------------------------------------------------------
-- helpers

assertGlassException
  :: (Eq b, Show b)
  => String
  -> GlassExceptionReason
  -> Either GlassException b
  -> Assertion
assertGlassException msg reason =
  assertEqual msg
    (Left $ GlassException
      {glassException_reasons = [reason]
      ,glassException_revisions = []
      })

symbolsList
  :: Glass.Env
  -> Simple (DocumentSymbolsRequest, RequestOptions)
  -> IO (Simple DocumentSymbolListXResult)
symbolsList env request = do
  let (req, ropts) = fromSimple request
  simplify <$> Glass.documentSymbolListX env req ropts

--------------------------------------------------------------------------------
-- simplify glass thrift types for 'assertEqual' convenience

class Simplify glassType where
  data Simple glassType
  simplify :: glassType -> Simple glassType
  fromSimple :: Simple glassType -> glassType

instance Simplify GlassException where
  data Simple GlassException =
    SimpleGlassException Glass.Revision GlassExceptionReason
    deriving (Eq, Show)
  simplify GlassException{..} =
    SimpleGlassException
      (head glassException_revisions)
      (head glassException_reasons)
  fromSimple = error "not implemented"

instance Simplify DocumentSymbolListXResult where
  data Simple DocumentSymbolListXResult = SimpleSymbolsListXResult
      { revision :: Glass.Revision
      , isSnapshot :: Bool }
    deriving (Eq, Show)
  simplify DocumentSymbolListXResult{..} =
    SimpleSymbolsListXResult
      documentSymbolListXResult_revision
      documentSymbolListXResult_truncated
  fromSimple (SimpleSymbolsListXResult rev isSnapshotMatch) =
    DocumentSymbolListXResult [] [] rev isSnapshotMatch Nothing mempty

instance Simplify (DocumentSymbolsRequest, RequestOptions) where
  data Simple (DocumentSymbolsRequest, RequestOptions) =
    SimpleDocumentSymbolsRequest {
      repo :: Glass.RepoName,
      path :: Glass.Path,
      revision :: Glass.Revision,
      exact :: Bool,
      useRevision :: Maybe (Maybe Bool)
    }
  simplify _ = error "Not implemented"
  fromSimple (SimpleDocumentSymbolsRequest repo path revision exact use) =
    ( DocumentSymbolsRequest
          { documentSymbolsRequest_repository = repo
          , documentSymbolsRequest_filepath = path
          , documentSymbolsRequest_range = Nothing
          , documentSymbolsRequest_include_refs = False
          }
    , RequestOptions
          { requestOptions_revision = Just revision
          , requestOptions_limit = Nothing
          , requestOptions_feature_flags = fmap Glass.FeatureFlags use
          , requestOptions_strict = True
          , requestOptions_exact_revision = exact
          }
    )

instance Default (Simple (DocumentSymbolsRequest, RequestOptions)) where
  def = SimpleDocumentSymbolsRequest
    { repo = Glass.RepoName "fbsource"
    , path = examplePath
    , revision = Glass.Revision "1"
    , exact = False
    , useRevision = Nothing
    }

--------------------------------------------------------------------------------
-- Mock snapshot backend

type GetSnapshot
  = Glass.RepoName
  -> Glass.Path
  -> Maybe Glass.Revision
  -> IO GetSnapshotResult

type GetSnapshotResult = Either SnapshotStatus DocumentSymbolListXResult

newtype MockSnapshotBackend = MockSnapshotBackend {
  mockGetSnapshot :: Mock GetSnapshot
}

instance SnapshotBackend MockSnapshotBackend where
  getSnapshot (MockSnapshotBackend get) repo path revision =
    call get repo path revision

-- | Create a mock snapshot backend containing the given snapshots
mockSnapshotBackend
  :: [(Glass.RepoName, Glass.Path, DocumentSymbolListXResult)]
  -> IO MockSnapshotBackend
mockSnapshotBackend snapshots = do
  mockGetSnapshot <- implement "getSnapshot" $ \repo path mb_rev ->
    return $
    case Map.lookup (repo, path) snapshotsMap of
      Just matches -> case mb_rev of
        Nothing -> case Map.toList matches of
          (_, snap) : _ -> Right snap
          [] -> Left NotFound
        Just rev -> case Map.lookup rev matches of
          Just snap -> Right snap
          Nothing -> Left NotFound
      Nothing -> Left NotFound
  return MockSnapshotBackend {..}
  where
    snapshotsMap = Map.fromListWith (<>)
      [ ( (repo, path)
        , Map.singleton (documentSymbolListXResult_revision result) result
        )
      | (repo, path, result) <- snapshots
      ]

-- | Create a mock snapshot backend containing simple snapshots
mockSnapshotBackendSimple
  :: [(Glass.Path, Glass.Revision)] -> IO MockSnapshotBackend
mockSnapshotBackendSimple revs =
  mockSnapshotBackend
    [(Glass.RepoName "fbsource", path, fromSimple result)
    | (path, rev) <- revs
    , let result = SimpleSymbolsListXResult rev True
    ]
