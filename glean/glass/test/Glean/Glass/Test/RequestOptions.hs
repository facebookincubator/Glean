{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Glean.Glass.Test.RequestOptions ( main) where

import Control.Exception
import Data.Bifunctor
import Data.Default
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Test.HUnit

import JustKnobs (evalKnob)

import qualified Glean
import qualified Glean.Clang.Test.DerivePass as DerivePass
import Glean.Regression.Config (TestConfig(..))
import Glean.Regression.Indexer (runIndexerForTest)
import Glean.Regression.Snapshot.Driver
import Glean.Regression.Test
import Glean.Test.Mock ( Mock, call, implement )
import Glean.Util.Some

import Glean.Glass.SnapshotBackend
  (SnapshotStatus (..), SnapshotBackend (getSnapshot))
import Glean.Glass.Types (
  DocumentSymbolListXResult (..),
  DocumentSymbolsRequest (..),
  GlassExceptionReason (..),
  RequestOptions (..),
  GlassException (..),
 )
import qualified Glean.Glass.Env as Glass
import qualified Glean.Glass.Handler as Glass
import qualified Glean.Glass.Regression.Util as Glass
import Glean.Glass.SourceControl
import qualified Glean.Glass.Types as Glass

cxxRepo :: Text
cxxRepo = "fbsource"

examplePath, newPath :: Glass.Path
examplePath = Glass.Path "main.cpp"
newPath = Glass.Path "NewFile.cpp"

type WithEnv = forall a . ((Glass.Env -> IO a) -> IO a)

main :: IO ()
main = do
  useJK <- evalKnob "code_indexing/glean:glass_use_fbcode_cxx_incr"

  mainTestIndexGeneric driver (pure ()) "glass-request-options"
    $ \_ _ _ _ get ->
      let
        withEnv :: WithEnv
        withEnv f = do
          (backend, _) <- get
          Glass.withTestEnvScm backend (Some MockSourceControl) f
      in
      TestList
        [ testBaselineDB withEnv
        , testBaselineSnapshot withEnv
        , testExactRevision withEnv
        , TestLabel "use-revision" $ TestList $
           [testUseRevision withEnv] <>
           [testUseRevisionJK withEnv | useJK == Right True]
        , testNearestRevision withEnv
        ]
  where
  baseDriver = DerivePass.driver []
  driver = baseDriver {
    driverCreateDatabase = createDB,  -- override so we can create 2 DBs
    driverGroups = take 1 . driverGroups baseDriver  -- we only need 1 platform
    }

  createDB opts backend indexer testConfig = do
    let createTestDatabase cfg =
          Glean.fillDatabase backend (testRepo cfg) "" Nothing
            (error "repo already exists") $ do
              let Glean.Repo name hash = testRepo cfg
              _ <- Glean.updateProperties backend (testRepo cfg)
                (HashMap.fromList [("glean.scm." <> name, hash)]) []
              runIndexerForTest backend (indexer opts) cfg

    createTestDatabase testConfig { testRepo = Glean.Repo cxxRepo "1" }
    createTestDatabase testConfig { testRepo = Glean.Repo cxxRepo "2" }


--------------------------------------------------------------------------------
-- tests
--
-- All tests start with a DB root containing two DBs for revisions 1 and 2
-- The snapshot backend is empty, but tests can mock it to add snapshots

testBaselineDB :: WithEnv -> Test
testBaselineDB withEnv = TestLabel "DBs" $ TestCase $ withEnv $ \env -> do

  result <- symbolsList env def{revision = Glass.Revision "3"}
  assertEqual "should return the latest DB available"
    (SimpleSymbolsListXResult (Glass.Revision "2") False)
    result

  result <- symbolsList env def{revision = Glass.Revision "1"}
  assertEqual "should return exact DB match"
    (SimpleSymbolsListXResult (Glass.Revision "1") False)
    result

testBaselineSnapshot :: WithEnv -> Test
testBaselineSnapshot withEnv = TestLabel "snaphots" $ TestCase $ withEnv $ \env -> do
  sb <- mockSnapshotBackendSimple
    [(examplePath, Glass.Revision "3")
    ,(newPath, Glass.Revision "4")
    ]
  let env' :: Glass.Env = env {Glass.snapshotBackend = Some sb}

  resultOlderMatch <-
    symbolsList env' def{revision = Glass.Revision "3", exact = True}
  assertEqual "Older snapshot match is honored"
    (SimpleSymbolsListXResult (Glass.Revision "3") True)
    resultOlderMatch

  resultNewFile <-
    symbolsList env' def{revision = Glass.Revision "5", path = newPath}
  assertEqual "Latest snapshot available is used for a new file"
    (SimpleSymbolsListXResult (Glass.Revision "4") True)
    resultNewFile

testExactRevision :: WithEnv -> Test
testExactRevision withEnv = TestLabel "exact-revision" $ TestList
  [ TestLabel "DB match" $ TestCase $ withEnv $ \env -> do
      result <- symbolsList (failSourceControl env)
        def{revision = Glass.Revision "2", exact = True}
      assertEqual "DB match"
        (SimpleSymbolsListXResult (Glass.Revision "2") False)
        result
  , TestLabel "no match" $ TestCase $ withEnv $ \env -> do
      result <- try $ symbolsList (failSourceControl env)
        def{revision = Glass.Revision "3", exact = True}
      assertGlassException
        "exact revision throws if no match"
        (GlassExceptionReason_exactRevisionNotAvailable "Requested exactly 3")
        result
  , TestLabel "snapshot match" $ TestCase $ withEnv $ \env -> do
      sb <- mockSnapshotBackendSimple
            [(examplePath, Glass.Revision "2"),
             (examplePath, Glass.Revision "3")]
      let env' :: Glass.Env = env { Glass.snapshotBackend = Some sb}

      result' <-
        symbolsList (failSourceControl env')
          def{revision = Glass.Revision "3", exact = True}
      assertEqual "Snapshot match"
        (SimpleSymbolsListXResult (Glass.Revision "3") True)
        result'

      result' <-
        symbolsList (failSourceControl env')
          def{revision = Glass.Revision "2", exact = True}
      assertEqual "DB match has priority over snapshot match"
        (SimpleSymbolsListXResult (Glass.Revision "2") False)
        result'
  ]

testUseRevision :: WithEnv -> Test
testUseRevision withEnv = TestList
  [ TestLabel "exact" $ TestCase $ withEnv $ \env -> do
      result <- try $ symbolsList env def{
        revision = Glass.Revision "3",
        exact = True}
      assertGlassException
        "exact revision throws if no match"
        (GlassExceptionReason_exactRevisionNotAvailable "Requested exactly 3")
        result
  , TestLabel "default" $ TestCase $ withEnv $ \env -> do
      result <- symbolsList env def{
        revision = Glass.Revision "3",
        exact = False}
      assertEqual "Pick latest if missing"
        (SimpleSymbolsListXResult (Glass.Revision "2") False)
        result
  ]

testUseRevisionJK :: WithEnv -> Test
testUseRevisionJK withEnv = TestLabel "incr" $ TestList
  [ TestLabel "default" $ TestCase $ withEnv $ \env -> do
      result <- symbolsList env def{
        revision = Glass.Revision "1",
        exact = False}
      assertEqual "Expected matching revision"
        (SimpleSymbolsListXResult (Glass.Revision "1") False)
        result
  , TestLabel "exact" $ TestCase $ withEnv $ \env -> do
      result <- symbolsList env def{
        revision = Glass.Revision "1",
        exact = True}
      assertEqual "Expected matching revision"
        (SimpleSymbolsListXResult (Glass.Revision "1") False)
        result
  , TestLabel "snapshot and stale DB" $ TestCase $ withEnv $ \env -> do
      sb <- mockSnapshotBackendSimple [(examplePath, Glass.Revision "3")]
      let env' :: Glass.Env = env { Glass.snapshotBackend = Some sb}
      result <- symbolsList env' def{
        revision = Glass.Revision "3",
        exact = True}
      assertEqual "Expected snapshot"
        (SimpleSymbolsListXResult (Glass.Revision "3") True)
        result
  , TestLabel "snapshot and exact DB" $ TestCase $ withEnv $ \env -> do
      sb <- mockSnapshotBackendSimple [(examplePath, Glass.Revision "1")]
      let env' :: Glass.Env = env { Glass.snapshotBackend = Some sb}
      result <- symbolsList env' def{
        revision = Glass.Revision "1",
        exact = True}
      assertEqual "Expected snapshot"
        (SimpleSymbolsListXResult (Glass.Revision "1") False)
        result
  , TestLabel "snapshot only" $ TestCase $ withEnv $ \env -> do
      sb <- mockSnapshotBackendSimple [(newPath, Glass.Revision "3")]
      let env' :: Glass.Env = env { Glass.snapshotBackend = Some sb}
      result <- symbolsList env' def{
        path = newPath,
        revision = Glass.Revision "3",
        exact = True}
      assertEqual "Expected snapshot"
        (SimpleSymbolsListXResult (Glass.Revision "3") True)
        result
  ]

--------------------------------------------------------------------------------
-- Nearest revision

testNearestRevision :: WithEnv -> Test
testNearestRevision withEnv = TestLabel "nearest" $ TestList
  [ TestLabel "pick" $ TestCase $ withEnv $ \env -> do
    result <- symbolsList env def {
      revision = Glass.Revision "1a"}
    assertEqual "Nearest DB to 1a should be 1"
      (SimpleSymbolsListXResult (Glass.Revision "1") False)
      result

    result <- symbolsList env def {
      revision = Glass.Revision "1b"}
    assertEqual "Nearest DB to 1b should be 2"
      (SimpleSymbolsListXResult (Glass.Revision "2") False)
      result

    result <- symbolsList env def {
      revision = Glass.Revision "2a"}
    assertEqual "Nearest DB to 2a should be 2"
      (SimpleSymbolsListXResult (Glass.Revision "2") False)
      result

    result <- symbolsList env def {
      revision = Glass.Revision "1"}
    assertEqual "Nearest DB to 1 should be 1"
      (SimpleSymbolsListXResult (Glass.Revision "1") False)
      result
  ]

data MockSourceControl = MockSourceControl

instance SourceControl MockSourceControl where
  getGeneration _ _repo (Glass.Revision "1") = return (Just (ScmGeneration 10))
  getGeneration _ _repo (Glass.Revision "1a") = return (Just (ScmGeneration 11))
  getGeneration _ _repo (Glass.Revision "1b") = return (Just (ScmGeneration 19))
  getGeneration _ _repo (Glass.Revision "2") = return (Just (ScmGeneration 20))
  getGeneration _ _repo (Glass.Revision "2a") = return (Just (ScmGeneration 21))
  getGeneration _ _repo (Glass.Revision "2b") = return (Just (ScmGeneration 29))
  getGeneration _ _repo (Glass.Revision "3") = return (Just (ScmGeneration 30))
  getGeneration _ _repo _ = return Nothing

-- Used to check scenarios where we don't expect to call
-- getGeneration, such as when exact_revision = True
data FailSourceControl = FailSourceControl

instance SourceControl FailSourceControl where
  getGeneration _ _ _ = throwIO $ ErrorCall "FailSourceControl"

failSourceControl :: Glass.Env -> Glass.Env
failSourceControl env = env { Glass.sourceControl = Some FailSourceControl }

--------------------------------------------------------------------------------
-- helpers

assertGlassException
  :: (Eq b, Show b)
  => String
  -> GlassExceptionReason
  -> Either GlassException b
  -> Assertion
assertGlassException msg reason =
  assertEqual msg (Left [reason]) . first glassException_reasons

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
      exact :: Bool
    }
  simplify _ = error "Not implemented"
  fromSimple (SimpleDocumentSymbolsRequest repo path revision exact) =
    ( DocumentSymbolsRequest
          { documentSymbolsRequest_repository = repo
          , documentSymbolsRequest_filepath = path
          , documentSymbolsRequest_range = Nothing
          , documentSymbolsRequest_include_refs = False
          }
    , RequestOptions
          { requestOptions_revision = Just revision
          , requestOptions_limit = Nothing
          , requestOptions_feature_flags =
            Just Glass.FeatureFlags {
              featureFlags_include_xlang_refs = Nothing
            }
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
  getSnapshot _ (MockSnapshotBackend get) repo path revision =
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
