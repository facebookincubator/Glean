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
import Data.List (find)
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
        , testMatchingRevision withEnv
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
    [(examplePath, Glass.Revision "3", gen0)
    ,(newPath, Glass.Revision "4", gen0)
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
            [(examplePath, Glass.Revision "2", gen0),
             (examplePath, Glass.Revision "3", gen0)]
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
      sb <- mockSnapshotBackendSimple [(examplePath, Glass.Revision "3", gen0)]
      let env' :: Glass.Env = env { Glass.snapshotBackend = Some sb}
      result <- symbolsList env' def{
        revision = Glass.Revision "3",
        exact = True}
      assertEqual "Expected snapshot"
        (SimpleSymbolsListXResult (Glass.Revision "3") True)
        result
  , TestLabel "snapshot and exact DB" $ TestCase $ withEnv $ \env -> do
      sb <- mockSnapshotBackendSimple [(examplePath, Glass.Revision "1", gen0)]
      let env' :: Glass.Env = env { Glass.snapshotBackend = Some sb}
      result <- symbolsList env' def{
        revision = Glass.Revision "1",
        exact = True}
      assertEqual "Expected snapshot"
        (SimpleSymbolsListXResult (Glass.Revision "1") False)
        result
  , TestLabel "snapshot only" $ TestCase $ withEnv $ \env -> do
      sb <- mockSnapshotBackendSimple [(newPath, Glass.Revision "3", gen0)]
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

--------------------------------------------------------------------------------
-- Matching revision

testMatchingRevision :: WithEnv -> Test
testMatchingRevision withEnv = TestLabel "matching" $ TestList
  [
    TestLabel "snapshot" $ TestCase $ withEnv $ \env -> do
    sb <- mockSnapshotBackendSimple
      [(examplePath, Glass.Revision "14", ScmGeneration 14)
      ,(examplePath, Glass.Revision "16", ScmGeneration 16)]
    let env' :: Glass.Env = env { Glass.snapshotBackend = Some sb}
    result <- symbolsList env' def
      { revision = Glass.Revision "15", exact = False, matching = True }
    assertEqual "Nearest DB to 15 should be 16"
      (SimpleSymbolsListXResult (Glass.Revision "16") True)
      result
  , TestLabel "no match" $ TestCase $ withEnv $ \env -> do
    sb <- mockSnapshotBackendSimple
      [(examplePath, Glass.Revision "21", ScmGeneration 21)]
    let env' :: Glass.Env = env { Glass.snapshotBackend = Some sb}
    result <- try $ symbolsList env' def
      { revision = Glass.Revision "1b", exact = False, matching = True }
    assertGlassException
      "Nothing matches revision 1b"
      (GlassExceptionReason_matchingRevisionNotAvailable "1b")
      result
  ]

--------------------------------------------------------------------------------
-- Mocks

data MockSourceControl = MockSourceControl

instance SourceControl MockSourceControl where
  getGeneration _ _repo (Glass.Revision "1") = return (Just (ScmGeneration 10))
  getGeneration _ _repo (Glass.Revision "1a") = return (Just (ScmGeneration 11))
  getGeneration _ _repo (Glass.Revision "15") = return (Just (ScmGeneration 15))
  getGeneration _ _repo (Glass.Revision "1b") = return (Just (ScmGeneration 19))
  getGeneration _ _repo (Glass.Revision "2") = return (Just (ScmGeneration 20))
  getGeneration _ _repo (Glass.Revision "2a") = return (Just (ScmGeneration 21))
  getGeneration _ _repo (Glass.Revision "2b") = return (Just (ScmGeneration 29))
  getGeneration _ _repo (Glass.Revision "3") = return (Just (ScmGeneration 30))
  getGeneration _ _repo _ = return Nothing

  checkMatchingRevisions _ _repo _path rev0 revs = return (map (f rev0) revs)
    where
      f (Glass.Revision "15") (Glass.Revision "14") = True
      f (Glass.Revision "15") (Glass.Revision "16") = True
      f _ _ = False

  getFileContentHash _ _repo _path _rev0 = return Nothing

-- Used to check scenarios where we don't expect to call
-- getGeneration, such as when exact_revision = True
data FailSourceControl = FailSourceControl

instance SourceControl FailSourceControl where
  getGeneration _ _ _ = throwIO $ ErrorCall "FailSourceControl.getGeneration"
  checkMatchingRevisions _ _ _ _ revs = return $ map (const True) revs
  getFileContentHash _ _repo _path _rev0 = return Nothing

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
    DocumentSymbolListXResult [] [] rev isSnapshotMatch Nothing mempty Nothing

instance Simplify (DocumentSymbolsRequest, RequestOptions) where
  data Simple (DocumentSymbolsRequest, RequestOptions) =
    SimpleDocumentSymbolsRequest {
      repo :: Glass.RepoName,
      path :: Glass.Path,
      revision :: Glass.Revision,
      exact :: Bool,
      matching :: Bool
    }
  simplify _ = error "Not implemented"
  fromSimple SimpleDocumentSymbolsRequest{..} =
    ( def
          { documentSymbolsRequest_repository = repo
          , documentSymbolsRequest_filepath = path
          , documentSymbolsRequest_include_refs = False
          }
    , def
          { requestOptions_revision = Just revision
          , requestOptions_strict = True
          , requestOptions_exact_revision = exact
          , requestOptions_matching_revision = matching
          }
    )

instance Default (Simple (DocumentSymbolsRequest, RequestOptions)) where
  def = SimpleDocumentSymbolsRequest
    { repo = Glass.RepoName "fbsource"
    , path = examplePath
    , revision = Glass.Revision "1"
    , exact = False
    , matching = False
    }

--------------------------------------------------------------------------------
-- Mock snapshot backend

type GetSnapshot
  = Glass.RepoName
  -> Glass.Path
  -> Maybe Glass.Revision
  -> Maybe ScmGeneration
  -> IO GetSnapshotResult

type GetSnapshotResult = Either
  SnapshotStatus
  (Glass.Revision, IO (Maybe DocumentSymbolListXResult))

newtype MockSnapshotBackend = MockSnapshotBackend {
  mockGetSnapshot :: Mock GetSnapshot
}

instance SnapshotBackend MockSnapshotBackend where
  getSnapshot _ (MockSnapshotBackend get) = call get

gen0 :: ScmGeneration
gen0 = ScmGeneration 0

-- | Create a mock snapshot backend containing the given snapshots
mockSnapshotBackend
  :: [(Glass.RepoName, Glass.Path, ScmGeneration, DocumentSymbolListXResult)]
  -> IO MockSnapshotBackend
mockSnapshotBackend snapshots = do
  mockGetSnapshot <- implement "getSnapshot" implementation
  return MockSnapshotBackend {..}
  where
    -- TODO replace with a test XDB instance
    implementation repo path mb_rev mb_gen = return $
      case Map.lookup (repo, path) snapshotsMap of
        Just (byRev, byGen) -> case (mb_rev, mb_gen) of
          (Nothing, Nothing) -> case Map.toList byRev of
            (_, snap) : _ -> wrapSnapshot snap
            [] -> Left NotFound
          (Just rev, _)
            | Just snap <- Map.lookup rev byRev
            -> wrapSnapshot snap
          (_ , Just gen)
            | Just (_, snap) <- find (\(gen',_) -> gen'>=gen) (Map.toList byGen)
            -> wrapSnapshot snap
          _ -> Left NotFound
        _ -> Left NotFound
    wrapSnapshot snap =
      Right (documentSymbolListXResult_revision snap, return $ Just snap)
    snapshotsMap = Map.fromListWith (<>)
      [ ( (repo, path)
        , (Map.singleton (documentSymbolListXResult_revision result) result
          ,Map.singleton gen result
          )
        )
      | (repo, path, gen, result) <- snapshots
      ]

-- | Create a mock snapshot backend containing simple snapshots
mockSnapshotBackendSimple
  :: [(Glass.Path, Glass.Revision, ScmGeneration)] -> IO MockSnapshotBackend
mockSnapshotBackendSimple revs =
  mockSnapshotBackend
    [(Glass.RepoName "fbsource", path, gen, fromSimple result)
    | (path, rev, gen) <- revs
    , let result = SimpleSymbolsListXResult rev True
    ]
