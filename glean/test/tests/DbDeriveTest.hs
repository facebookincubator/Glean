{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module DbDeriveTest (main) where

import Control.Exception
import Data.Default
import Test.HUnit
import Util.String.Quasi
import Data.Proxy
import qualified Data.Text as Text
import Control.Concurrent
import Control.Monad
import Control.Concurrent.Async
import qualified Data.HashMap.Strict as HashMap
import Control.Concurrent.STM
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath

import TestRunner

import qualified Glean
import Glean hiding (deriveStored)
import Glean.Angle.Types
import Glean.Init
import Glean.Database.Open
import Glean.Database.Schema.Types
import Glean.Database.Test
import Glean.Database.Types
import qualified Glean.Database.Catalog as Catalog
import Glean.Internal.Types hiding (Predicate)
import Glean.Types as Thrift
import Glean.Test.HUnit
import qualified Glean.Schema.GleanTest.Types as Glean.Test
import Glean.Write.JSON (syncWriteJsonBatch)

import TestDB

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "deriveStored" $ testDerivation $ deriveSerial $ const mempty
  , TestLabel "deriveParallel" deriveParallelTest
  , TestLabel "deriveStoredLogging" testDeriveStoredLogging
  , TestLabel "deriveIncremental" deriveIncrementalTest
  ]

testDerivation :: RunDerive -> Test
testDerivation derive = TestList
  [ TestLabel "deriveTest" $ deriveTest derive
  , TestLabel "completenessTest" $ completenessTest derive
  , TestLabel "deriveDeleteDeriveTest" $ deriveDeleteDeriveTest derive
  ]

type RunDerive = forall p. Predicate p => Env -> Repo -> Proxy p -> IO Int

-- Test that deleting a DB and re-creating it works
deriveDeleteDeriveTest :: RunDerive -> Test
deriveDeleteDeriveTest runDerive = TestLabel "deriveDeleteDerive" $
  TestCase $ withTestEnv [] $ \env -> do
    let repo = Thrift.Repo "dbtest-repo" "1"
    createTestDB env repo
    deriveTestCases runDerive env repo
    void $ deleteDatabase env repo
    createTestDB env repo
    deriveTestCases runDerive env repo

-- Test deriving stored facts
deriveTest :: RunDerive -> Test
deriveTest runDerive = dbTestCaseWritable (deriveTestCases runDerive)

deriveParallelTest :: Test
deriveParallelTest = TestLabel "deriveParallelTest" $
  TestCase $ withWritableTestDB [] $ \env repo -> do
    derivedCount <-
      deriveParallel (const mempty) env repo
        (Proxy @Glean.Test.StoredDualStringPair)
        (ParallelDerivation
          { parallelDerivation_outer_predicate = "glean.test.StringPair"
          , parallelDerivation_inner_query =
              "glean.test.StoredDualStringPair { fst = X }"
          , parallelDerivation_min_batch_size = Just 1
          })
    assertEqual "deriveParallelTest" 4 derivedCount

deriveTestCases :: RunDerive -> Env -> Repo -> IO ()
deriveTestCases runDerive env repo = do
  let derive :: forall p. Predicate p => Proxy p -> IO Int
      derive = runDerive env repo
  -- initially zero facts
  results <- runQuery_ env repo $
    angle @Glean.Test.StoredRevStringPair $
    [s|
      glean.test.StoredRevStringPair _
    |]
  assertEqual "deriveTest - pre-derive" 0 (length results)

  -- compute and store. Should derive 6 facts
  derivedCount <- derive (Proxy @Glean.Test.StoredRevStringPair)
  assertEqual "deriveTest - derivation" 6 derivedCount

  -- now there should be 6 facts stored in the DB
  results <- runQuery_ env repo $
    angle @Glean.Test.StoredRevStringPair $
    [s|
      glean.test.StoredRevStringPair _
    |]
  assertEqual "deriveTest - post-derive" 6 (length results)

  -- should not confuse predicates from different repos
  let pred = Proxy @Glean.Test.StoredRevStringPairWithA
      PredicateRef{..} = getName pred
      sref = SourceRef predicateRef_name (Just predicateRef_version)
  pid <- withOpenDatabase env repo $ \opendb ->
    either (throwIO . ErrorCall . Text.unpack) return $
      lookupPredicateSourceRef sref LatestSchemaAll (odbSchema opendb)
  addDummyDerivationForPredicate (predicateId pid) env
  derivedCount <- derive pred
  assertEqual "deriveTest - repo check" 1 derivedCount

addDummyDerivationForPredicate :: PredicateId -> Env -> IO ()
addDummyDerivationForPredicate pred env =
  let repo = Thrift.Repo "name" "hash"
  in
  atomically
    $ modifyTVar' (envDerivations env)
    $ HashMap.insert (repo, pred) Derivation
      { derivationStart = undefined
      , derivationFinished = undefined
      , derivationStats = error "wrong repo!"
      , derivationPendingWrites = undefined
      , derivationError = undefined
      , derivationHandle = "dummy-handle"
      }

-- Test completeness constraint enforcement
completenessTest :: RunDerive -> Test
completenessTest runDerive = dbTestCaseWritable $ \env repo -> do
  let derive :: forall p. Predicate p => Proxy p -> IO Int
      derive = runDerive env repo
  -- deriving a stored predicate depending on a non-derived one succeeds
  derivedCount <-
    derive (Proxy @Glean.Test.StoredRevStringPairWithRev)
  assertEqual "completenessTest - derived depending on non-derived"
    6 derivedCount

  -- deriving a non-derived predicate fails
  assertThrows "completenessTest - non-derived" Thrift.NotAStoredPredicate $
    void $ derive (Proxy @Glean.Test.StringPair)

  -- deriving a derived but not stored predicate fails
  assertThrows "completenessTest - non-stored" Thrift.NotAStoredPredicate $
    void $ derive (Proxy @Glean.Test.RevStringPair)

  -- deriving a stored predicate that depends on incomplete predicates fails
  assertThrows "completenessTest - incomplete dep"
    (Thrift.IncompleteDependencies
      [ getName $ Proxy @Glean.Test.StoredRevStringPair
      , getName $ Proxy @Glean.Test.StoredRevStringPairWithA
      ])
    $ void $ derive (Proxy @Glean.Test.StoredRevStringPairSum)

  -- parallel derivation works
  let run = derive (Proxy @Glean.Test.StoredRevStringPair)
  (derivedCount1, derivedCount2) <- concurrently run run
  assertEqual "deriveTest - parallel"
    (6, 6)
    (derivedCount1, derivedCount2)

  -- should derive 6 facts now that the dependency is complete
  derivedCount <- derive (Proxy @Glean.Test.StoredRevStringPairWithA)
  assertEqual "deriveTest - subsequent derivation" 1 derivedCount

  -- deriving a complete predicate is a no-op
  derivedCount <- derive (Proxy @Glean.Test.StoredRevStringPair)
  assertEqual "deriveTest -  complete" 6 derivedCount

  -- deriving a predicate with no facts records it as complete
  let prox = Proxy @Glean.Test.EmptyStoredStringPair
      pred = getName prox
  derivedCount <- derive prox
  complete <- atomically $
    metaCompletePredicates <$> Catalog.readMeta (envCatalog env) repo
  assertEqual "deriveTest -  empty"
    (0, True)
    (derivedCount, pred `elem` complete)

testDeriveStoredLogging :: Test
testDeriveStoredLogging = dbTestCaseWritable $ \env repo -> do
  tvar <- newTVarIO (0 :: Int)
  let log _ = atomically $ modifyTVar' tvar (+ 1)

  -- The result is logged once and only once.
  replicateM_ 10 $
    deriveSerial log env repo $ Proxy @Glean.Test.StoredRevStringPairWithRev
  logCalls <- readTVarIO tvar
  assertEqual "deriveStoredLogging - logs once" 1 logCalls


deriveIncrementalTest :: Test
deriveIncrementalTest = TestLabel "incremental" $ TestList
  [ TestLabel "statement" $ TestCase $
    derivationStats
      [s|schema all.1 {
        predicate Node : nat
        predicate IsNode : nat stored X where Node X
      }|]
      [ mkBatch (PredicateRef "all.Node" 1)
          [ [s|{"key": 1}|]
          , [s|{"key": 2}|]
          , [s|{"key": 3}|]
          ]
      ]
      [ mkBatch (PredicateRef "all.Node" 1)
          [ [s|{"key": 4}|]
          ]
      ]
      (PredicateRef "all.IsNode" 1)
      $ \stats -> do
        assertEqual "incremental" 1 (userQueryStats_result_count stats)
  , TestLabel "sequence" $ TestCase $
    derivationStats
      [s|schema all.1 {
        predicate Node : nat
        predicate NodeXNode : { a: Node, b: Node } stored
          { A, B } where
            A = Node _;
            B = Node _;
      }|]
      [ mkBatch (PredicateRef "all.Node" 1)
          [ [s|{"key": 1}|]
          , [s|{"key": 2}|]
          , [s|{"key": 3}|]
          ]
      ]
      [ mkBatch (PredicateRef "all.Node" 1)
          [ [s|{"key": 4}|]
          ]
      ]
      (PredicateRef "all.NodeXNode" 1)
      $ \stats -> do
        -- (new x old) + (old x new) + (new x new)
        assertEqual "incremental" 7 (userQueryStats_result_count stats)
  , TestLabel "disjunction" $ TestCase $
    derivationStats
      [s|schema all.1 {
        predicate Node1 : nat
        predicate Node2 : nat
        predicate NodeCombinations : { a: nat, b: nat } stored
          { A, B } where
            (Node1 A; Node2 B) |
            (Node1 B; Node2 A)
      }|]
      [ mkBatch (PredicateRef "all.Node1" 1)
          [ [s|{"key": 1}|]
          , [s|{"key": 2}|]
          , [s|{"key": 3}|]
          ]

      , mkBatch (PredicateRef "all.Node2" 1)
          [ [s|{"key": 4}|]
          , [s|{"key": 5}|]
          , [s|{"key": 6}|]
          ]
      ]
      [ mkBatch (PredicateRef "all.Node1" 1)
          [ [s|{"key": 7}|]
          ]
      ]
      (PredicateRef "all.NodeCombinations" 1)
      $ \stats -> do
        -- (new x old) + (old x new)
        assertEqual "incremental" 6 (userQueryStats_result_count stats)
  , TestLabel "disjunction followed by sequence" $ TestCase $
    derivationStats
      [s|schema all.1 {
        predicate Node1 : nat
        predicate Node2 : nat
        predicate Node1and2XNode2 : { a: nat, b: nat } stored
          { A, B } where
            (Node1 A | Node2 A);
            Node2 B
      }|]
      [ mkBatch (PredicateRef "all.Node1" 1)
          [ [s|{"key": 1}|]
          , [s|{"key": 2}|]
          , [s|{"key": 3}|]
          ]

      , mkBatch (PredicateRef "all.Node2" 1)
          [ [s|{"key": 4}|]
          , [s|{"key": 5}|]
          , [s|{"key": 6}|]
          ]
      ]
      [ mkBatch (PredicateRef "all.Node1" 1)
          [ [s|{"key": 7}|]
          ]
      ]
      (PredicateRef "all.Node1and2XNode2" 1)
      $ \stats -> do
        -- (new x old)
        assertEqual "incremental" 3 (userQueryStats_result_count stats)
  , TestLabel "disjunction of expression and FactGenerator" $ TestCase $
    derivationStats
      [s|schema all.1 {
        predicate Node : nat
        predicate Node2 : { a: nat, b: nat } stored
          { A, B } where
            Node A;
            B = (X where Node X) | [11,12,13,14,15][..];
      }|]
      [ mkBatch (PredicateRef "all.Node" 1)
          [ [s|{"key": 1}|]
          , [s|{"key": 2}|]
          , [s|{"key": 3}|]
          ]
      ]
      [ mkBatch (PredicateRef "all.Node" 1)
          [ [s|{"key": 4}|]
          ]
      ]
      (PredicateRef "all.Node2" 1)
      $ \stats -> do
        -- crucially we don't want to have the 3*5 case of matching
        -- Node<base> + the array element generator.
        assertEqual "incremental" 12 -- 1*5 + 1*3 + 3*1 + 1*1
          (userQueryStats_result_count stats)
    ]
  where
    mkBatch ref facts =
      JsonFactBatch
        { jsonFactBatch_predicate = ref
        , jsonFactBatch_facts = facts
        , jsonFactBatch_unit = Nothing
        }

    -- compare the stats of an incremental derivation with that of a
    -- non-incremental one.
    derivationStats
      :: String
      -> [JsonFactBatch]
      -> [JsonFactBatch]
      -> PredicateRef
      -> (UserQueryStats -> IO a)
      -> IO a
    derivationStats schema baseFacts topFacts pref action =
      withSchemaFile latestAngleVersion schema $ \root file -> do
      let settings =
            [ setRoot root
            , setSchemaPath file
            ]
      withTestEnv settings $ \env -> do
        let base = Repo "base" "0"
        kickOffTestDB env base id
        void $ syncWriteJsonBatch env base baseFacts Nothing
        void $ completePredicates env base
        _ <- deriveStored' env base pref
        completeTestDB env base

        stacked <- stackedDB env base (Repo "base-stacked" "0")
        void $ syncWriteJsonBatch env stacked topFacts Nothing

        stats <- deriveStored' env stacked pref
        action stats

    stackedDB :: Env -> Repo -> Repo -> IO Repo
    stackedDB env base top = do
      kickOffTestDB env top $ \kickOff ->
        kickOff { kickOff_dependencies = Just $ Dependencies_stacked base }
      return top

    -- get derivation stats
    deriveStored' :: Env -> Repo -> PredicateRef -> IO UserQueryStats
    deriveStored' env repo pref = loop
      where
        loop = do
          let query = derivePredicateQuery pref Nothing
          res <- Glean.deriveStored env (const mempty) repo query
          case res of
            DerivationStatus_complete complete ->
              return $ derivationComplete_stats complete
            DerivationStatus_ongoing _ -> do
              threadDelay (ceiling @Double 1e6)
              loop

    withSchemaFile :: Int -> String -> (FilePath -> FilePath -> IO a) -> IO a
    withSchemaFile version str action = do
      withSystemTempDirectory "glean-dbtest" $ \root -> do
        let newSchemaFile = root </> "schema"
        appendFile newSchemaFile $ "version: " <> show version
        appendFile newSchemaFile str
        action root newSchemaFile

deriveSerial
  :: forall p. Predicate p
  => Glean.LogDerivationResult
  -> Env
  -> Repo
  -> Proxy p
  -> IO Int
deriveSerial log env repo proxy = deriveStored log env repo proxy Nothing

deriveParallel
  :: forall p. Predicate p
  => Glean.LogDerivationResult
  -> Env
  -> Repo
  -> Proxy p
  -> ParallelDerivation
  -> IO Int
deriveParallel log env repo proxy par =
  deriveStored log env repo proxy (Just par)

deriveStored
  :: forall p. Predicate p
  => Glean.LogDerivationResult
  -> Env
  -> Repo
  -> Proxy p
  -> Maybe ParallelDerivation
  -> IO Int
deriveStored log env repo proxy par = do
  () <- loop
  length <$> runQuery_ env repo (allFacts @p)
  where
    pred = getName proxy
    loop = do
      res <- Glean.deriveStored env log repo
        $ derivePredicateQuery pred par
      case res of
        DerivationStatus_ongoing _ -> threadDelay (ceiling @Double 1e6) >> loop
        DerivationStatus_complete _ -> return ()

derivePredicateQuery
  :: PredicateRef
  -> Maybe Thrift.ParallelDerivation
  -> Thrift.DerivePredicateQuery
derivePredicateQuery (PredicateRef name version) par = def
  { derivePredicateQuery_predicate = name
  , derivePredicateQuery_predicate_version = Just version
  , derivePredicateQuery_parallel = par
  }
