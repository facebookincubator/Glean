-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module DbDeriveTest (main) where

import Data.Default
import Test.HUnit
import Util.String.Quasi
import Data.Proxy
import Control.Concurrent
import Control.Monad
import Control.Concurrent.Async
import qualified Data.HashMap.Strict as HashMap
import Control.Concurrent.STM

import TestRunner

import qualified Glean
import Glean hiding (deriveStored)
import Glean.Init
import Glean.Database.Test
import Glean.Database.Types
import qualified Glean.Database.Catalog as Catalog
import Glean.Internal.Types hiding (Predicate)
import Glean.Types as Thrift
import Glean.Test.HUnit
import qualified Glean.Schema.GleanTest.Types as Glean.Test

import TestDB

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "deriveStored" $ testDerivation $ deriveSerial $ const mempty
  , TestLabel "deriveParallel" deriveParallelTest
  , TestLabel "deriveStoredLogging" testDeriveStoredLogging
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
  addDummyDerivationForPredicate (getName pred) env
  derivedCount <- derive pred
  assertEqual "deriveTest - repo check" 1 derivedCount

addDummyDerivationForPredicate :: PredicateRef -> Env -> IO ()
addDummyDerivationForPredicate pred env =
  let repo = Thrift.Repo "name" "hash"
  in
  atomically
    $ modifyTVar' (envDerivations env)
    $ HashMap.insert (repo, pred) Derivation
      { derivationStart = undefined
      , derivationQueryingFinished = undefined
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
