{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module DbDeriveTest (main) where

import Data.Default
import Test.HUnit
import Util.String.Quasi
import Data.Proxy
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Concurrent.Async
import qualified Data.HashMap.Strict as HashMap
import Control.Concurrent.STM

import TestRunner

import qualified Glean
import Glean hiding (derivePredicate, deriveStored)
import Glean.Init
import Glean.Database.Types
import qualified Glean.Database.Catalog as Catalog
import Glean.Types as Thrift
import Glean.Test.HUnit
import qualified Glean.Schema.GleanTest.Types as Glean.Test

import TestDB

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "derivePredicate" $ testDerivation derivePredicate
  , TestLabel "deriveStored" $ testDerivation $ deriveStored $ const mempty
  , TestLabel "deriveStoredLogging" testDeriveStoredLogging
  ]

testDerivation :: RunDerive -> Test
testDerivation derive = TestList
  [ TestLabel "deriveTest" $ deriveTest derive
  , TestLabel "completenessTest" $ completenessTest derive
  ]

type RunDerive = forall p. Predicate p => Env -> Repo -> Proxy p -> IO Int

-- Test deriving stored facts
deriveTest :: RunDerive -> Test
deriveTest runDerive = dbTestCaseWritable $ \env repo -> do
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
      [getName $ Proxy @Glean.Test.StoredRevStringPair])
    $ void $ derive (Proxy @Glean.Test.StoredRevStringPairWithA)

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

derivePredicate :: Predicate p => Env -> Repo -> Proxy p -> IO Int
derivePredicate env repo proxy = do
  DerivePredicateResponse handle <-
    Glean.derivePredicate env repo $ derivePredicateQuery pred
  fromIntegral . Thrift.userQueryStats_result_count <$> loop handle
  where
    pred = getName proxy
    loop handle = do
      progress <- Glean.pollDerivation env handle
      case progress of
        Thrift.DerivationProgress_ongoing _ ->
          threadDelay (ceiling @Double 1e6) >> loop handle
        Thrift.DerivationProgress_complete  stats -> return stats

testDeriveStoredLogging :: Test
testDeriveStoredLogging = dbTestCaseWritable $ \env repo -> do
  tvar <- newTVarIO (0 :: Int)
  let log _ = atomically $ modifyTVar' tvar (+ 1)

  -- The result is logged once and only once.
  mapConcurrently_ (deriveStored log env repo)
      $ replicate 10 $ Proxy @Glean.Test.StoredRevStringPairWithRev
  logCalls <- readTVarIO tvar
  assertEqual "deriveStoredLogging - logs once" 1 logCalls


deriveStored
  :: forall p. Predicate p
  => (Either SomeException Thrift.UserQueryStats -> IO ())
  -> Env
  -> Repo
  -> Proxy p
  -> IO Int
deriveStored log env repo proxy = do
  () <- loop
  length <$> runQuery_ env repo (allFacts @p)
  where
    pred = getName proxy
    loop = do
      res <- Glean.deriveStored env log repo
        $ derivePredicateQuery pred
      case res of
        DerivationStatus_ongoing _ -> threadDelay (ceiling @Double 1e6) >> loop
        DerivationStatus_complete _ -> return ()

derivePredicateQuery :: PredicateRef -> Thrift.DerivePredicateQuery
derivePredicateQuery (PredicateRef name version) = def
  { derivePredicateQuery_predicate = name
  , derivePredicateQuery_predicate_version = Just version
  }
