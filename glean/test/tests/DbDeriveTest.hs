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

import Glean
import Glean.Init
import Glean.Database.Types
import Glean.Types as Thrift
import Glean.Test.HUnit
import qualified Glean.Schema.GleanTest.Types as Glean.Test

import TestDB

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "deriveTest" deriveTest
  , TestLabel "completenessTest" completenessTest
  ]

-- Test deriving stored facts
deriveTest :: Test
deriveTest = dbTestCaseWritableWithDerive $ \env repo derive -> do
  -- initially zero facts
  results <- runQuery_ env repo $
    angle @Glean.Test.StoredRevStringPair $
    [s|
      glean.test.StoredRevStringPair _
    |]
  assertEqual "deriveTest - pre-derive" 0 (length results)

  -- compute and store. Should derive 6 facts
  derivedCount <- derive $ getName (Proxy @Glean.Test.StoredRevStringPair)
  assertEqual "deriveTest - derivation" 6 derivedCount

  -- now there should be 6 facts stored in the DB
  results <- runQuery_ env repo $
    angle @Glean.Test.StoredRevStringPair $
    [s|
      glean.test.StoredRevStringPair _
    |]
  assertEqual "deriveTest - post-derive" 6 (length results)

  -- should not confuse predicates from different repos
  let pred = getName (Proxy @Glean.Test.StoredRevStringPairWithA)
  addDummyDerivationForPredicate pred env
  derivedCount <- derive pred
  assertEqual "deriveTest - repo check" 1 derivedCount

addDummyDerivationForPredicate :: PredicateRef -> Env -> IO ()
addDummyDerivationForPredicate ref env =
  let repo = Thrift.Repo "name" "hash"
  in
  atomically
    $ modifyTVar' (envDerivations env)
    $ HashMap.insert (repo, ref) Derivation
      { derivationStart = undefined
      , derivationQueryingFinished = undefined
      , derivationStats = error "wrong repo!"
      , derivationPendingWrites = undefined
      , derivationError = undefined
      , derivationHandle = "dummy-handle"
      }

-- Test completeness constraint enforcement
completenessTest :: Test
completenessTest = dbTestCaseWritableWithDerive $ \_ _ derive -> do
  -- deriving a stored predicate depending on a non-derived one succeeds
  derivedCount <-
    derive $ getName (Proxy @Glean.Test.StoredRevStringPairWithRev)
  assertEqual "deriveTest - derived depending on non-derived" 6 derivedCount

  -- deriving a non-derived predicate fails
  assertThrows "completenessTest - non-derived" Thrift.NotAStoredPredicate $
    void $ derive $ getName (Proxy @Glean.Test.StringPair)

  -- deriving a derived but not stored predicate fails
  assertThrows "completenessTest - non-stored" Thrift.NotAStoredPredicate $
    void $ derive $ getName (Proxy @Glean.Test.RevStringPair)

  -- deriving a stored predicate that depends on incomplete predicates fails
  assertThrows "completenessTest - incomplete dep"
    (Thrift.IncompleteDependencies
      [getName $ Proxy @Glean.Test.StoredRevStringPair])
    $ void $ derive $ getName (Proxy @Glean.Test.StoredRevStringPairWithA)

  -- parallel derivation works
  let run = derive $ getName (Proxy @Glean.Test.StoredRevStringPair)
  (derivedCount1, derivedCount2) <- concurrently run run
  assertEqual "deriveTest - parallel"
    (6, 6)
    (derivedCount1, derivedCount2)

  -- should derive 6 facts now that the dependency is complete
  derivedCount <- derive $ getName (Proxy @Glean.Test.StoredRevStringPairWithA)
  assertEqual "deriveTest - subsequent derivation" 1 derivedCount

  -- deriving a complete predicate is a no-op
  derivedCount <- derive $ getName (Proxy @Glean.Test.StoredRevStringPair)
  assertEqual "deriveTest -  complete" 6 derivedCount

dbTestCaseWritableWithDerive
  :: (Env -> Repo -> (PredicateRef -> IO Int) -> IO ())
  -> Test
dbTestCaseWritableWithDerive f =
  dbTestCaseWritable $ \env repo ->
  let
    derive (PredicateRef name version) = do
      let query = def
            { derivePredicateQuery_predicate = name
            , derivePredicateQuery_predicate_version = Just version
            }
      DerivePredicateResponse handle <- Glean.derivePredicate env repo query
      let loop = do
            progress <- Glean.pollDerivation env handle
            case progress of
              Thrift.DerivationProgress_ongoing _ ->
                threadDelay (ceiling @Double 1e6) >> loop
              Thrift.DerivationProgress_complete  stats -> return stats
      fromIntegral . Thrift.userQueryStats_result_count <$> loop
  in
  f env repo derive
