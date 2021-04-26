{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module DbDeriveTest (main) where

import Data.Default
import Test.HUnit
import Util.String.Quasi
import Data.Proxy
import Control.Concurrent

import TestRunner

import Glean
import Glean.Init
import Glean.Database.Types
import Glean.Types as Thrift
import qualified Glean.Schema.GleanTest.Types as Glean.Test

import TestDB

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "deriveTest" deriveTest
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
