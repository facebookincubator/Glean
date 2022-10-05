{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Angle.OptTest (main) where

import Data.Default
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import Test.HUnit

import TestRunner
import Util.String.Quasi

import Glean.Backend as Backend
import Glean.Init
import Glean.Query.Angle as Angle
import Glean.Query.Thrift as Thrift
import Glean.Query.Thrift.Internal
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.GleanTest.Types as Glean.Test
import Glean.Typed hiding (end)
import Glean.Types

import TestDB

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "opt" optTest
  ]

queryStats
  :: forall q backend . (Backend backend)
  => backend
  -> Repo
  -> Query q
  -> IO ([q], Maybe UserQueryStats)

queryStats be repo (Query query) = do
  let
    opts = fromMaybe def (userQuery_options query)
    query' = query
      { userQuery_encodings = [UserQueryEncoding_bin def]
      , userQuery_options = Just
        opts { userQueryOptions_collect_facts_searched = True }
      }
  UserQueryResults{..} <- userQuery be repo query'
  results <- decodeResults userQueryResults_results decodeAsFact
  return (results, userQueryResults_stats)


factsSearched
  :: PredicateRef
  -> Map PredicateRef Int64
  -> Maybe UserQueryStats
  -> Maybe Int
factsSearched ref lookupPid maybeStats = do
  pid <- Map.lookup ref lookupPid
  stats <- maybeStats
  searched <- userQueryStats_facts_searched stats
  count <- Map.lookup pid searched
  return (fromIntegral count)

optTest :: Test
optTest = dbTestCase $ \env repo -> do
  si <- getSchemaInfo env repo def { getSchemaInfo_omit_source = True }
  let lookupPid = Map.fromList
        [ (ref,pid) | (pid,ref) <- Map.toList (schemaInfo_predicateIds si) ]

  -- Test unification: this query should be as efficient as simply
  --    cxx1.Name "an"..
  (_, stats) <- queryStats env repo $ angle @Cxx.Name
    [s|
      N where
        {Y,_} = W : {a : string, b : nat};
        N = cxx1.Name X;
        X = Y;
        W = {"an".., 3}
    |]
  assertEqual "opt 1" (Just 2) $
    factsSearched (PredicateRef "cxx1.Name" 5) lookupPid stats

  -- test that unification can optimise A = B | C, unifying A/B and A/C
  (_, stats) <- queryStats env repo $ angle @Cxx.Name
    [s|
      N where
        {"an".., N} =
          {X:string, cxx1.Name X} |
          {"def", cxx1.Name "def"}
    |]
  assertEqual "opt 2" (Just 2) $
    factsSearched (PredicateRef "cxx1.Name" 5) lookupPid stats

  -- test for recursive substitution
  r <- runQuery_ env repo $ angleData @Nat $
    [s|
      X where X = Y : nat; Y = X; X = 3
    |]
  assertEqual "opt 3" [Nat 3] r

  -- test for optimised ordering of nested fact patterns
  (_, stats) <- queryStats env repo $ angleData @Text
    [s|
      S where
        glean.test.Predicate {
          string_ = "clobber",
          sum_ = { c = { string_ = S }}
        }
    |]
  assertEqual "opt 4" (Just 4) $
    factsSearched (PredicateRef "glean.test.Predicate" 5) lookupPid stats

  -- test for optimising away unmatchable alternative without
  -- removing matchable ones
  r <- runQuery_ env repo $ angleData @Nat
    [s|
      1 = (2|1); 1
    |]
  assertEqual "opt 5" 1 (length r)

  -- Test for optimising away unmatchable alternatives.
  (_, stats) <- queryStats env repo $ angleData @Text
    [s|
      B where
      { "a", B } =
          # this should be optimised away:
        ( { A, B } where glean.test.StringPair { A, B }; A = "b") |
          # leaving this only:
        ( { A, B } where glean.test.StringPair { A, B }; A = "a")
     |]
  -- we can tell the first alternative was optimised away by counting
  -- the number of facts searched for glean.test.StringPair.
  assertEqual "optimise or 1" (Just 1) $
    factsSearched (PredicateRef "glean.test.StringPair" 1) lookupPid stats

  -- Test that unification works properly with literal fact IDs
  result : _ <- runQuery_ env repo (allFacts :: Query Glean.Test.StringPair)
  let fid = factId (getId result)
  results <- runQuery_ env repo $ Angle.query $ fid `where_` [ fid .= fid ]
  assertEqual "unify fact Id" 1 (length results)

  -- Test that trivially false negations fail the entire query
  (_, stats) <- queryStats env repo $ angle @Glean.Test.StringPair
    [s|
      A where
        A = glean.test.StringPair _;
        !(1 = 1);
    |]
  assertEqual "opt - negation" Nothing $
    factsSearched (PredicateRef "glean.test.StringPair" 1) lookupPid stats

  -- Test for unification inside negation
  (_, stats) <- queryStats env repo $ angleData @Nat
    [s|
      A where
        !(!(glean.test.Node { label = X }; X = "g"..));
        A = 1;
    |]
  assertEqual "opt - negation 2" Nothing $
    factsSearched (PredicateRef "glean.test.Node" 1) lookupPid stats

  -- a false condition in an if pattern gets optimised away.
  (_, stats) <- queryStats env repo $ angleData @Nat
    [s|
      if (1 = 2; glean.test.StringPair _)
        then 1
        else 1
     |]
  assertEqual "optimise if condition" Nothing $
    factsSearched (PredicateRef "glean.test.StringPair" 1) lookupPid stats


  -- we should recognise that the `cxx1.Name _` statement only checks
  -- whether there is any cxx1.Name fact in the db and stop after finding
  -- the first result.
  (_, stats) <- queryStats env repo $ angle @Glean.Test.Predicate
    [s| cxx1.Name _;
        glean.test.Predicate _
    |]
  assertEqual "optimise existence check" (Just 1) $
    factsSearched (PredicateRef "cxx1.Name" 5) lookupPid stats
