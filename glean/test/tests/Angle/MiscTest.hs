{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Angle.MiscTest (main) where

import Control.Exception
import Data.Default
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Test.HUnit

import TestRunner
import Util.String.Quasi

import Glean.Backend.Types as Backend
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
  [ TestLabel "justKeys" $ justKeys id
  , TestLabel "justKeys/page" $ justKeys (limit 1)
  , TestLabel "reorder" reorderTest
  , TestLabel "scoping" scopingTest
  , TestLabel "queryOptions" angleQueryOptions
  , TestLabel "limitBytes" limitTest
  , TestLabel "fullScans" fullScansTest
  , TestLabel "newold" $ newOldTest id
  ]

newOldTest :: (forall a . Query a -> Query a) -> Test
newOldTest modify = TestCase $ withStackedTestDB [] $ \env repo -> do
  results <- runQuery_ env repo $ modify $ keys $ Angle.query $
    new $ predicate @Cxx.Name wild
  assertEqual "angle - new facts"
    [ "abcd", "allin", "anywhere", "barbie", "blob" ]
    results

  results <- runQuery_ env repo $ modify $ keys $ Angle.query $
    old $ predicate @Cxx.Name wild
  assertEqual "angle - old facts"
    [ "abba", "anonymous", "azimuth", "blubber", "book", "foo" ]
    results

scopingTest :: Test
scopingTest = dbTestCase $ \env repo -> do
  r <- try $ runQuery_ env repo $ angle @Glean.Test.Predicate
    [s|
      cxx1.Name X
    |]
  print r
  assertBool "angle - unbound var" $
    case r of
      Left (BadQuery x) -> "mentioned only once: X" `Text.isInfixOf` x
      _ -> False

  -- bind a variable in multiple branches
  r <- runQuery_ env repo $ angleData @(Text, Nat)
    [s|
      {N,X} where X = (1 where N = "abc") | (2 where N = "def")
    |]
  print r
  assertEqual "angle - scoping" 2 (length r)

  -- must bind the variable in *all* branches
  r <- try $ runQuery_ env repo $ angleData @(Text, Nat)
    [s|
      {N,X} where X = (1 where N = "abc") | 2
    |]
  print r
  assertBool "angle - scoping 2" $ case r of
    Left e@BadQuery{} -> "not bound anywhere: N" `isInfixOf` show e
    _ -> False

  -- it's OK to not bind the variable in every branch of | if it is
  -- bound outside
  r <- runQuery_ env repo $ angleData @Text
    [s|
      N where N = (X : string) | "abc"; X = "a"
    |]
  print r
  assertEqual "angle - scoping 3" 2 (length r)

  -- local variables can have different types in disjoint branches of |
  r <- runQuery_ env repo $ angleData @(Nat, Text)
    [s|
      ({X,Y} where X = 3; Y = "a") | ({Y,X} where Y = 3; X = "a")
    |]
  print r
  assertEqual "angle - scoping 4" 1 (length r)

  -- but this is an error if the variable(s) are visible outside |
  r <- try $ runQuery_ env repo $ angleData @(Text, Nat)
    [s|
      {X,Y} where
        {X,Y} = ({X,Y} where X = 3; Y = "a") |
                ({Y,X} where Y = 3; X = "a")
    |]
  print r
  assertBool "angle - scoping 5" $ case r of
    Left e@BadQuery{} -> "type error" `isInfixOf` show e
    _ -> False

  r <- runQuery_ env repo $ Angle.query $
      predicate @Glean.Test.NothingTest wild
  print (r :: [Glean.Test.NothingTest])
  assertEqual "angle - nothingTest" 1 (length r)

justKeys :: (forall a . Query a -> Query a) -> Test
justKeys modify = dbTestCase $ \env repo -> do
  results <- runQuery_ env repo $ modify $ keys $ allFacts @Cxx.Name
  assertEqual "angle - justKeys" 11 (length results)
  assertBool "angle - justKeys" $ "abba" `elem` results

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

{-
  Test reordering of nested matches using a simple DAG:

      "a"
      / \
    "b" "c"
      \ /
      "d"
-}
reorderTest :: Test
reorderTest = dbTestCase $ \env repo -> do
  si <- getSchemaInfo env (Just repo) def { getSchemaInfo_omit_source = True }
  let lookupPid = Map.fromList
        [ (ref,pid) | (pid,ref) <- Map.toList (schemaInfo_predicateIds si) ]

  -- Inner match is not in a prefix position: do it last
  (_, stats) <- queryStats env repo $ angleData @Text
    [s|
      "result" where
        glean.test.Tree {
          { label = "b" },
          _,
          { just = { { label = "d" }, _, _ } }
        }
    |]
  print stats
  assertEqual "reorder nested 1" (Just 1) $
    factsSearched (PredicateRef "glean.test.Tree" 6) lookupPid stats

  -- Inner match is in a prefix position but irrefutable: do it last
  (_, stats) <- queryStats env repo $ angleData @Text
    [s|
      X where glean.test.Tree { { label = "b" }, { just = { X, _, _ } }, _ }
    |]
  assertEqual "reorder nested 2" Nothing $
    factsSearched (PredicateRef "glean.test.Tree" 6) lookupPid stats

  -- Inner match is in a prefix position and refutable: do it first
  (_, stats) <- queryStats env repo $ angleData @Text
    [s|
      "result" where
        glean.test.Tree {
          { label = "b" },
          { just = { { label = "d" }, _, _ } },
          _
        }
    |]
  print stats
  assertEqual "reorder nested 3" Nothing $
    factsSearched (PredicateRef "glean.test.Tree" 6) lookupPid stats

  -- inner match is in a prefix position (conditional on L being
  -- bound, which it is), and refutable, so do it first.
  (_, stats) <- queryStats env repo $ angleData @Text
    [s|
      "d" where
         L = glean.test.Tree { { label = "d" }, _, _ };
         glean.test.Tree {
           { label = "b" },
           { just = L },
           { just = { { label = "d" }, _, _ } } }
    |]
  assertEqual "reorder nested 4" (Just 1) $
    factsSearched (PredicateRef "glean.test.Tree" 6) lookupPid stats

  -- point match in a non-prefix position: do it first
  (_, stats) <- queryStats env repo $ angle @Glean.Test.Predicate
    [s|
      glean.test.Predicate { sum_ = { d = "hello" } }
    |]
  assertEqual "reorder nested 5" (Just 1) $
    factsSearched (PredicateRef "sys.Blob" 1) lookupPid stats

  -- nested (almost) irrefutable inner match: do it last
  (_, stats) <- queryStats env repo $ angleData @Text
    [s|
      X where
        glean.test.Tree {
          { label = "a" },
          _,
          { just = { _, { just = { X, _, _ } }, _ }}
        }
    |]
  assertEqual "reorder nested 6" (Just 1) $
    factsSearched (PredicateRef "glean.test.Tree" 6) lookupPid stats

  -- Nested matches on the rhs of a lookup should become lookups
  (_, stats) <- queryStats env repo $ angle @Glean.Test.Tree
    [s|
      L where
        L = glean.test.Tree { { label = "a" }, { just = M }, _ };
        M = glean.test.Tree { { label = "b" }, { just = { { label = "d" }, _, _ }}, _}
          # L is bound, so even though the nested match { { label = "d" }, _, _ }
          # is in a prefix position and would normally be done first,
          # in this case we want to do it afterwards because it's a lookup
          # and a lookup is always cheaper than a search.
    |]
  assertEqual "reorder nested 7" (Just 1) $
    factsSearched (PredicateRef "glean.test.Tree" 6) lookupPid stats

  -- Ordering where the nested query is in a prefix position of its
  -- parent and not a point query.
  (_, stats) <- queryStats env repo $ angle @Glean.Test.Edge
    [s|
      X where
        X = glean.test.Edge { parent = { label = "b".. }}
          # We want the inner query to be done first, on the grounds that
          # then the outer query becomes a prefix match.
    |]
  assertEqual "reorder nested 8" (Just 1) $
    factsSearched (PredicateRef "glean.test.Edge" 6) lookupPid stats

  -- negations are moved after the binding of the variables it mentions.
  r <- runQuery_ env repo $ angleData @Text
    [s|
       A where
        !(glean.test.IsGlean A);
        A = "not-glean";
    |]
  print r
  assertEqual "negation - reorder 1" 1 (length r)

  -- negations are moved after the binding of the variables it mentions
  -- even when the variable comes from a disjunction.
  r <- runQuery_ env repo $ angleData @Text
    [s|
       A where
        !(glean.test.IsGlean A);
        glean.test.IsGlean ("g".. A) | glean.test.IsGlean ("gl".. A);
    |]
  print r
  assertEqual "negation - reorder 2" 2 (length r)

  -- negations are moved after the binding of the variables it mentions
  -- even when the variable comes from an if statement
  r <- runQuery_ env repo $ angleData @Text
    [s|
      A where
        !(glean.test.IsGlean A);
        if 1 then
          glean.test.IsGlean ("g".. A)
        else
          glean.test.IsGlean ("g".. A)
    |]
  print r
  assertEqual "negation - reorder 3" 1 (length r)

  -- The negation is moved after the binding of all the variables it mentions.
  r <- runQuery_ env repo $ angleData @Nat
    -- only if the negation is moved after the last statement will tere be two
    -- results
    [s|
      A where
        !(A = B : nat);
        TWO = [ { 1, 1 }, { 2, 2} ];
        ONE = [ { 1, 1 } ];
        { _, A} | { B, _} = ONE[..];
        { _, A} = TWO[..];
        { B, _} = TWO[..];
    |]
  print r
  assertEqual "negation - reorder 4" 2 (length r)

  -- A negation which is an O(1) filter gets moved up.
  (_,stats) <- queryStats env repo $ angleData @Text
    [s|
      N where
        V = [1];
        A = V[..];
        B = V[..];
        cxx1.Name N;
        !(A = B);
    |]
  print r
  assertEqual "negation - reorder 5" Nothing $
    factsSearched (PredicateRef "cxx1.Name" 5) lookupPid stats

  -- test for a bad case in reordering where the nested matches under
  -- `left` were being lifted out before the outer match.
  (_, stats) <- queryStats env repo $ angleData @Text
    [s|
      L where
        glean.test.Tree {
          node = { label = "a" },
          left = { just = { node = { label = L } } }
        }
    |]
  assertEqual "reorder nested 9" (Just 1) $
    factsSearched (PredicateRef "glean.test.Tree" 6) lookupPid stats

  -- Test for a bug in reordering where we were erroneously hoising
  -- statements that couldn't resolve, because we ignored wildcards
  (_, stats) <- queryStats env repo $ angleData @Glean.Test.Tree
    [s|
      X where
        A = glean.test.Node { label = "a" };
        (glean.test.Tree X; { node = A } = X) |
        glean.test.Tree X
    |]
  assertEqual "reorder nested 10" (Just 14) $
    factsSearched (PredicateRef "glean.test.Tree" 6) lookupPid stats

angleQueryOptions :: Test
angleQueryOptions = dbTestCase $ \env repo -> do
  let omitResults :: Bool -> Query q -> Query q
      omitResults omit (Query query) = Query q
        where
          opts = fromMaybe def (userQuery_options query)
          q = query
            { userQuery_options = Just opts
              { userQueryOptions_omit_results = omit }
            }

      counts (_, Nothing) = error "No query stats"
      counts (results, Just UserQueryStats{..}) =
        ( length results
        , userQueryStats_result_count
        )

  r <- queryStats env repo $ omitResults True $ angle @Cxx.FunctionName
    [s|
      cxx1.FunctionName { name = "ab"..  }
    |]
  assertEqual "queryOptions - omitting results" (counts r) (0, 2)

  r <- queryStats env repo $ omitResults False $ angle @Cxx.FunctionName
    [s|
      cxx1.FunctionName { name = "ab"..  }
    |]
  assertEqual "queryOptions - not omitting results" (counts r) (2, 2)


limitTest :: Test
limitTest = dbTestCase $ \env repo -> do
  (results, truncated) <- runQuery env repo $ limitBytes 30 $ recursive $
    Angle.query $ predicate @Glean.Test.Edge wild
  -- each Edge will be
  --   about 12 bytes for the Edge (8 byte Id + 2 refs @ 2 bytes each)
  --   about 11 bytes for each Node (8 byte Id + 3 byte string)
  --   = 33 bytes
  -- so a limit of 30 bytes should ensure that we're counting the size
  -- of the nested facts too.
  assertBool "limitBytes" (length results == 1 && truncated)

  (results, truncated) <- runQuery env repo $ limitBytes 40 $ recursive $
    Angle.query $ predicate @Glean.Test.Edge wild
  assertBool "limitBytes" (length results == 2 && truncated)

fullScansTest :: Test
fullScansTest = TestList $
  [ TestLabel "no full scans" $ TestCase $ withTestDB [] $ \env repo -> do
      (_, Just stats) <- queryStats env repo $ angleData @Glean.Test.Node
        [s| glean.test.Node { label = "A".. } |]
      assertEqual "full scans" [] (userQueryStats_full_scans stats)
  , TestLabel "one full scan" $ TestCase $ withTestDB [] $ \env repo -> do
      (_, Just stats) <- queryStats env repo $ angleData @Glean.Test.Edge
        [s| N = glean.test.Node _;
            glean.test.Edge { N, _ };
        |]
      assertEqual "full scans"
        [PredicateRef "glean.test.Node" 6 ]
        (userQueryStats_full_scans stats)
  , TestLabel "multiple full scans" $ TestCase $ withTestDB [] $ \env repo -> do
      (_, Just stats) <- queryStats env repo $ angleData @Glean.Test.Edge
        [s| glean.test.Edge { A, B } where
            A = glean.test.Node _;
            B = glean.test.Node _;
        |]
      assertEqual "full scans"
        [ PredicateRef "glean.test.Node" 6
        , PredicateRef "glean.test.Node" 6
        ]
        (userQueryStats_full_scans stats)
  ]
