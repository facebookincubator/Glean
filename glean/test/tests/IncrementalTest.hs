{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
module IncrementalTest (main) where

import Control.Exception (finally)
import Control.Monad
import Data.ByteString (ByteString)
import Data.Default
import Data.List
import Test.HUnit
import System.Info
import System.Exit

import TestRunner

import Glean.Backend.Types (completePredicates)
import Glean.Init
import Glean.Database.Close
import Glean.Database.Types
import Glean.Database.Ownership
import Glean.Database.Test
import Glean.RTS.Foreign.Ownership
  ( setOwnershipCompactThreshold
  , getOwnershipCompactThreshold
  , setOwnershipMergeCacheSize
  , getOwnershipMergeCacheSize
  )
import Glean.Derive
import Glean.Schema.Util (parseRef)
import qualified Glean.Schema.GleanTest as Glean.Test
import qualified Glean.Schema.GleanTest.Types as Glean.Test
import Glean.Query.Angle
import Glean.Query.Thrift
import Glean.Typed hiding (end)
import Glean.Types


{-
Build a graph using two predicates, Node and Edge, see test.angle:

predicate Node : { label : string }
predicate Edge : { parent : Node, child : Node }

The graph is:

     a
    / \
   b   c
    \ /
     d

owners:
   A: a, ab, ac
   B: b, bd
   C: c, cd
   D: d
after ownership propagation:
   A: a, ab, ac, b, c
   B: b, bd, d
   C: c, cd, d
   D: d
-}
mkGraph :: Env -> Repo -> IO ()
mkGraph env repo =
  writeFactsIntoDB env repo [ Glean.Test.allPredicates ] $ do
    d <- withUnit "D" $
      makeFact @Glean.Test.Node (Glean.Test.Node_key "d")
    c <- withUnit "C" $ do
      c <- makeFact @Glean.Test.Node (Glean.Test.Node_key "c")
      makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key c d)
      return c
    b <- withUnit "B" $ do
      b <- makeFact @Glean.Test.Node (Glean.Test.Node_key "b")
      makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key b d)
      return b
    withUnit "A" $ do
      a <- makeFact @Glean.Test.Node (Glean.Test.Node_key "a")
      makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key a b)
      makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key a c)

incrementalDB :: Env -> Repo -> Repo -> [ByteString] -> IO Repo
incrementalDB env base inc prune = do
  kickOffTestDB env inc $ \kickOff ->
    kickOff { kickOff_dependencies = Just $
      Dependencies_pruned def {
        pruned_base = base,
        pruned_units = prune,
        pruned_exclude = True
      } }
  return inc

-- | Test ownership propagation and incremental (pruned) DB correctness.
--
-- Base graph built by 'mkGraph':
--
-- @
--      a          owners:
--     / \           nodes: A owns {a,b,c}, B owns {b,d},
--    b   c                 C owns {c,d}, D owns {d}
--     \ /           edges: A owns {a→b, a→c},
--      d                   B owns {b→d}, C owns {c→d}
-- @
--
-- Ownership propagates through references (edges reference their
-- endpoints), so node "d" ends up owned by B|C|D.
--
-- The test then creates several incremental DBs that prune different
-- units and verifies:
--
--  1. Exclude A  → node "a" disappears, but b's edge to d survives.
--  2. Exclude D  → edges to "d" survive (B and C still own them).
--  3. Exclude all → everything disappears.
--  4. Exclude A,B then add new facts forming B→A→C (with C→D from base)
--     → verify the new edge structure.
incrementalTest :: [Setting] -> Test
incrementalTest settings = TestCase $

  -- build a DB with some units and a non-trivial fact graph
  withTestEnv settings $ \env -> do
    let base = Repo "base" "0"
    kickOffTestDB env base id
    mkGraph env base
    completeTestDB env base

    -- check the owners of Node "d"
    results <- runQuery_ env base $ query $
      predicate @Glean.Test.Node (rec $ field @"label" (string "d") end)
    case results of
      [Glean.Test.Node id _] -> do
        ownerExpr <- factOwnership env base (Fid id)
        print ownerExpr
        assertBool "owners" $
          case ownerExpr of
            Just (OrOwners [Unit x, Unit y, Unit z]) ->
              sort [x,y,z] == ["B","C","D"]
            _otherwise -> False
      _ -> assertFailure "query failed"

    -- ------------------------------------------------------------------
    -- exclude unit A

    inc <- incrementalDB env base (Repo "base-inc" "0") ["A"]
    completeTestDB env inc

    -- node "a" does not exist
    results <- runQuery_ env inc $ query $
      predicate @Glean.Test.Node (rec $ field @"label" (string "a") end)
    assertEqual "inc 0" [] results

    -- edge from "b" (and node "b") exists
    results <- runQuery_ env inc $ query $
      predicate @Glean.Test.Edge
        (rec $ field @"parent" (rec $ field @"label" (string "b") end) end)
    assertEqual "inc 1" 1 (length results)

    -- ------------------------------------------------------------------
    -- exclude unit D

    inc <- incrementalDB env base (Repo "base-inc" "1") ["D"]
    completeTestDB env inc

    -- edges from "b"/"c" to "d" (and therefore node "d") still exist
    results <- runQuery_ env inc $ query $
      predicate @Glean.Test.Edge
        (rec $ field @"child" (rec $ field @"label" (string "d") end) end)
    assertEqual "inc 2" 2 (length results)

    -- ------------------------------------------------------------------
    -- exclude all units

    inc <- incrementalDB env base (Repo "base-inc" "2") ["A","B","C","D"]
    completeTestDB env inc

    -- no nodes exist
    results <- runQuery_ env inc $ query $
      predicate @Glean.Test.Node wild
    assertEqual "inc 3" [] results

    -- ------------------------------------------------------------------
    -- exclude A and B, add some new facts

    inc <- incrementalDB env base (Repo "base-inc" "3") ["A","B"]

    -- the new graph will be
    --              B
    --              |
    --              A
    --             /
    --            C
    --            |
    --            D

    writeFactsIntoDB env inc [ Glean.Test.allPredicates ] $ do
      c <- withUnit "C" $ do
        makeFact @Glean.Test.Node (Glean.Test.Node_key "c")
      a <- withUnit "A" $ do
        a <- makeFact @Glean.Test.Node (Glean.Test.Node_key "a")
        makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key a c)
        return a
      withUnit "B" $ do
        b <- makeFact @Glean.Test.Node (Glean.Test.Node_key "b")
        makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key b a)
    completeTestDB env inc

    let
      edgesFrom s = fmap sort $ runQuery_ env inc $ query $
        var $ \n ->
          n `where_` [
            wild .= predicate @Glean.Test.Edge (rec $
              field @"parent" (rec $ field @"label" (string s) end) $
              field @"child" (rec $ field @"label" n end)
            end)
          ]

    -- check the edges
    r <- edgesFrom "c"
    assertEqual "inc 4" ["d"] r

    r <- edgesFrom "a"
    assertEqual "inc 5" ["c"] r

    r <- edgesFrom "b"
    assertEqual "inc 6" ["a"] r

stackedIncrementalTest :: [Setting] -> Test
stackedIncrementalTest settings = TestCase $
  withTestEnv settings $ \env -> do
    let
      deriveAndFinish :: Env -> Repo -> IO ()
      deriveAndFinish env repo = do
        void $ completePredicates env repo (CompletePredicates_axiom def)
        derivePredicate env repo Nothing Nothing
          (parseRef "glean.test.RevEdge") Nothing
        derivePredicate env repo Nothing Nothing
          (parseRef "glean.test.IsParent") Nothing
        derivePredicate env repo Nothing Nothing
          (parseRef "glean.test.SkipRevEdge") Nothing
        completeTestDB env repo

      nodesAre test db ns = do
        results <- runQuery_ env db $ query $ predicate @Glean.Test.Node wild
        print results
        assertEqual test ns
          (sort [ x | Glean.Test.Node _ (Just (
                        Glean.Test.Node_key x)) <- results ])
      edgesAre test db es = do
        edges <- runQuery_ env db $ recursive $ query $
          predicate @Glean.Test.Edge wild
        assertEqual test es
          (sort [ (x,y) | Glean.Test.Edge _ (Just (
                    Glean.Test.Edge_key
                      (Glean.Test.Node _ (Just (Glean.Test.Node_key x)))
                      (Glean.Test.Node _ (Just (Glean.Test.Node_key y)))))
            <- edges ])
      parentsAre test db ns = do
        results <- runQuery_ env db $ recursive $ query $ predicate @Glean.Test.IsParent wild
        print results
        assertEqual test ns
          (sort [ x | Glean.Test.IsParent _ (Just (
                        Glean.Test.Node _ (Just (Glean.Test.Node_key x)))) <- results ])

      revEdgesAre test db es = do
        edges <- runQuery_ env db $ recursive $ query $
          predicate @Glean.Test.RevEdge wild
        assertEqual test es
          (sort [ (x,y) | Glean.Test.RevEdge _ (Just (
                    Glean.Test.RevEdge_key
                      (Glean.Test.Node _ (Just (Glean.Test.Node_key x)))
                      (Glean.Test.Node _ (Just (Glean.Test.Node_key y)))))
            <- edges ])

      skipRevEdgesAre test db es = do
        edges <- runQuery_ env db $ recursive $ query $
          predicate @Glean.Test.SkipRevEdge wild
        assertEqual test es
          (sort [ (x,y) | Glean.Test.SkipRevEdge _ (Just (
                    Glean.Test.SkipRevEdge_key
                      (Glean.Test.Node _ (Just (Glean.Test.Node_key x)))
                      (Glean.Test.Node _ (Just (Glean.Test.Node_key y)))))
            <- edges ])

    let base = Repo "base" "0"
    kickOffTestDB env base id
    mkGraph env base
    deriveAndFinish env base
    closeDatabase env base  -- test closing and re-opening the base DB

    {-
    base:
         a
        / \
       b   c
        \ /
         d
    -}

    inc <- incrementalDB env base (Repo "base-inc" "1") ["A","B"]
    writeFactsIntoDB env inc [ Glean.Test.allPredicates ] $ do
      d <- withUnit "D" $ do
        makeFact @Glean.Test.Node (Glean.Test.Node_key "d")
      a <- withUnit "A" $ do
        a <- makeFact @Glean.Test.Node (Glean.Test.Node_key "a")
        makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key a d)
        return a
      withUnit "B" $ do
        b <- makeFact @Glean.Test.Node (Glean.Test.Node_key "b")
        makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key b a)
    deriveAndFinish env inc
    closeDatabase env base  -- test closing and re-opening the incremental DB

    {-
    inc:
         b
         |
         a   c
          \ /
           d
    -}

    -- nodes are a,b,c,d
    nodesAre "stacked inc 0n" inc ["a", "b", "c", "d"]
    parentsAre "stacked inc 0p" inc ["a", "b", "c"]
    edgesAre "stacked inc 0e" inc [("a","d"),("b","a"),("c","d")]
    revEdgesAre "stacked inc 0r" inc [("a","b"),("d","a"),("d","c")]
    skipRevEdgesAre "stacked inc 0s" inc [("d","b")]

    -- exclude C, and add a new unit E
    inc2 <- incrementalDB env inc (Repo "base-inc" "2") ["C"]
    writeFactsIntoDB env inc2 [ Glean.Test.allPredicates ] $ do
      withUnit "E" $ do
        a <- makeFact @Glean.Test.Node (Glean.Test.Node_key "a")
        e <- makeFact @Glean.Test.Node (Glean.Test.Node_key "e")
        makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key e a)
    deriveAndFinish env inc2

    {-
    inc2:
         b e
         |/
         a
          \
           d
    -}

    nodesAre "stacked inc 2n" inc2 ["a", "b", "d", "e"]
    edgesAre "stacked inc 2e" inc2 [("a","d"),("b","a"),("e","a")]
    parentsAre "stacked inc 2p" inc2 ["a", "b", "e"]
    revEdgesAre "stacked inc 2r" inc2 [("a","b"),("a","e"),("d","a")]
    skipRevEdgesAre "stacked inc 2s" inc2 [("d","b"),("d","e")]

    -- now exclude E
    inc3 <- incrementalDB env inc2 (Repo "base-inc" "3") ["E"]
    deriveAndFinish env inc3

    {-
    inc3:
         b
         |
         a
          \
           d
    -}

    nodesAre "stacked inc 3n" inc3 ["a", "b", "d"]
    edgesAre "stacked inc 3e" inc3 [("a","d"),("b","a")]
    parentsAre "stacked inc 3p" inc3 ["a", "b"]
    revEdgesAre "stacked inc 3r" inc3 [("a","b"),("d","a")]
    skipRevEdgesAre "stacked inc 3s" inc3 [("d","b")]

    -- The new edge E-A should induce retention of A, even if we exclude unit A
    inc4 <- incrementalDB env inc2 (Repo "base-inc" "4") ["A","B","D"]
    deriveAndFinish env inc4

    {-
    inc4:
           e
          /
         a
    -}

    nodesAre "stacked inc 4n" inc4 ["a", "e"]
    edgesAre "stacked inc 4e" inc4 [("e","a")]
    parentsAre "stacked inc 4p" inc4 ["e"]
    revEdgesAre "stacked inc 4r" inc4 [("a","e")]
    skipRevEdgesAre "stacked inc 4s" inc4 []

    inc5 <- incrementalDB env inc (Repo "base-inc" "5") ["C"]
    writeFactsIntoDB env inc5 [ Glean.Test.allPredicates ] $ do
      b <- withUnit "B" $ makeFact @Glean.Test.Node (Glean.Test.Node_key "b")
      withUnit "E" $ do
        e <- makeFact @Glean.Test.Node (Glean.Test.Node_key "e")
        makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key b e)
    deriveAndFinish env inc5

    {-
    inc5:
         b---e
         |
         a
          \
           d
    -}

    nodesAre "stacked inc 5n" inc5 ["a", "b", "d", "e"]
    edgesAre "stacked inc 5e" inc5 [("a","d"),("b","a"),("b","e")]
    parentsAre "stacked inc 5p" inc5 ["a", "b"]
    revEdgesAre "stacked inc 5r" inc5 [("a","b"),("d","a"),("e","b")]
    skipRevEdgesAre "stacked inc 5s" inc5 [("d","b")]

    -- Check ownership of the `IsParent (Node "b")` fact in base-inc/5
    -- It should be B|E, because it was derived from B in the base DB
    -- and from E in the increment.
    results <- runQuery_ env inc5 $ recursive $ query $
      predicate @Glean.Test.IsParent $
        predicate @Glean.Test.Node $ rec $ field @"label" "b" end
    case results of
      [Glean.Test.IsParent id _] -> do
        ownerExpr <- factOwnership env inc5 (Fid id)
        assertBool "IsParent owner" $ case ownerExpr of
          Just (OrOwners [OrOwners [Unit b],OrOwners [Unit e]]) ->
            sort [b,e] == ["B","E"]
          _ -> False
      _ -> assertFailure "query failed"

    inc6 <- incrementalDB env inc5 (Repo "base-inc" "6") ["B"]
      -- excludes the edge b->a but node b remains because it is
      -- mentioned by the edge e->b

    {-
    inc6:
         b---e

         a
          \
           d
    -}

    nodesAre "stacked inc 6n" inc6 ["a", "b", "d", "e"]
    edgesAre "stacked inc 6e" inc6 [("a","d"),("b","e")]
    parentsAre "stacked inc 6p" inc6 ["a", "b"]
    revEdgesAre "stacked inc 6r" inc6 [("d","a"),("e","b")]
    skipRevEdgesAre "stacked inc 6s" inc6 []

    -- Check ownership of the `IsParent (Node "b")` fact in the new DB
    results <- runQuery_ env inc6 $ recursive $ query $
      predicate @Glean.Test.IsParent $
        predicate @Glean.Test.Node $ rec $ field @"label" "b" end
    case results of
      [Glean.Test.IsParent id _] -> do
        ownerExpr <- factOwnership env inc6 (Fid id)
        assertBool "IsParent owner" $ case ownerExpr of
          Just (OrOwners [OrOwners [Unit b],OrOwners [Unit e]]) ->
            sort [b,e] == ["B","E"]
          _ -> False
      _ -> assertFailure "query failed"


stackedIncrementalTest2 :: [Setting] -> Test
stackedIncrementalTest2 settings = TestCase $
  withTestEnv settings $ \env -> do
    let
      deriveAndFinish :: Env -> Repo -> IO ()
      deriveAndFinish env repo = do
        void $ completePredicates env repo (CompletePredicates_axiom def)
        derivePredicate env repo Nothing Nothing
          (parseRef "glean.test.NodePair") Nothing
        completeTestDB env repo

      nodesAre test db ns = do
        results <- runQuery_ env db $ query $ predicate @Glean.Test.Node wild
        print results
        assertEqual test ns
          (sort [ x | Glean.Test.Node _ (Just (
                        Glean.Test.Node_key x)) <- results ])
      pairsAre test db es = do
        edges <- runQuery_ env db $ recursive $ query $
          predicate @Glean.Test.NodePair wild
        assertEqual test es
          (sort [ (x,y) | Glean.Test.NodePair _ (Just (
                    Glean.Test.NodePair_key
                      (Glean.Test.Node _ (Just (Glean.Test.Node_key x)))
                      (Glean.Test.Node _ (Just (Glean.Test.Node_key y)))))
            <- edges ])

    let base = Repo "base" "0"
    kickOffTestDB env base id
    writeFactsIntoDB env base [ Glean.Test.allPredicates ] $ do
      withUnit "X" $ do
        makeFact_ @Glean.Test.Node (Glean.Test.Node_key "a")
      withUnit "Y" $ do
        makeFact_ @Glean.Test.Node (Glean.Test.Node_key "c")
    deriveAndFinish env base

    {-
    base:
         a{X}<--->b{Y}
    -}

    inc <- incrementalDB env base (Repo "base-inc" "1") ["Y"]
    writeFactsIntoDB env inc [ Glean.Test.allPredicates ] $ do
      a <- withUnit "X" $
        makeFact @Glean.Test.Node (Glean.Test.Node_key "a")
      withUnit "Z" $ do
        makeFact_ @Glean.Test.Node (Glean.Test.Node_key "c")
        makeFact_ @Glean.Test.Node (Glean.Test.Node_key "d")
      withUnit "E" $
        makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key a a)
    closeDatabase env inc -- we should be able to close and re-open here
    deriveAndFinish env inc

    {-
    inc:

    c{Z}-----d{Z}
       \    /
        \  /
         a{X} <--- Edge{Z}
    -}
    nodesAre "0n" inc ["a","c","d"]
    pairsAre "0p" inc [
      ("a","c"),("a","d"),("c","a"),("c","d"),("d","a"),("d","c") ]

    -- check the owner of node "a"
    results <- runQuery_ env inc $ query $
      predicate @Glean.Test.Node $ rec $ field @"label" "a" end
    case results of
      [Glean.Test.Node id _] -> do
        ownerExpr <- factOwnership env inc (Fid id)
        print ownerExpr
        assertBool "owners" $
          case ownerExpr of
            -- {X} is the original owner, {{X},X,E} is the propagated
            -- ownership from the stacked DB.
            Just (OrOwners [OrOwners [Unit "X"], Unit x, Unit e]) ->
              sort [x,e] == ["E", "X"]
            _otherwise -> False
      _ -> assertFailure "query failed"

    inc2 <- incrementalDB env inc (Repo "base-inc" "2") ["X"]
    deriveAndFinish env inc2
    -- exclude X, but node "a" should be kept alive by Edge{Z}
    -- and hence the pairs should all still be alive
    {-
    inc2:

    c{Z}-----d{Z}
       \    /
        \  /
         a{X} <--- Edge{Z}
    -}
    nodesAre "1n" inc2 ["a","c","d"]
    pairsAre "1p" inc2 [
      ("a","c"),("a","d"),("c","a"),("c","d"),("d","a"),("d","c") ]

    -- excluding "E" should make "a" and its derived pairs go away
    inc3 <- incrementalDB env inc2 (Repo "base-inc" "3") ["E"]
    deriveAndFinish env inc3
    {-
    inc3:

    c{Z}-----d{Z}
    -}
    nodesAre "1n" inc3 ["c","d"]
    pairsAre "1p" inc3 [("c","d"),("d","c")]

-- tickled a bug in the storage of ownership information
dupSetTest :: [Setting] -> Test
dupSetTest settings = TestCase $
  withTestEnv settings $ \env -> do
    let base = Repo "base" "0"
    kickOffTestDB env base id
    writeFactsIntoDB env base [ Glean.Test.allPredicates ] $ do
      withUnit "A" $
        makeFact_ @Glean.Test.Node (Glean.Test.Node_key "a")
      withUnit "B" $
        makeFact_ @Glean.Test.Node (Glean.Test.Node_key "b")
    writeFactsIntoDB env base [ Glean.Test.allPredicates ] $ do
      withUnit "A" $
        makeFact_ @Glean.Test.Node (Glean.Test.Node_key "aa")
      withUnit "B" $
        makeFact_ @Glean.Test.Node (Glean.Test.Node_key "bb")
    completeTestDB env base

    inc <- incrementalDB env base (Repo "base-inc" "0") ["B"]

    results <- runQuery_ env inc $ query $
      predicate @Glean.Test.Node wild
    assertEqual "dupSetTest" 2 (length results)

orphanTest :: [Setting] -> Test
orphanTest settings = TestCase $
  withTestEnv settings $ \env -> do
    let base = Repo "base" "0"
    kickOffTestDB env base id
    writeFactsIntoDB env base [ Glean.Test.allPredicates ] $ do
      d <- withUnit "D" $
        makeFact @Glean.Test.Node (Glean.Test.Node_key "d")
      c <- withUnit "C" $ do
        c <- makeFact @Glean.Test.Node (Glean.Test.Node_key "c")
        makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key c d)
        return c
      b <- withUnit "B" $ do
        b <- makeFact @Glean.Test.Node (Glean.Test.Node_key "b")
        makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key b d)
        return b
      -- these facts are orphans:
      a <- makeFact @Glean.Test.Node (Glean.Test.Node_key "a")
      makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key a b)
      makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key a c)
    completeTestDB env base

    -- exclude all of B, C, D
    inc <- incrementalDB env base (Repo "base-inc" "0") ["B","C","D"]

    -- no nodes should exist
    results <- runQuery_ env inc $ query $
      predicate @Glean.Test.Node wild
    assertEqual "orphan 0" [] results

    -- no edges should exist
    results <- runQuery_ env inc $ query $
      predicate @Glean.Test.Edge
        (rec $ field @"parent" (rec $ field @"label" (string "a") end) end)
    assertEqual "orphan 1" [] results

    -- exclude just B
    inc <- incrementalDB env base (Repo "base-inc" "1") ["B"]

    -- 2 nodes now
    results <- runQuery_ env inc $ query $
      predicate @Glean.Test.Node wild
    assertEqual "orphan 2" 2 (length results)

    -- one edge
    results <- runQuery_ env inc $ query $
      predicate @Glean.Test.Edge
        (rec $ field @"child" (rec $ field @"label" (string "d") end) end)
    assertEqual "orphan 3" 1 (length results)

externalDerivationTest :: [Setting] -> Test
externalDerivationTest settings = TestList
  [ TestLabel "add fact with dependencies" $ TestCase $
    withDB $ \env repo -> do
      writeFactsIntoDB env repo [ Glean.Test.allPredicates ] $ do
        a <- makeFact @Glean.Test.Node (Glean.Test.Node_key "a")
        b <- makeFact @Glean.Test.Node (Glean.Test.Node_key "b")
        p <- makeFact @Glean.Test.StringPair (Glean.Test.StringPair_key "a" "b")
        derivedFrom [idOf (getId a), idOf (getId b)] [p]

      void $ completePredicates env repo $ CompletePredicates_derived $
        CompleteDerivedPredicate $
        PredicateRef "glean.test.StringPair" 6

      owners <- ownersOf env repo $ query $
        predicate @Glean.Test.StringPair wild

      assertEqual "owners"
        (show [Just $ AndOwners
          [ OrOwners [Unit "B"]
          , OrOwners [Unit "A"]
          ] ])
        (show owners)

  , TestLabel "multiple predicates in the same batch" $ TestCase $
    withDB $ \env repo -> do
      writeFactsIntoDB env repo [ Glean.Test.allPredicates ] $ do
        a <- makeFact @Glean.Test.Node (Glean.Test.Node_key "a")
        p <- makeFact @Glean.Test.StringPair (Glean.Test.StringPair_key "a" "a")
        derivedFrom [idOf (getId a)] [p]

        b <- makeFact @Glean.Test.Node (Glean.Test.Node_key "b")
        n <- makeFact @Glean.Test.Name "b"
        derivedFrom [idOf (getId b)] [n]

      void $ completePredicates env repo $ CompletePredicates_derived $
        CompleteDerivedPredicate $
        PredicateRef "glean.test.StringPair" 6

      void $ completePredicates env repo $ CompletePredicates_derived $
        CompleteDerivedPredicate $
        PredicateRef "glean.test.Name" 6

      owners <- ownersOf env repo $ query $
        predicate @Glean.Test.StringPair wild
      assertEqual "StringPair owners"
        (show [Just $ OrOwners [Unit "A"]])
        (show owners)

      owners <- ownersOf env repo $ query $
        predicate @Glean.Test.Name wild
      assertEqual "Name owners"
        (show [Just $ OrOwners [Unit "B"]])
        (show owners)
  ]
  where
  ownersOf env repo q = do
    results <- runQuery_ env repo q
    let fids = map (idOf . getId) results
    traverse (factOwnership env repo) fids

  withDB act =
    withTestEnv (settings <> [setCompactOnCompletion]) $ \env -> do
      let repo = Repo "base" "0"
      kickOffTestDB env repo id
      writeFactsIntoDB env repo [ Glean.Test.allPredicates ] $ do
        void $ withUnit "A" $
          makeFact @Glean.Test.Node (Glean.Test.Node_key "a")
        void $ withUnit "B" $
          makeFact @Glean.Test.Node (Glean.Test.Node_key "b")
      void $ completePredicates env repo $ CompletePredicates_axiom def
      act env repo

deriveTest :: [Setting] -> Test
deriveTest settings = TestCase $
  withTestEnv (settings <> [setCompactOnCompletion]) $ \env -> do
    let base = Repo "base" "0"
    kickOffTestDB env base id
    mkGraph env base
    void $ completePredicates env base (CompletePredicates_axiom def)

    derivePredicate env base Nothing Nothing
      (parseRef "glean.test.RevEdge") Nothing
    derivePredicate env base Nothing Nothing
      (parseRef "glean.test.SkipRevEdge") Nothing

    completeTestDB env base

    -- check the owners of SkipRevEdge d->a
    results <- runQuery_ env base $ query $
      predicate @Glean.Test.SkipRevEdge $
        rec $ field @"child" (
          rec $ field @"label" (string "d") end
        ) end
    case results of
      [Glean.Test.SkipRevEdge id _] -> do
        ownerExpr <- factOwnership env base (Fid id)
        print ownerExpr
        assertBool "owners" $
          case ownerExpr of
            Just (OrOwners [
                AndOwners [OrOwners [Unit a],OrOwners [Unit b]],
                AndOwners [OrOwners [Unit c],OrOwners [Unit d]]
              ]) ->
              sort [a,b,c,d] == ["A", "A", "B", "C"]
            _otherwise -> False
      _ -> assertFailure "query failed"

    -- should have two RevEdges, d->b and d->c
    results <- runQuery_ env base $ query $
      predicate @Glean.Test.RevEdge
        (rec $ field @"child" (rec $ field @"label" (string "d") end) end)
    assertEqual "derived 1" 2 (length results)

    -- We have one SkipRevEdge fact, d->a
    results <- runQuery_ env base $ query $
      predicate @Glean.Test.SkipRevEdge wild
    assertEqual "derived 2" 1 (length results)

    inc <- incrementalDB env base (Repo "base-inc" "1") ["B"]

    -- should have pruned the RevEdge from d->b
    results <- runQuery_ env inc $ query $
      predicate @Glean.Test.RevEdge
        (rec $ field @"child" (rec $ field @"label" (string "d") end) end)
    assertEqual "derived 3" 1 (length results)

    -- The SkipRevEdge fact d->a should be visible if
    --    (A && B) || (A && C)

    -- visible if we hide B
    results <- runQuery_ env inc $ query $
      predicate @Glean.Test.SkipRevEdge wild
    assertEqual "derived 4" 1 (length results)

    -- invisible if we hide A
    inc <- incrementalDB env base (Repo "base-inc" "2") ["A"]

    results <- runQuery_ env inc $ query $
      predicate @Glean.Test.SkipRevEdge wild
    assertEqual "derived 5" 0 (length results)

    -- visible if we hide D
    inc <- incrementalDB env base (Repo "base-inc" "3") ["D"]

    results <- runQuery_ env inc $ query $
      predicate @Glean.Test.SkipRevEdge wild
    assertEqual "derived 6" 1 (length results)

{-
Build a fact graph with 3-level reference chains and fan-in of 3,
using Node and the chain predicates NodeRef/NodeRef2/NodeRef3.

Each NodeRef has a { label, node } key so multiple NodeRefs can
reference the same Node (the label provides key uniqueness).

  Level 3   r3a(G)  r3b(H)  r3c(I)
              |       |       |
  Level 2   r2a(D)  r2b(E)  r2c(F)
              |       |       |
  Level 1   r1a(A)  r1b(B)  r1c(C)
               \      |      /
  Level 0          n (N)

After propagation:
  r1a: A|D|G   r1b: B|E|H   r1c: C|F|I
  n:   N|A|B|C|D|E|F|G|H|I  (10 units)
-}
mkCompactionGraph :: Env -> Repo -> IO ()
mkCompactionGraph env repo =
  writeFactsIntoDB env repo [ Glean.Test.allPredicates ] $ do
    n <- withUnit "N" $
      makeFact @Glean.Test.Node (Glean.Test.Node_key "n")
    r1a <- withUnit "A" $
      makeFact @Glean.Test.NodeRef (Glean.Test.NodeRef_key "a" n)
    r1b <- withUnit "B" $
      makeFact @Glean.Test.NodeRef (Glean.Test.NodeRef_key "b" n)
    r1c <- withUnit "C" $
      makeFact @Glean.Test.NodeRef (Glean.Test.NodeRef_key "c" n)
    r2a <- withUnit "D" $
      makeFact @Glean.Test.NodeRef2 (Glean.Test.NodeRef2_key "a" r1a)
    r2b <- withUnit "E" $
      makeFact @Glean.Test.NodeRef2 (Glean.Test.NodeRef2_key "b" r1b)
    r2c <- withUnit "F" $
      makeFact @Glean.Test.NodeRef2 (Glean.Test.NodeRef2_key "c" r1c)
    withUnit "G" $
      makeFact_ @Glean.Test.NodeRef3 (Glean.Test.NodeRef3_key "a" r2a)
    withUnit "H" $
      makeFact_ @Glean.Test.NodeRef3 (Glean.Test.NodeRef3_key "b" r2b)
    withUnit "I" $
      makeFact_ @Glean.Test.NodeRef3 (Glean.Test.NodeRef3_key "c" r2c)

-- | Collect all unit names from a (possibly nested) OwnerExpr.
allUnits :: OwnerExpr -> [ByteString]
allUnits (Unit u)       = [u]
allUnits (OrOwners xs)  = concatMap allUnits xs
allUnits (AndOwners xs) = concatMap allUnits xs

-- | Run an IO action with the ownership compact threshold set to 0
-- and the merge cache size set to 2, restoring original values
-- afterwards. The small cache forces frequent evictions, which is
-- needed to reproduce the original use-after-free bug with shared
-- trie owners.
withCompaction :: IO () -> IO ()
withCompaction act = do
  savedThreshold <- getOwnershipCompactThreshold
  savedCache <- getOwnershipMergeCacheSize
  setOwnershipCompactThreshold 0
  setOwnershipMergeCacheSize 2
  act `finally` do
    setOwnershipCompactThreshold savedThreshold
    setOwnershipMergeCacheSize savedCache

-- | Exercise the UsetsMerge compact path with threshold=0 using a
-- fact graph that has 3-level reference chains and fan-in >= 3.
-- With threshold=0, every 'addUset' triggers 'UsetsMerge::compact'.
--
-- See 'mkCompactionGraph' for the graph layout and expected ownership.
incrementalTestWithCompaction :: [Setting] -> Test
incrementalTestWithCompaction settings = TestCase $
  withCompaction $
  withTestEnv settings $ \env -> do
    let base = Repo "base" "0"
    kickOffTestDB env base id
    mkCompactionGraph env base
    completeTestDB env base

    -- n is at the bottom of three 3-level chains, each referencing it.
    -- Transitive propagation gives it all 10 units:
    -- N (own) | A,D,G (chain a) | B,E,H (chain b) | C,F,I (chain c)
    results <- runQuery_ env base $ query $
      predicate @Glean.Test.Node
        (rec $ field @"label" (string "n") end)
    case results of
      [Glean.Test.Node fid _] -> do
        ownerExpr <- factOwnership env base (Fid fid)
        case ownerExpr of
          Just expr ->
            assertEqual "n owners (compact)"
              ["A","B","C","D","E","F","G","H","I","N"]
              (sort (allUnits expr))
          Nothing -> assertFailure "n has no owner"
      _ -> assertFailure "query for n failed"

    -- Exclude G,H,I (level-3 wrappers): level-2 and below
    -- survive via their own units.
    inc <- incrementalDB env base
      (Repo "base-inc" "0") ["G","H","I"]
    completeTestDB env inc

    results <- runQuery_ env inc $ query $
      predicate @Glean.Test.Node
        (rec $ field @"label" (string "n") end)
    assertEqual "compact: n survives" 1 (length results)

    -- Exclude all: nothing remains
    inc <- incrementalDB env base (Repo "base-inc" "1")
      ["A","B","C","D","E","F","G","H","I","N"]
    completeTestDB env inc

    results <- runQuery_ env inc $ query $
      predicate @Glean.Test.Node wild
    assertEqual "compact: all excluded" [] results

{-
Regression test for the use-after-free in the early-merge path.

The crash mechanism: when Usets::merge returns one of its inputs
(because one subsumes the other), the old early-merge code does:

  result = addUsetAndMerge(factId, me)   -- result == me (same ptr)
  usets.drop(me)                          -- drops flatten ref
  me = result                             -- stores me back (no-op)

Later, processFact reads owner(factId) = me and drops it again
— double-drop of the same flatten ref.  This can reduce me.refs
to 0, deleting it while other facts still hold raw pointers.

The subsumption case triggers when a fact's trie owner already
contains the queued unit.  This happens when an edge in unit A
references a node also in unit A: {A} is queued for a node
whose trie owner is already {A}.

This test creates those conditions:
  - Nodes and edges all in unit A (same owner, forces subsumption)
  - Bystander nodes that are never referenced
With threshold=0 and cache_size=2, the double-drops accumulate
and cache evictions expose the freed memory.
-}
sharedOwnerCompactionTest :: [Setting] -> Test
sharedOwnerCompactionTest settings = TestCase $
  withCompaction $
  withTestEnv settings $ \env -> do
    let base = Repo "base" "0"
    kickOffTestDB env base id
    writeFactsIntoDB env base [ Glean.Test.allPredicates ] $ do
      -- Bystander nodes (lower IDs, processed last, share trie owner)
      void $ withUnit "A" $
        makeFact @Glean.Test.Node (Glean.Test.Node_key "f1")
      void $ withUnit "A" $
        makeFact @Glean.Test.Node (Glean.Test.Node_key "f2")
      void $ withUnit "A" $
        makeFact @Glean.Test.Node (Glean.Test.Node_key "f3")
      -- Referenced nodes and edges, ALL in unit A.
      -- Edge in unit A referencing nodes in unit A causes {A} to be
      -- queued for a node with trie owner {A} → subsumption.
      x1 <- withUnit "A" $
        makeFact @Glean.Test.Node (Glean.Test.Node_key "x1")
      x2 <- withUnit "A" $
        makeFact @Glean.Test.Node (Glean.Test.Node_key "x2")
      x3 <- withUnit "A" $
        makeFact @Glean.Test.Node (Glean.Test.Node_key "x3")
      x4 <- withUnit "A" $
        makeFact @Glean.Test.Node (Glean.Test.Node_key "x4")
      x5 <- withUnit "A" $
        makeFact @Glean.Test.Node (Glean.Test.Node_key "x5")
      x6 <- withUnit "A" $
        makeFact @Glean.Test.Node (Glean.Test.Node_key "x6")
      withUnit "A" $ do
        makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key x1 x2)
        makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key x2 x3)
        makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key x3 x4)
        makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key x4 x5)
        makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key x5 x6)
        makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key x6 x1)
    completeTestDB env base

    results <- runQuery_ env base $ query $
      predicate @Glean.Test.Node wild
    assertEqual "shared owner: all nodes exist" 9 (length results)

    -- Bystanders should still have owner A
    results <- runQuery_ env base $ query $
      predicate @Glean.Test.Node
        (rec $ field @"label" (string "f1") end)
    case results of
      [Glean.Test.Node fid _] -> do
        ownerExpr <- factOwnership env base (Fid fid)
        case ownerExpr of
          Just expr ->
            assertEqual "f1 owner" ["A"] (sort (allUnits expr))
          Nothing -> assertFailure "f1 has no owner"
      _ -> assertFailure "query for f1 failed"

restartIndexing :: [Setting] -> Test
restartIndexing settings = TestCase $
  withTestEnv settings $ \env -> do
    let repo = Repo "base" "0"
    kickOffTestDB env repo id
    writeFactsIntoDB env repo [ Glean.Test.allPredicates ] $ do
      withUnit "A" $
        makeFact_ @Glean.Test.Node (Glean.Test.Node_key "a")
    closeDatabase env repo
    -- After closing and re-opening, add more facts to the "A" unit.
    -- This tests loadOwnershipUnitCounters()
    writeFactsIntoDB env repo [ Glean.Test.allPredicates ] $ do
      withUnit "A" $
        makeFact_ @Glean.Test.Node (Glean.Test.Node_key "b")
    completeTestDB env repo

    results <- runQuery_ env repo $ recursive $ query $
      predicate @Glean.Test.Node $ rec $ field @"label" "a" end
    case results of
      [Glean.Test.Node id _] -> do
        ownerExpr <- factOwnership env repo (Fid id)
        assertBool "Node owner" $ case ownerExpr of
          Just (OrOwners [Unit "A"]) -> True
          _ -> False
      _ -> assertFailure "query failed"

main :: IO ()
main = do
  if System.Info.arch == "x86_64"
    then main_
    else do
      putStrLn "Incremental Glean not supported on non-x86_64 (see ownership.{h/cpp})"
      exitWith (ExitFailure 1)

backends :: ([Setting] -> Test) -> Test
backends fn =
  TestList [
    TestLabel lbl (fn settings)
    | (lbl, settings) <- allStorage
    , lbl /= "memory" -- doesn't support ownership yet
  ]

main_ :: IO ()
main_ = withUnitTest $ testRunner $ TestList
  [ TestLabel "incrementalTest" $ backends incrementalTest
  , TestLabel "incrementalTestWithCompaction" $
      backends incrementalTestWithCompaction
  , TestLabel "sharedOwnerCompactionTest" $
      backends sharedOwnerCompactionTest
  , TestLabel "dupSetTest" $ backends dupSetTest
  , TestLabel "orphanTest" $ backends orphanTest
  , TestLabel "deriveTest" $ backends deriveTest
  , TestLabel "externalDerivationTest" $ backends externalDerivationTest
  , TestLabel "stackedIncrementalTest" $ backends stackedIncrementalTest
  , TestLabel "stackedIncrementalTest2" $ backends stackedIncrementalTest2
  , TestLabel "restartIndexing" $ backends restartIndexing
  ]
