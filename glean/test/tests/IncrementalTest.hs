-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE TypeApplications #-}
module IncrementalTest (main) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Default
import Data.List
import Test.HUnit

import TestRunner

import Glean hiding (query)
import Glean.Write (parseRef)
import Glean.Angle
import Glean.Init
import Glean.Database.Types
import Glean.Database.Test
import Glean.Derive
import qualified Glean.Schema.GleanTest as Glean.Test
import qualified Glean.Schema.GleanTest.Types as Glean.Test


{-
     a
    / \
   b   c
    \ /
     d

owners:
  nodes:
   A: a, b, c, d
   B: b, d
   C: c, d
   D: d
  edges:
   A: ab, ac
   B: bd
   C: cd,
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

incrementalTest :: Test
incrementalTest = TestCase $

  -- build a DB with some units and a non-trivial fact graph
  withTestEnv [] $ \env -> do
    let base = Repo "base" "0"
    kickOffTestDB env base id
    mkGraph env base
    completeTestDB env base

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
      edgesFrom s = fmap sort $ runQuery_ env inc $ data_ $
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


-- tickled a bug in the storage of ownership information
dupSetTest :: Test
dupSetTest = TestCase $
  withTestEnv [] $ \env -> do
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

orphanTest :: Test
orphanTest = TestCase $
  withTestEnv [] $ \env -> do
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

deriveTest :: Test
deriveTest = TestCase $
  withTestEnv [] $ \env -> do
    let base = Repo "base" "0"
    kickOffTestDB env base id
    mkGraph env base
    void $ completePredicates env base

    derivePredicate env base Nothing Nothing
      (parseRef "glean.test.RevEdge") Nothing
    derivePredicate env base Nothing Nothing
      (parseRef "glean.test.SkipRevEdge") Nothing

    completeTestDB env base

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


main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "incrementalTest" incrementalTest
  , TestLabel "dupSetTest" dupSetTest
  , TestLabel "orphanTest" orphanTest
  , TestLabel "deriveTest" deriveTest
  ]
