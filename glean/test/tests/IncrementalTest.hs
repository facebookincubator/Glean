{-# LANGUAGE TypeApplications #-}
module IncrementalTest (main) where

import Data.Default
import Data.List
import Test.HUnit

import TestRunner

import Glean hiding (query)
import Glean.Angle
import Glean.Init
import Glean.Database.Test
import qualified Glean.Schema.GleanTest as Glean.Test
import qualified Glean.Schema.GleanTest.Types as Glean.Test


incrementalTest :: Test
incrementalTest = TestCase $

  -- build a DB with some units and a non-trivial fact graph
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
      withUnit "A" $ do
        a <- makeFact @Glean.Test.Node (Glean.Test.Node_key "a")
        makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key a b)
        makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key a c)
    completeTestDB env base

    -- ------------------------------------------------------------------
    -- exclude unit A

    let inc = Repo "base-inc" "0"
    kickOffTestDB env inc $ \kickOff ->
      kickOff { kickOff_dependencies = Just $
        Dependencies_pruned def {
          pruned_base = base,
          pruned_units = ["A"],
          pruned_exclude = True
        } }
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

    let inc = Repo "base-inc" "1"
    kickOffTestDB env inc $ \kickOff ->
      kickOff { kickOff_dependencies = Just $
        Dependencies_pruned def {
          pruned_base = base,
          pruned_units = ["D"],
          pruned_exclude = True
        } }
    completeTestDB env inc

    -- edges from "b"/"c" to "d" (and therefore node "d") still exist
    results <- runQuery_ env inc $ query $
      predicate @Glean.Test.Edge
        (rec $ field @"child" (rec $ field @"label" (string "d") end) end)
    assertEqual "inc 2" 2 (length results)

    -- ------------------------------------------------------------------
    -- exclude all units

    let inc = Repo "base-inc" "2"
    kickOffTestDB env inc $ \kickOff ->
      kickOff { kickOff_dependencies = Just $
        Dependencies_pruned def {
          pruned_base = base,
          pruned_units = ["A","B","C","D"],
          pruned_exclude = True
        } }
    completeTestDB env inc

    -- no nodes exist
    results <- runQuery_ env inc $ query $
      predicate @Glean.Test.Node wild
    assertEqual "inc 3" [] results

    -- ------------------------------------------------------------------
    -- exclude A and B, add some new facts

    let inc = Repo "base-inc" "3"
    kickOffTestDB env inc $ \kickOff ->
      kickOff { kickOff_dependencies = Just $
        Dependencies_pruned def {
          pruned_base = base,
          pruned_units = ["A","B"],
          pruned_exclude = True
        } }

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

    let inc = Repo "base-inc" "0"
    kickOffTestDB env inc $ \kickOff ->
      kickOff { kickOff_dependencies = Just $
        Dependencies_pruned def {
          pruned_base = base,
          pruned_units = ["B"],
          pruned_exclude = True
        } }

    results <- runQuery_ env inc $ query $
      predicate @Glean.Test.Node wild
    assertEqual "dupSetTest" 2 (length results)

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "incrementalTest" incrementalTest
  , TestLabel "dupSetTest" dupSetTest
  ]
