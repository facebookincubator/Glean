{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
module ApiTest (main) where

import Control.Exception
import Data.ByteString (ByteString)
import Data.Text (Text)
import Glean.Query.Angle
import Glean.Query.Thrift
import Glean.Init
import qualified Glean.Schema.GleanTest.Types as Glean.Test
import Glean.Types
import Test.HUnit
import TestRunner
import TestDB

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "angle DSL" angleDSL
  ]

angleDSL :: Test
angleDSL = dbTestCase $ \env repo -> do
  results <- runQuery env repo $ query $
    predicate @Glean.Test.Predicate $
      rec $
        field @"string_" "acca" $
        field @"nat" (nat 42) $
        field @"array_of_nat" (array [nat 3, nat 4, nat 5]) $
        field @"bool_" false
      end
  assertEqual "angle - DSL 0" 1 (length results)

  results <- runQuery env repo $ query $
    var $ \p -> p `where_`
      [ wild .= predicate @Glean.Test.Predicate (
          rec $
            field @"sum_" (
              alt @"c" (asPredicate p))
          end)
      , p .= predicate @Glean.Test.Predicate (
          rec $
            field @"string_" "abba"
          end)
      ]
  assertEqual "angle - DSL 1" 1 (length results)

  results <- runQuery env repo $ query $
    predicate @Glean.Test.Tree $
      rec $
        field @"left" (just wild) $
        field @"right" nothing
      end
  assertEqual "angle - DSL 2" 1 (length results)

  results <- runQuery_ env repo $ query $
    vars $ \x y ->
      tuple (x,y) `where_` [
        x .= nat 1,
        y .= nat 2
      ]
  assertEqual "angle - DSL 3" [(Nat 1, Nat 2)] results

  results <- runQuery_ env repo $ query $
    vars $ \x ->
      x `where_` [
        x .= elementsOf (array [nat 1, nat 2, nat 3 ]),
        not_ [x .= (nat 1 .| nat 3)]
      ]
  assertEqual "angle - DSL 4" [Nat 2] results

  -- This is just a compile test, inference should not fail with an
  -- ambiguous type error.
  _results <- runQuery_ env repo $ query $
    vars $ \(x :: Angle Text) ->
      x `where_` [
        wild .= predicate @Glean.Test.Node (rec $ field @"label" x end)
      ]

  results <- runQuery_ env repo $ query $
    var $ \p -> p `where_` [p .= sig (never @Glean.Test.Predicate)]
  assertEqual "angle - DSL 4" 0 (length results)

  results <- runQuery_ env repo $ query  @((), Bool, Text) $
    var $ \p -> p `where_`
      [ wild .= sig (wild @Glean.Test.Predicate)
      , wild .= sig unit
      , wild .= sig true
      , wild .= sig (string "a")
      , wild .= sig (nat 1)
      , wild .= sig @Glean.Test.Sum (alt @"wed" true)
      , wild .= sig (array [nat 1, nat 2])
      , wild .= sig (nothing :: Angle (Maybe Glean.Test.Predicate))
      , wild .= sig (tuple (unit, true))
      , p .= sig (tuple (unit, true, string "a"))
      ]
  assertEqual "angle - DSL type signatures" 1 (length results)

  -- test for a bug in which RecordFields and SumFields gave the wrong
  -- type for a predicate nested inside an array or maybe type.
  results <- runQuery_ env repo $ query @[Glean.Test.Predicate] $
    var $ \p -> p `where_` [
      wild .= predicate @Glean.Test.Predicate (
        rec $ field @"array_of_pred" p end)
    ]
  assertEqual "angle - DSL 5" 2 (length (concat results))

  -- test for type inference with the DSL. If we don't tell the compiler
  -- what type we're expecting for the variable `p` it will assume
  -- the type of the field (Sys.Blob) rather than the key type (ByteString)
  -- and we'll get a deserialization error.
  r <- try $ runQuery_ env repo $
    query @(ByteString,ByteString) $
      var $ \p -> tuple (p,p) `where_` [
        wild .= predicate @Glean.Test.Predicate (
          rec $
           field @"pred" p
          end)
      ]
  print r
  assertBool "angle - DSL 6" $ case r of
    Left BadQuery{} -> True
    _ -> False

  -- Test signatures around `elementsOf`. T192115813
  r <- runQuery_ env repo $
    query @Nat $
      sig (elementsOf (array [nat 1, nat 2]))
  print r
  assertEqual "angle - sig elementsOf" r [Nat 1, Nat 2]

  -- Test "expanding"
  r <- runQuery_ env repo $
    keys $
    expanding @Glean.Test.Node $
    query @Glean.Test.Tree $
      predicate @Glean.Test.Tree (
        rec $
          field @"node" (rec $
            field @"label" "a"
          end)
        end)
  print r
  assertBool "angle - expanding" $ case r of
    [Glean.Test.Tree_key
      { tree_key_node = Glean.Test.Node
        { node_key = Just{} }
      , tree_key_left = Just Glean.Test.Tree { tree_key = Nothing }
      , tree_key_right = Just Glean.Test.Tree { tree_key = Nothing }
      }] -> True
    _ -> False

  r <- runQuery_ env repo $
        query $ var $ \x -> zipp x x `where_` [
          stmt $
            predicate @Glean.Test.Predicate (
                rec $
                  field @"nat" (nat 42) $
                  field @"array_of_nat" x
                end)
        ]
  print r
  assertEqual "angle - DSL zip" [[(Nat 1, Nat 1),(Nat 2, Nat 2)]] r

  r <- runQuery_ env repo $
        query $ var $ \x -> conc x x `where_` [
          stmt $
            predicate @Glean.Test.Predicate (
                rec $
                  field @"nat" (nat 42) $
                  field @"array_of_nat" x
                end
            )
        ]
  print r
  assertEqual "concat" [[Nat 1, Nat 2, Nat 1, Nat 2]] r
