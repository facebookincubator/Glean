{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module Angle.ErrorTest (main) where

import Control.Exception
import qualified Data.Text as Text
import Test.HUnit

import TestRunner
import Util.String.Quasi

import Glean.Init
import Glean.Query.Thrift as Thrift
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.GleanTest.Types as Glean.Test
import Glean.Types

import TestDB

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "error" angleErrorTests
  ]

-- No need to re-run the error tests with paging, so we separate them out.
angleErrorTests  :: Test
angleErrorTests = dbTestCase $ \env repo -> do
  -- sum type: error to give multiple alternatives
  results <- try $ runQuery_ env repo $ angle @Glean.Test.Predicate
    [s|
      glean.test.Predicate { sum_ = { d = _, c = _ } }
    |]
  print results
  assertBool "angle - sum - too many alts" $
    case results of
      Left (BadQuery s) -> "union type" `Text.isInfixOf` s
      _ -> False

  -- check that we get an error if there's a version mismatch between
  -- query and call site.
  r <- try $ runQuery_ env repo $ angle @Glean.Test.Predicate_1
    [s|
      glean.test.Predicate { }
    |]
  assertBool "angle - type error" $
    case r of
      Left BadQuery{} -> True
      _ -> False

  -- type error when using a type annotation on the lhs
  r <- try $ runQuery_ env repo $ angle @Glean.Test.Predicate_1
    [s|
      P where
      P = glean.test.Predicate { array_of_pred = X };
      glean.test.KitchenSink { nat = 42 } = X
    |]
  print r
  assertBool "angle - type error 2" $
    case r of
      Left (BadQuery s) -> "type error" `Text.isInfixOf` s
      _ -> False

  -- using the wrong predicate name is an error in a nested query
  r <- try $ runQuery_ env repo $ angle @Cxx.FunctionName
    [s|
      cxx1.FunctionName { name = sys.Blob "an".. }
    |]
  print r
  assertBool "angle - nested 3" $
    case r of
      Left (BadQuery x) -> "expected type: cxx1.Name" `Text.isInfixOf` x
      _ -> False

  r <- try $ runQuery_ env repo $ angle @Glean.Test.Predicate
    [s|
      N = cxx1.Name "abc";
      N[..]
    |]
  print r
  assertBool "angle - array generator type error" $
    case r of
      Left (BadQuery x) -> "array element generator" `Text.isInfixOf` x
      _ -> False

  r <- try $ runQuery_ env repo $ angle @Glean.Test.Predicate
    [s|
      unknown.predicate "abc"
    |]
  print r
  assertBool "angle - unknown predicate " $
    case r of
      Left (BadQuery x) -> "not in scope" `Text.isInfixOf` x
      _ -> False

  -- general type error
  r <- try $ runQuery_ env repo $ angle @Glean.Test.Predicate
    [s|
      cxx1.Name 3
    |]
  print r
  assertBool "angle - type error" $
    case r of
      Left (BadQuery x) -> "type error" `Text.isInfixOf` x
      _ -> False

  -- types of generators in sequence don't match
  r <- try $ runQuery_ env repo $ angle @Glean.Test.Predicate
    [s|
      (cxx1.Name "abc") | (cxx1.Type "def")
    |]
  print r
  assertBool "angle - mismatched generator types" $
    case r of
      Left (BadQuery x) ->
        "expected type: cxx1.Name" `Text.isInfixOf` x
      _ -> False

  -- types of array elements don't match
  r <- try $ runQuery_ env repo $ angle @Glean.Test.Predicate
    [s|
      [A,B] where A = cxx1.Name "abc"; B = cxx1.Type "def"
    |]
  print r
  assertBool "angle - mismatched array elements" $
    case r of
      Left (BadQuery x) -> "type mismatch for variable B" `Text.isInfixOf` x
      _ -> False

  r <- try $ runQuery_ env repo $ angle @Cxx.Name
    [s|
      _ = prim.length (_ : [string]); cxx1.Name "xyz"
    |]
  print r
  assertBool "angle - wildcard in expr" $
    case r of
      Left (BadQuery x) -> "unbound variable" `Text.isInfixOf` x
      _ -> False

  r <- try $ runQuery_ env repo $ angle @Glean.Test.Predicate
    [s|
      x = 1; glean.test.Predicate { nat = x }
    |]
  print r
  assertBool "angle - lowercase variable" $
    case r of
      Left (BadQuery x) -> "variable does not begin with an upper-case"
        `Text.isInfixOf` x
      _ -> False

  -- source location of error
  r <- try $ runQuery_ env repo $ angle @Glean.Test.Predicate
    [s|
      A where
        A = glean.test.Node _;
        B = glean.test.Edge _;
        A = B;
    |]
  print r
  assertBool "angle - variable location" $
    case r of
      Left (BadQuery x) -> "line 5, column 9" `Text.isInfixOf` x
      _ -> False

  r <- try $ runQuery_ env repo $ angleData @Nat "1 + never"
  print r
  assertBool
    "angle never - cannot be used in an expression" $
    case r of
      Left (BadQuery x) ->
        "cannot use 'never' in an expression" `Text.isInfixOf` x
      _ -> False

  r <- runQuery_ env repo $ angleData @Nat "A = [1,2][..]; never = A; A"
  print r
  assertEqual
    "angle - never does not match any result"
    r []

  r <- runQuery_ env repo $ angleData @Glean.Test.Predicate
    "A = never : glean.test.Predicate; A"
  print r
  assertEqual
    "angle - works for FactGenerators"
    r []

  -- a negated term has type unit
  r <- try $ runQuery_ env repo $ angleData @Nat "!1 = 2; 3"
  print r
  assertBool "angle - negation type" $
    case r of
      Left (BadQuery x) -> "type error in pattern" `Text.isInfixOf` x
      _ -> False

  -- unbound variables in a negated statement are errors
  r <- try $ runQuery_ env repo $ angleData @Nat "!(A + A)"
  print r
  assertBool "angle - negation unbound" $
    case r of
      Left (BadQuery x) ->
        "unbound variable: A" `Text.isInfixOf` x
      _ -> False
