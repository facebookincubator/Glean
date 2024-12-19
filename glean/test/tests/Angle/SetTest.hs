{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Angle.SetTest (main) where

import Control.Monad.Trans.Except
import Control.Monad.Except
import Data.Default
import Data.Text

import Glean.Angle.Parser
import Glean.Angle.Types hiding (Nat)
import Glean.Init
import Glean.Query.Typecheck
import Glean.Query.Flatten
import Glean.Query.Thrift as Thrift
import Glean.Schema.Resolve
import Glean.Database.Schema.Types
import Glean.Types

import qualified Data.HashMap.Strict as HashMap
import Data.Set

import Test.HUnit
import TestRunner
import Util.String.Quasi
import Glean.Database.Types

import TestDB

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "set parse" setParseTest
  , TestLabel "set semantics" setSemanticsTest
  ]

-- First version to support sets
v :: AngleVersion
v = AngleVersion 9

setParseTest :: Test
setParseTest = TestList
    [ TestLabel "schema parses" $ TestCase $
        case parseSchemaWithVersion v
            [s|
            schema foo {
            type Foo = set (maybe nat)
            }
            |]
        of
            Left err ->
              assertFailure $ "Parsing schema failed with error:\n" <> err
            Right _ -> return ()
    , TestLabel "query parses" $ TestCase $
        case parseQueryWithVersion  v
            [s| all (all (all Y))
            |]
        of
            Left err ->
              assertFailure $ "Parsing query failed with error:\n" <> err
            Right _ -> return ()
    , TestLabel "query type checks" $ TestCase $
        case parseQueryWithVersion v
            [s| 1 = elements(all(1)) ; 1
            |]
        of
            Left err ->
                assertFailure $ "Parsing query failed with error:\n" <> err
            Right parsed -> do
              r <- runExceptT $ do
                let scope = addTmpPredicate HashMap.empty
                resolved <- liftEither $ runExcept $
                  runResolve v scope (resolveQuery parsed)
                typecheck undefined (defaultTcOpts def v) undefined resolved
              case r of
                Left err ->
                    assertFailure $
                      "Resolving and typechecking query failed with error:\n"
                      <> unpack err
                Right _ -> return ()
    , TestLabel "query flattens" $ TestCase $
        case parseQueryWithVersion v
            [s| 1 = elements(all(1)) ; 1
            |]
        of
            Left err ->
                assertFailure $ "Parsing query failed with error:\n" <> err
            Right parsed -> do
              r <- runExceptT $ do
                let scope = addTmpPredicate HashMap.empty
                resolved <- liftEither $ runExcept $
                  runResolve v scope (resolveQuery parsed)
                (typechecked, _) <-
                  typecheck undefined (defaultTcOpts def v) undefined resolved
                liftEither $ runExcept $
                  flatten DisableRecursion undefined v  False typechecked
              case r of
                Left err ->
                    assertFailure $
                      "Resolving, typechecking and "
                      <> "flatting query failed with error:\n"
                      <> unpack err
                Right _ -> return ()
    ]

setSemanticsTest :: Test
setSemanticsTest = dbTestCase $ \env repo -> do
  r <- runQuery_ env repo $ angleData @(Set Text) [s| all ("foo"|"bar") |]
  assertEqual "two results in a set" r [fromList ["foo", "bar"]]

  r <- runQuery_ env repo $ angleData @Nat [s| elements (all (1|2)) |]
  assertEqual "two separate results" r [Nat 1, Nat 2]

  r <- runQuery_ env repo $ angleData @Nat [s| elements (all 65535) |]
  assertEqual "big nat" r [Nat 65535]

  r <- runQuery_ env repo $ angleData @Nat [s| prim.size (all (1 | 2 | 3))|]
  assertEqual "size" r [Nat 3]
