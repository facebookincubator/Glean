{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
module ParserTest
  ( main
  ) where

import Data.ByteString.Lazy.Char8 ()
import Data.Either
import Data.List
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Test.HUnit

import TestRunner
import Util.String.Quasi

import Glean.Angle.Parser
import Glean.Angle.Types
import Glean.Init
import Glean.Schema.Resolve

queryParser :: Test
queryParser = TestCase $ do
  let r = parseQuery "cxx1.Name \"foo\""
  either print (putDoc . pretty) r
  assertBool "simple query" $ case r of
    Right (SourceQuery Nothing [
      SourceStatement (Wildcard _) (App _ _pred [String _ _str])]) -> True
    _ -> False

  -- lexer error
  let r = parseQuery "\""
  either print (putDoc . pretty) r
  assertBool "lexical error" $ case r of
    Left s -> "lexical error" `isPrefixOf` s
    _ -> False

  -- parse error
  let r = parseQuery "}"
  either print (putDoc . pretty) r
  assertBool "parse error" $ case r of
    Left s -> "line 1, column 1\nparse error at: }" `isPrefixOf` s
    _ -> False

  let r = parseQuery "A\n    where"
  either print (putDoc . pretty) r
  assertBool "parse error 2" $ case r of
    Left s -> "line 2, column 10\nparse error at: end of string" `isPrefixOf` s
    _ -> False


schemaParser :: Test
schemaParser = TestCase $ do
  -- schema parse errors, with #FILE
  let r = parseAndResolveSchema [s|
#FILE parsertest.angle
    parse error
  |]
  either print (putDoc . pretty . fst) r
  assertBool "schema parser 1" $ case r of
    Left s -> "line 1, column 5\nparsertest.angle:" `isInfixOf` s
    _ -> False

  -- empty enum is not allowed
  let r = parseSchema [s|
    schema a.1 {
      type X = enum {}
    }
  |]
  assertBool "schema parser 2" $ case r of
    Left s -> "parse error at: }" `isInfixOf` s
    _ -> False

  -- test all the constructs
  let r = parseAndResolveSchema [s|
    schema a.1 {}
    schema b.1 {}
    schema a.2 : a.1 {
      import b.1
      type Y =
        {
          x1  : byte,
          x2  : nat,
          x3  : string,
          x4  : bool,
          x5  : [X],
          x6  : {},  # empty record
          x7  : { a : X },  # singleton record
          x8  : { a : X, },  # trailing comma in a record
          x9  : { a : X, b : Y },
          # field names can be keywords:
          x10 : { nat : nat },
          x11 : { a : X | },  # singleton sum type
          x12 : { a : X | b : Y },
          x13 : { a : X | b : Y | }, # trailing |
          x14 : { nothing | just : string },
          x15 : enum { a },
          x16 : enum { a | b },
          x17 : maybe X,
          x18 : maybe (maybe {}),  # parens
        }
      predicate X : Y

      # predicate names can also be keywords
      predicate type : { nat | bool_ }
    }
  |]
  either print (putDoc . pretty . fst) r
  assertBool "schema parser 1" $ case r of
    Right _ -> True
    _ -> False


schemaResolver :: Test
schemaResolver = TestCase $ do
  -- schemas must have versions
  let r = parseAndResolveSchema [s|
    schema test {}
  |]
  assertBool "schema resolver 1" $ case r of
    Left s -> "missing version" `isInfixOf` s
    _ -> False

  -- cycles aren't allowed
  let r = parseAndResolveSchema [s|
    schema test.1 : test.1 {}
  |]
  assertBool "schema resolver 2" $ case r of
    Left s -> "cycle" `isInfixOf` s
    _ -> False

  -- a bigger cycle
  let r = parseAndResolveSchema [s|
    schema test.1 { import test.2 }
    schema test.2 : test.1 {}
  |]
  assertBool "schema resolver 3" $ case r of
    Left s -> "cycle" `isInfixOf` s
    _ -> False

  -- schemas parent must exist
  let r = parseAndResolveSchema [s|
    schema test.2 : test.1 {}
  |]
  assertBool "schema resolver 4" $ case r of
    Left s -> "unknown schema: test.1" `isInfixOf` s
    _ -> False

  -- schema import must exist
  let r = parseAndResolveSchema [s|
    schema test.2 { import test.1 }
  |]
  assertBool "schema resolver 5" $ case r of
    Left s -> "unknown schema: test.1" `isInfixOf` s
    _ -> False

  -- schema import does not re-export
  let r = parseAndResolveSchema [s|
    schema test.1 { type X = nat }
    schema test.2 { import test.1 }
    schema test.3 {
      import test.2
      type Y = test.X
    }
  |]
  either print (putDoc . pretty . fst) r
  assertBool "schema resolver 6" $ case r of
    Left s -> "not in scope: test.X" `isInfixOf` s
    _ -> False

  -- 1. unversioned predicates get the schema version
  -- 2. we can use an explicit version if we want
  let r = parseAndResolveSchema [s|
    schema test.3 {
      predicate X : string
      predicate Y.2 : X.3
    }
    schema foo.1 {
      import test.3
      predicate Z : { a: test.Y.2, b: test.X.3 }
    }
  |]
  either print (putDoc . pretty . fst) r
  assertBool "schema resolver 7" $ isRight r

  -- imported names are not in scope unqualified
  let r = parseAndResolveSchema [s|
    schema test.1 { type X = nat }
    schema test.2 {
      import test.1
      type Y = X
    }
  |]
  either print (putDoc . pretty . fst) r
  assertBool "schema resolver 8" $ case r of
    Left s -> "not in scope: X" `isInfixOf` s
    _ -> False

  -- ambiguity
  let r = parseAndResolveSchema [s|
    schema a.1 { type X = nat }
    schema a.2 { type X = string }
    schema test.1 {
      import a.1
      import a.2
      type Y = a.X
    }
  |]
  either print (putDoc . pretty . fst) r
  assertBool "schema resolver 9" $ case r of
    Left s -> "a.X is ambiguous" `isInfixOf` s
    _ -> False

  -- ambiguity from multiple inheritance
  let r = parseAndResolveSchema [s|
    schema a.1 { type X = nat }
    schema b.1 { type X = string }
    schema test.1 : a.1, b.1 {
      type Y = X
    }
  |]
  either print (putDoc . pretty . fst) r
  assertBool "schema resolver 10" $ case r of
    Left s -> "inherited schemas give multiple definitions for: X" `isInfixOf` s
    _ -> False

  -- resolve ambiguity by specifying a version
  let r = parseAndResolveSchema [s|
    schema a.1 { type X = nat }
    schema a.2 { type X = string }
    schema test.1 {
      import a.1
      import a.2
      type Y = a.X.2
    }
  |]
  either print (putDoc . pretty . fst) r
  assertBool "schema resolver 11" $ isRight r

  -- no ambiguity when we inherit
  let r = parseAndResolveSchema [s|
    schema a.1 { type X = nat }
    schema a.2 : a.1 { type X = string }
    schema test.1 {
      import a.2
      type Y = a.X  # picks the latest version of a.X
    }
  |]
  either print (putDoc . pretty . fst) r
  assertBool "schema resolver 12" $ isRight r

  let r = parseAndResolveSchema [s|
    schema a.1 { type X = nat }
    schema a.2 : a.1 { type X = string }
    schema a.3 : a.2 {
      type Y = X
    }
    # should be legal to inherit from both a.2 and a.3, because X
    # refers to the same thing.
    schema test.1 : a.2, a.3 {}
  |]
  either print (putDoc . pretty . fst) r
  assertBool "schema resolver 12a" $ isRight r

  let r = parseAndResolveSchema [s|
    schema a.1 { type X = nat }
    schema a.2 : a.1 { type X = string }
    schema a.3 : a.2 {
      type Y = X
    }
    # but illegal to inherit from a.1 and a.3, because X refers to
    # different things.
    schema test.1 : a.1, a.3 {}
  |]
  either print (putDoc . pretty . fst) r
  assertBool "schema resolver 12b" $ case r of
    Left s -> "inherited schemas give multiple definitions for: X" `isInfixOf` s
    _ -> False

  -- duplicate definitions
  let r = parseAndResolveSchema [s|
    schema test.1 {
       predicate X : nat
       predicate X.1 : string
    }
  |]
  assertBool "schema resolver 13" $ case r of
    Left s -> "multiple definitions for: test.X.1" `isInfixOf` s
    _ -> False

  -- type and predicate with the same name
  let r = parseAndResolveSchema [s|
    schema test.1 {
       predicate X : nat
       type X = nat
    }
  |]
  assertBool "schema resolver 14" $ case r of
    Left s -> "multiple definitions for: test.X.1" `isInfixOf` s
    _ -> False

  -- duplicate field names
  let r = parseAndResolveSchema [s|
    schema test.1 {
       predicate X : { a: nat, a: string }
    }
  |]
  assertBool "schema resolver 15" $ case r of
    Left s -> "duplicate field: a" `isInfixOf` s
    _ -> False

  -- reserved words are not allowed
  let r = parseAndResolveSchema [s|
    schema a.1 {
      predicate class : string
    }
  |]
  assertBool "schema resolver 16" $ case r of
    Left s -> "class is a reserved word" `isInfixOf` s
    _ -> False


main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "query" queryParser
  , TestLabel "schema" schemaParser
  , TestLabel "resolve" schemaResolver
  ]
