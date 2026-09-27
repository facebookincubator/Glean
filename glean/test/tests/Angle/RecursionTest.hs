{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
module Angle.RecursionTest (main) where

import Control.Exception
import Data.Default (def)
import Data.Text (Text, unpack)
import Test.HUnit

import TestRunner
import Util.String.Quasi

import Glean.Database.Schema.Types
import Glean.Database.Config (Config(..))
import Glean.Init
import Glean (userQuery)
import qualified Glean.RTS.Term as RTS
import qualified Glean.RTS.Types as RTS
import Glean.Schema.Util
import Glean.Types as Thrift

import Schema.Lib

enableRecursion :: Config -> Config
enableRecursion settings = settings { cfgEnableRecursion = True }

recursionTest :: Test
recursionTest = TestList
  [ TestLabel "compiles" $ TestCase $ do
    -- doesn't get stuck expanding recursive terms.
    withSchemaAndFacts [enableRecursion]
      [s|
        schema x.1 {
          type Node = nat
          predicate Edge : { from: Node, to: Node }
          predicate Path : { from: Node, to: Node }
            { A, B } where
              (Edge { A, B }) | (Path { A, K }; Edge { K, B })
        }
        schema all.1 : x.1 {}
      |]
      [ mkBatch (PredicateRef "x.Edge" 1)
          [ [s|{ "key": { "from": 1, "to": 2 } }|]
          , [s|{ "key": { "from": 2, "to": 3 } }|]
          ]
      ]
      $ \env repo schema -> do
        response <- runQ env repo [s| x.Path _ |]
        facts <- decodeResultsAs "x.Path.1" schema response
        assertEqual "result content"
          [ RTS.Tuple [ RTS.Nat 1, RTS.Nat 2 ]
          , RTS.Tuple [ RTS.Nat 2, RTS.Nat 3 ]
          , RTS.Tuple [ RTS.Nat 1, RTS.Nat 3 ]
          ]
          facts

  , TestLabel "calculates recursive relation with fixed arguments" $
    TestCase $ do
    withSchemaAndFacts [enableRecursion]
      [s|
        schema x.1 {
          type Node = nat
          predicate Edge : { from: Node, to: Node }
          predicate Path : { from: Node, to: Node }
            { A, B } where
              (Edge { A, B }) | (Path { A, K }; Edge { K, B })
        }
        schema all.1 : x.1 {}
      |]
      -- 1 -> 2 -> 3 -> 4 -> 5
      [ mkBatch (PredicateRef "x.Edge" 1)
          [ [s|{ "key": { "from": 1, "to": 2 } }|]
          , [s|{ "key": { "from": 2, "to": 3 } }|]
          , [s|{ "key": { "from": 3, "to": 4 } }|]
          , [s|{ "key": { "from": 4, "to": 5 } }|]
          ]
      ]
      $ \env repo schema -> do
        response <- runQ env repo [s| x.Path { 1, _ } |]
        facts <- decodeResultsAs "x.Path.1" schema response
        assertEqual "result content"
          [ RTS.Tuple [ RTS.Nat 1, RTS.Nat 2 ]
          , RTS.Tuple [ RTS.Nat 1, RTS.Nat 3 ]
          , RTS.Tuple [ RTS.Nat 1, RTS.Nat 4 ]
          , RTS.Tuple [ RTS.Nat 1, RTS.Nat 5 ]
          ]
          facts

  , TestLabel "non-linear recursion typechecks" $ TestCase $ do
    withSchemaAndFacts [enableRecursion]
      [s|
        schema x.1 {
          type Node = nat
          predicate Edge : { from: Node, to: Node }
          predicate Path : { from: Node, to: Node }
            { A, B } where
              (Path { A, X }; Path { X, B }) | Edge { A, B }
        }
        schema all.1 : x.1 {}
      |]
      [ mkBatch (PredicateRef "x.Edge" 1)
          [ [s|{ "key": { "from": 1, "to": 2 } }|]
          ]
      ]
      $ \_ _ _ -> return ()

  , TestLabel "mutual recursion typechecks" $ TestCase $ do
    withSchemaAndFacts [enableRecursion]
      [s|
        schema x.1 {
          predicate P : nat
          predicate Q : nat
          predicate R : nat
            A where P A | S A
          predicate S : nat
            A where Q A | R A
        }
        schema all.1 : x.1 {}
      |]
      [ mkBatch (PredicateRef "x.P" 1)
          [ [s|{ "key": 1 }|]
          ]
      ]
      $ \_ _ _ -> return ()

  , TestLabel "cycle closed by a later derive declaration" $ TestCase $ do
    -- P is declared without a derivation in x.1 and only gets one in x.2,
    -- closing the cycle P -> Q -> P across schemas.
    withSchemaAndFacts [enableRecursion]
      [s|
        schema x.1 {
          predicate Base : nat
          predicate P : nat
          predicate Q : nat
            A where x.P.1 A
        }
        schema x.2 : x.1 {
          derive x.P.1
            A where x.Base.1 A | x.Q.1 A
        }
        schema all.1 : x.2 {}
      |]
      [ mkBatch (PredicateRef "x.Base" 1)
          [ [s|{ "key": 1 }|]
          , [s|{ "key": 2 }|]
          ]
      ]
      $ \env repo schema -> do
        p <- decodeResultsAs "x.P.1" schema =<< runQ env repo [s| x.P.1 _ |]
        assertEqual "P uses the derivation from x.2"
          [ RTS.Nat 1, RTS.Nat 2 ] p
        q <- decodeResultsAs "x.Q.1" schema =<< runQ env repo [s| x.Q.1 _ |]
        assertEqual "Q sees P's derivation from x.2"
          [ RTS.Nat 1, RTS.Nat 2 ] q
  ]
  where
    runQ env repo query =
      try $ userQuery env repo $ def
        { userQuery_query = query
        , userQuery_options = Just def
          { userQueryOptions_syntax = QuerySyntax_ANGLE
          , userQueryOptions_recursive = True
          , userQueryOptions_collect_facts_searched = True
          , userQueryOptions_debug = def
            { queryDebugOptions_bytecode = False
            , queryDebugOptions_ir = False
            }
          }
        , userQuery_encodings = [ UserQueryEncoding_bin def ]
        }

    decodeResultsAs
      :: Text
      -> DbSchema
      -> Either BadQuery UserQueryResults
      -> IO [RTS.Value]
    decodeResultsAs ref schema eresults = do
      res <- decodeResults
        (keyType (parseRef ref) schema) userQueryResultsBin_facts eresults
      either assertFailure return res
      where
      keyType
        :: SourceRef
        -> DbSchema
        -> RTS.Type
      keyType ref dbSchema =
        case lookupPredicateSourceRef ref LatestSchema dbSchema of
          Left err -> error $ "can't find predicate: " <>
            unpack (showRef ref) <> ": " <> unpack err
          Right details -> predicateKeyType details

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "recursion" recursionTest
  ]
