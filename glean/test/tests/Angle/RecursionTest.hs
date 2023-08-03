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
        case lookupPredicateSourceRef ref LatestSchemaAll dbSchema of
          Left err -> error $ "can't find predicate: " <>
            unpack (showRef ref) <> ": " <> unpack err
          Right details -> predicateKeyType details

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "recursion" recursionTest
  ]
