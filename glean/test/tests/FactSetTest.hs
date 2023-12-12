{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
module FactSetTest where

import Control.Exception (try, SomeException(..))
import Test.HUnit


import Schema.Lib (mkBatch, withSchemaAndFacts)
import TestRunner
import Util.String.Quasi

import Glean.Init
import Glean.Database.Schema.Types (schemaInventory)
import Glean.Types as Thrift
import Glean.RTS.Types (Fid(..))
import Glean.RTS.Foreign.Define (defineBatch, DefineFlags(..))
import qualified Glean.RTS.Foreign.FactSet as FactSet
import qualified Glean.RTS.Foreign.LookupCache as LookupCache
import Glean.Write.JSON (buildJsonBatch)

rebaseTest :: Test
rebaseTest = TestList
  [ TestLabel "duplicate keys after rebase" $ TestCase $
    withSchemaAndFacts []
    [s| schema x.1 {
          predicate P : string
          predicate Q : P
        }
        schema all.1 : x.1 {}
    |] [] $ \_ _ schema -> do
      let inventory = schemaInventory schema

          add factset json = do
            batch <- buildJsonBatch schema Nothing [json]
            defineBatch factset inventory batch flags

      remote <- FactSet.new $ Fid 1024
      local  <- FactSet.new $ Fid 9990

      _ <- add local $ mkBatch (PredicateRef "x.P" 1)
        [ [s|{ "id" : 9990, "key": "A" }|] ]

      -- Add local facts to a 'remote' factset.
      -- The substitution will say that 9991 maps to 1024
      subst <- do
        batch <- FactSet.serialize local
        defineBatch remote inventory batch flags

      _ <- add local $ mkBatch (PredicateRef "x.Q" 1)
        -- After substitution these two will have the same key.
        -- This should not be an error.
        [ [s|{ "id" : 9991, "key": 9990  }|]
        , [s|{ "id" : 9992, "key": 1024  }|]
        ]

      cache <- LookupCache.new 1 1 =<< LookupCache.newStats
      r <- try $ FactSet.rebase inventory subst cache local
      result <- case r of
        Left err@SomeException{} -> assertFailure $ "Failed with: " <> show err
        Right (factset,_) -> return factset

      count <- FactSet.factCount result
      assertEqual "facts created" count 1

  , TestLabel "key/value conflicts after rebase" $ TestCase $
    withSchemaAndFacts []
    [s| schema x.1 {
          predicate P : string
          predicate Q : P -> P
        }
        schema all.1 : x.1 {}
    |] [] $ \_ _ schema -> do
      let inventory = schemaInventory schema

          add factset json = do
            batch <- buildJsonBatch schema Nothing [json]
            defineBatch factset inventory batch flags

      remote <- FactSet.new $ Fid 1000
      local  <- FactSet.new $ Fid 9000

      _ <- add local $ mkBatch (PredicateRef "x.P" 1)
        [ [s|{ "id" : 9000, "key": "A" }|] ]

      -- Add local facts to a 'remote' factset.
      -- The substitution will say that 9001 maps to 1000
      subst <- do
        batch <- FactSet.serialize local
        defineBatch remote inventory batch flags

      -- because 9000 is mapped to 1000 this rebase will detect that the keys
      -- are duplicates, but it should not throw an error.
      _ <- add local $ mkBatch (PredicateRef "x.Q" 1)
        [ [s|{ "id" : 9001, "key": 9000, "value": 8000 }|]
        , [s|{ "id" : 9002, "key": 1000, "value": 1000 }|]
        ]

      cache <- LookupCache.new 1 1 =<< LookupCache.newStats
      r <- try $ FactSet.rebase inventory subst cache local
      result <- case r of
        Left err@SomeException{} -> assertFailure $ "Failed with: " <> show err
        Right (factset,_) -> return factset

      count <- FactSet.factCount result
      assertEqual "facts created" count 1
  ]
  where
    flags = DefineFlags
      { trustRefs = True
      , ignoreRedef = False
      }

main :: IO ()
main = withUnitTest $ testRunner $  TestLabel "FactSet" $ TestList
  [ TestLabel "rebase" rebaseTest
  ]
