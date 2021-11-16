{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module QueryBench (main) where

import Control.Concurrent.Async
import Criterion.Types
import Data.Default
import Data.Text (Text)

import Util.String.Quasi

import Glean
import Glean.Query.Thrift.Internal
import qualified Glean.Schema.CodeCxx.Types as Code.Cxx
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.Sys.Types as Sys
import qualified Glean.Schema.Query.Sys.Types as Query.Sys
import qualified Glean.Schema.Query.Cxx1.Types as Query.Cxx
import qualified Glean.Schema.Query.GleanTest.Types as Query.Glean.Test
import qualified Glean.Schema.Codemarkup.Types as Codemarkup
import Glean.Util.Benchmark

import BenchDB

main :: IO ()
main = benchmarkMain $ \run -> withBenchDB 100000 $ \env repo -> do
  let
    simpleQueryThrift = query $ Query.Sys.Blob_with_key "x1"
    nestedQueryThrift = query $ Query.Cxx.FunctionName_with_key $ def
      { Query.Cxx.functionName_key_name = Just $
          Query.Cxx.Name_with_key "x1" }
    complexQueryThrift = query $ Query.Glean.Test.Predicate_with_key $ def
      { Query.Glean.Test.kitchenSink_string_ = Just "x1" }

    nestedAngle :: Query Cxx.FunctionName
    nestedAngle = angle "cxx1.FunctionName { name = \"x1\" }"

    pageQueryThrift :: Glean.Query Cxx.Name
    pageQueryThrift = limit 1 $ mkQuery ("{\"key\":{\"prefix\":\"y\"}}" :: Text)

    pageAngle :: Query Cxx.Name
    pageAngle = limit 1 $ angle "cxx1.Name \"y\".."

    -- this is the slow case for the JSON query API - when a nested
    -- query matches multiple facts (in this case 1000), the
    -- re-serialized query contains all 1000 matches.
    pageNestedQueryThrift :: Glean.Query Cxx.FunctionName
    pageNestedQueryThrift = limit 1 $ mkQuery
      ("{\"key\":{\"name\":{\"key\":{\"prefix\":\"y\"}}}}" :: Text)

    pageNestedAngle :: Query Cxx.FunctionName
    pageNestedAngle = limit 1 $ angle "cxx1.FunctionName { name = \"y\".. }"

    simpleAngle :: Query Sys.Blob
    simpleAngle = angle "sys.Blob \"x1\""

    -- test compiling a complex derived predicate, should let us
    -- spot regressions in the compiler pipeline.
    compile :: Query Code.Cxx.Entity
    compile = angleData @Code.Cxx.Entity $
      "E where search.cxx.SearchByNameAndScope { \"malloc\", global_, E }"

    compile2 :: Query Codemarkup.EntityUses
    compile2 = angleData @Codemarkup.EntityUses $
      [s|
        codemarkup.ResolveLocation {
          entity = E,
          location = {
            location = { span = { length = 6, start = 1308 } },
            file = "foo"
          }
        };
        codemarkup.EntityUses { target = E }
      |]

    compile3 :: Query Codemarkup.FileEntityXRefs
    compile3 = angleData @Codemarkup.FileEntityXRefs $
      [s|
        codemarkup.FileEntityXRefs { file = "foo" }
      |]

  run
    [
      bgroup "thrift"
      [ bench "simple" $ whnfIO $
          runQuery env repo simpleQueryThrift
      , bench "nested" $ whnfIO $
          runQuery env repo nestedQueryThrift
      , bench "complex" $ whnfIO $
          runQuery env repo complexQueryThrift
      , bench "complex-parallel" $ whnfIO
        $ mapConcurrently (\io -> io >>= \x -> x `seq` return x)
        $ replicate 1000
        $ runQuery env repo complexQueryThrift
      , bench "page" $ whnfIO $
          runQuery_ env repo pageQueryThrift
      , bench "pagenested" $ whnfIO $
          runQuery_ env repo pageNestedQueryThrift
      ]
    , bgroup "angle"
      [ bench "simple" $ whnfIO $
          runQuery env repo simpleAngle
      , bench "nested" $ whnfIO $
          runQuery env repo nestedAngle
      , bench "page" $ whnfIO $
          runQuery_ env repo pageAngle
      , bench "pagenested" $ whnfIO $
          runQuery_ env repo pageNestedAngle
      , bench "compile" $ whnfIO $
          runQuery_ env repo compile
      , bench "compile2" $ whnfIO $
          runQuery_ env repo compile2
      , bench "compile3" $ whnfIO $
          runQuery_ env repo compile3
      ]
    ]
