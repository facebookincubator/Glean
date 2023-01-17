{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
module QueryBench (main) where

import Control.Concurrent.Async
import Control.Monad (void)
import Criterion.Types
import Data.Default

import Util.String.Quasi

import Glean
import Glean.Angle as Angle
import qualified Glean.Schema.CodeCxx.Types as Code.Cxx
import qualified Glean.Schema.Codemarkup.Types as Codemarkup
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.GleanTest.Types as Glean.Test
import qualified Glean.Schema.Sys.Types as Sys
import Glean.Util.Benchmark

import BenchDB

main :: IO ()
main = benchmarkMain $ \run -> withBenchDB 10000 $ \env repo -> do
  let
    nestedAngle :: Query Cxx.FunctionName
    nestedAngle = angle "cxx1.FunctionName { name = \"x1\" }"

    pageAngle :: Query Cxx.Name
    pageAngle = limit 1 $ angle "cxx1.Name \"y\".."

    pageNestedAngle :: Query Cxx.FunctionName
    pageNestedAngle = limit 1 $ angle "cxx1.FunctionName { name = \"y\".. }"

    simpleAngle :: Query Sys.Blob
    simpleAngle = angle "sys.Blob \"x1\""

    complexQueryAngle :: Query Glean.Test.Predicate
    complexQueryAngle = Angle.query $ predicate @Glean.Test.Predicate $ rec $
      field @"string_" (string "x1") end

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

    compile3 :: Query Codemarkup.FileEntityXRefLocations
    compile3 = angleData @Codemarkup.FileEntityXRefLocations $
      [s|
        codemarkup.FileEntityXRefLocations { file = "foo" }
      |]

    arrayPrefix :: Query Nat
    arrayPrefix = angleData @Nat
      [s| N where
         glean.test.Predicate { array_of_nat = [N, ..]}
       |]

  let stacked = Repo "test" "stacked"
  let
    dependencies =
      Dependencies_stacked $ Stacked name hash Nothing
      where (Repo name hash) = repo
  void $ kickOffDatabase env def
    { kickOff_repo = stacked
    , kickOff_fill = Just $ KickOffFill_writeHandle ""
    , kickOff_dependencies = Just dependencies
    }
  run
      [ bench "complex-parallel" $ whnfIO
        $ mapConcurrently (\io -> io >>= \x -> x `seq` return x)
        $ replicate 1000
        $ runQuery env repo complexQueryAngle
      , bench "simple" $ whnfIO $
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
      , bench "array_prefix" $ whnfIO $
          runQuery_ env repo arrayPrefix
      , bgroup "stacked"
        [ bench "page" $ whnfIO $
            runQuery_ env repo pageAngle
        , bench "pagenested" $ whnfIO $
            runQuery_ env repo pageNestedAngle
        ]
      ]
