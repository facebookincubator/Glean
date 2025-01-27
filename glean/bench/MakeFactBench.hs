{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
module MakeFactBench (main) where

import Criterion.Types
import qualified Data.Text as Text

import Glean
import Glean.Database.Test (withEmptyTestDB)
import Glean.Backend.Types as Backend (loadPredicates)
import qualified Glean.Schema.Cxx1 as Cxx
import qualified Glean.Schema.Cxx1.Types as Cxx
import Glean.Util.Benchmark
import Glean.Typed

main :: IO ()
main = benchmarkMain $ \run -> do
  withEmptyTestDB [] $ \env repo -> do
    predicates <- Backend.loadPredicates env repo
      [ Cxx.allPredicates
      ]
    let
      cxxName =
        buildBatch predicates Nothing $
          mapM_ (makeFact @Cxx.Name)
            [ "x" <> Text.pack (show n) | n <- [1::Int .. 1000] ]

    run
      [ bgroup "makefact"
        [ bench "cxx.Name" $ whnfIO cxxName ]
      ]
