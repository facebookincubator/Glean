{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
module BenchDB (withBenchDB) where

import Control.Monad
import qualified Data.ByteString.Char8 as BC
import Data.Default
import qualified Data.Text as Text

import Glean
import qualified Glean.Backend as Backend
import Glean.Database.Test (withEmptyTestDB, completeTestDB)
import Glean.Database.Open
import Glean.Database.Write.Batch
import qualified Glean.Schema.Sys.Types as Sys
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.GleanTest.Types as Glean.Test
import qualified Glean.Schema.Sys as Sys
import qualified Glean.Schema.Cxx1 as Cxx
import qualified Glean.Schema.GleanTest as Glean.Test
import Glean.Typed

withBenchDB :: Int -> (forall b . Backend b => b -> Repo -> IO a) -> IO a
withBenchDB num act = withEmptyTestDB [] $ \env repo -> do
  withOpenDatabase env repo $ \odb ->
    void $ return odb

  predicates <- Backend.loadPredicates env repo
    [ Sys.allPredicates
    , Cxx.allPredicates
    , Glean.Test.allPredicates
    ]
  batch <- buildBatch predicates Nothing $ do
    blobs <- mapM (makeFact @Sys.Blob)
      [ "x" <> BC.pack (show n) | n <- [1..num] ]

    names <- mapM (makeFact @Cxx.Name)
      [ "x" <> Text.pack (show n) | n <- [1..num] ]
    mapM_ (makeFact @Cxx.FunctionName . Cxx.FunctionName_key_name) names

    mapM_ (makeFact @Glean.Test.Predicate)
      [ def
        { Glean.Test.kitchenSink_string_ = "x" <> Text.pack (show n)
        , Glean.Test.kitchenSink_pred = blob
        , Glean.Test.kitchenSink_sum_ = Glean.Test.KitchenSink_sum__d blob
        , Glean.Test.kitchenSink_named_record_ =
            def { Glean.Test.rec_beta = Glean.Test.Sum_wed True }
        , Glean.Test.kitchenSink_named_sum_ =
            Glean.Test.Sum_wed True
        , Glean.Test.kitchenSink_maybe_ = Nothing
        , Glean.Test.kitchenSink_array_of_nat = map Nat [1 .. fromIntegral n]
        }

      | (n,blob) <- zip [(1::Int)..] blobs
      ]

    ynames <- mapM (makeFact @Cxx.Name)
      [ "y" <> Text.pack (show n) | n <- [1..1000::Int] ]
    mapM_ (makeFact @Cxx.FunctionName . Cxx.FunctionName_key_name) ynames

  void $ syncWriteDatabase env repo batch
  completeTestDB env repo
  act env repo
