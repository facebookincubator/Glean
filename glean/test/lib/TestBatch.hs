{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE AllowAmbiguousTypes, TypeApplications #-}
module TestBatch (testBatch) where

import Data.Default
import qualified Data.Text as Text

import Glean.Backend (Backend)
import qualified Glean.Backend as Backend
import qualified Glean.Schema.Cxx1 as Cxx
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.Src as Src
import qualified Glean.Schema.Src.Types as Src
import Glean.Typed
import Glean.Types as Thrift

testBatch :: Backend e => Int -> e -> Repo -> IO Thrift.Batch
testBatch max backend repo = do
  predicates <- Backend.loadPredicates backend repo
    [ Cxx.allPredicates
    , Src.allPredicates
    ]
  buildBatch predicates Nothing $ do
    files <- mapM (makeFact @Src.File)
      [ mconcat
          $ Text.pack (show $ n `div` 100)
          : replicate (n `mod` 10) "abcdefh"
          ++ [Text.pack (show $ n `mod` 100)]
        | n <- [1..max] ]

    names <- mapM (makeFact @Cxx.Name)
      [ "x" <> Text.pack (show n) | n <- [1..max] ]

    mapM_ (makeFact @Cxx.FunctionName . Cxx.FunctionName_key_name) names

    mapM_ (makeFact @Cxx.FileXRefMap)
      [ def
          { Cxx.fileXRefMap_key_file = file
          , Cxx.fileXRefMap_key_variable = replicate 20 $ replicate 10 def
              { Src.relByteSpan_offset = toNat 2
              , Src.relByteSpan_length = toNat 10
              }
          }
        | file <- take (max `div` 20) files ]
