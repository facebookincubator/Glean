{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Util.XRefs (collectXRefTargets) where

import Data.Ord
import qualified Data.Set as Set
import Data.Set (Set)

import Util.List

import Glean.Schema.Cxx1.Types as Cxx
import Glean.Util.Range

-- | Expand the compact xref format we store in the schema into a simple
-- list of byte ranges and xref targets.
collectXRefTargets :: [Cxx.FileXRefs] -> Set (ByteRange, Cxx.XRefTarget)
collectXRefTargets allFileXRefs = Set.fromList $ fixedXRefs <> externalXRefs
  where
    -- There might be multiple cxx.FileXRefMaps if the file is
    -- compiled multiple times with different CPP #defines, for
    -- example.
    uniqMaps = uniqBy (comparing Cxx.fileXRefMap_id)
      [ fileXRefs_key_xmap
      | Cxx.FileXRefs{fileXRefs_key = Just Cxx.FileXRefs_key{..}}
         <- allFileXRefs
      ]

    fixedXRefs =
      [ (span, fixedXRef_target)
      | Cxx.FileXRefMap{fileXRefMap_key =
          Just Cxx.FileXRefMap_key{..}} <- uniqMaps
      , Cxx.FixedXRef{..} <- fileXRefMap_key_fixed
      , span <- relByteSpansToRanges fixedXRef_ranges
      ]

    externalXRefs =
      [ (span, xref)
      | Cxx.FileXRefs{fileXRefs_key = Just Cxx.FileXRefs_key
          { fileXRefs_key_xmap = Cxx.FileXRefMap{
              fileXRefMap_key = Just Cxx.FileXRefMap_key{..}}
          , fileXRefs_key_externals = externals }} <- allFileXRefs
      , let ranges = map relByteSpansToRanges fileXRefMap_key_variable
      , (spans, xref) <- zip ranges externals
      , span <- spans
      ]
