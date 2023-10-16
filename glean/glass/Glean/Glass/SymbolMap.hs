{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.SymbolMap
  (
    SymbolIndex
  , toSymbolIndex

  -- * helpers
  , toReferences
  , toDefinitions

  ) where

import qualified Data.Map.Strict as Map

import Data.Maybe ( fromMaybe )
import Data.Int ( Int64 )
import Data.List as List ( sortBy, groupBy )
import Data.Ord ( comparing )
import Data.Function as List ( on )

import Glean.Glass.Types
    ( Range(range_lineBegin),
      SymbolX(..),
      DefinitionSymbolX(..),
      ReferenceRangeSymbolX(..) )

import Glean.Glass.Attributes as Attributes
    ( attrMapToList, attrListToMap )

--
-- Resolved range version of the API. No bytespans to leak to client
--

type SymbolIndex = Map.Map Int64 [SymbolX]

-- | Build a sparse map of line numbers to all symbols that span that line
--
-- This is a line-indexed map to symbols whose spans cover that line
-- Lines are 1-indexed.
--
-- This structure is useful for clients that use cursor / pointer navigation
-- to find the symbol under the pointer
--
-- In most indexers we have "invisible" symbols whose span is a container, e.g.
-- in Python, the module span is the entire file. For symbol maps such symbols
-- are not useful, so we only anchor symbols to the first line they appear on,
-- when doing navigation maps.
--
-- We can have mulitple symbols with identical spans
--
toSymbolIndex :: [ReferenceRangeSymbolX] -> [DefinitionSymbolX] -> SymbolIndex
toSymbolIndex theRefs theDefs = Map.unionWith (++) defs refs
  where
    refs = toLineMap $ map referenceToSymbolX theRefs
    defs = toLineMap $ map definitionToSymbolX theDefs

    toLineMap :: [SymbolX] -> SymbolIndex
    toLineMap syms = Map.fromAscList keyvals
      where
        keyvals :: [(Int64, [SymbolX])]
        keyvals =
          map (\xs -> (fst (head xs), map snd xs)) $
          groupBy ((==) `on` fst) $
          sortBy (comparing fst) $
            [ (symbolXToStartLine sym, sym)
            | sym <- syms
            ]

definitionToSymbolX :: DefinitionSymbolX -> SymbolX
definitionToSymbolX DefinitionSymbolX{..} =
  SymbolX {
    symbolX_sym = definitionSymbolX_sym,
    symbolX_range = fromMaybe definitionSymbolX_range
      definitionSymbolX_nameRange,
    symbolX_target = Nothing,
    symbolX_attributes = Attributes.attrListToMap definitionSymbolX_attributes
  }

referenceToSymbolX :: ReferenceRangeSymbolX -> SymbolX
referenceToSymbolX ReferenceRangeSymbolX{..} =
  SymbolX {
    symbolX_sym = referenceRangeSymbolX_sym,
    symbolX_range = referenceRangeSymbolX_range,
    symbolX_target = Just referenceRangeSymbolX_target,
    symbolX_attributes =
      Attributes.attrListToMap referenceRangeSymbolX_attributes
  }

-- | Symbols can span multiple lines (e.g. containers). However for the
-- line-index map, we simply tie symbols to the identifier first line if
-- available, or just to the first line if not.
symbolXToStartLine :: SymbolX -> Int64
symbolXToStartLine SymbolX{..} = range_lineBegin symbolX_range

--
-- Elimination functions for testing
--

-- | Extract references (xrefs) from the symbol map
toReferences :: SymbolIndex -> [ReferenceRangeSymbolX]
toReferences m =
  [ ReferenceRangeSymbolX {
      referenceRangeSymbolX_sym = symbolX_sym,
      referenceRangeSymbolX_range = symbolX_range,
      referenceRangeSymbolX_target = actual_target,
      referenceRangeSymbolX_attributes =
        Attributes.attrMapToList symbolX_attributes
    }
  | lines <- Map.elems m
  , SymbolX { symbolX_target = Just actual_target, .. } <- lines
  ]

-- | Extract definitions from the symbol map
toDefinitions :: SymbolIndex -> [DefinitionSymbolX]
toDefinitions m =
  [ DefinitionSymbolX {
      definitionSymbolX_sym = symbolX_sym,
      definitionSymbolX_range = symbolX_range,
      definitionSymbolX_nameRange = Just symbolX_range,
      definitionSymbolX_attributes =
        Attributes.attrMapToList symbolX_attributes
    }
  | lines <- Map.elems m
  , SymbolX { symbolX_target = Nothing, .. } <- lines
  ]
