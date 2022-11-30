{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Relations.Hack (
    difference
  , patchDescriptions
  ) where

import Data.Maybe ( fromMaybe )
import Data.Text ( Text )
import Data.Map.Strict ( Map )
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HashMap
import Glean.Glass.Types
import qualified Data.Map.Strict as Map
import Glean.Glass.SearchRelated

--
-- Additional filtering rules for what is in scope via inheritance
-- Implements the Hack rules for `X overrides Y syntactically`.
-- as a work around until we have recorded this `extends` relationship
-- in the Hack index.
--
-- - Removes the inherited children that are hidden by local names
-- - And a set of (implicit) `extends` methods that are now hidden
--
-- This is cumbersome to do in Glass. Idealy we index the sym -> sym
-- implict override relation, and do join in glass to remove those filtered
--
-- We need to recursively choose a winner.
--
difference
  :: HashMap SymbolId [SymbolId]
  -> SymbolId
  -> [RelatedLocatedEntities]
  -> [InheritedContainer]
  -> ([InheritedContainer], HashMap SymbolId SymbolId)
difference _edges _sym0 base inherited0 = partitionOverrides seen inherited0
  where
    seen = HashMap.fromList
      [ (localNameOf sym, symIdOf sym)
      | sym <- map childRL base
      ]

--
-- find all syms with multiple names (i.e. one pass, record duplicates
-- container).  in topo order, check duplicates, look up where it is defined
-- elsewhere remove from main map and shift to re-mapped.
--

-- | Knowing what is locally defined and what is inherited
-- Remove overridden inherited methods, and note the relationship
-- as a synthetic searchRelated `extends` result
partitionOverrides
  :: HashMap Text SymbolId -- those that have been seen
  -> [InheritedContainer]
  -> ([InheritedContainer], HashMap SymbolId SymbolId)
partitionOverrides seen inherited =
    (map fst results, HashMap.unions (map snd results))
  where
    results = map filterParent inherited

    filterParent (parent, children) = ((parent, children'), overrides)
      where
        (children', overrides) = go [] HashMap.empty children

    -- paritiion inherited symbols into those that are and are not overridden
    go children overrides [] = (children, overrides)
    go children overrides (ent : ents) =
      case HashMap.lookup (localNameOf ent) seen of
        Just this -> -- exact name locally! shadows the inherited symbol
          let !mapping = HashMap.insert this (symIdOf ent) overrides
          in go children mapping ents
        Nothing -> go (ent : children) overrides ents
          -- no override, retain `ent` as an inherited child

-- the raw marshalled tuple from the search result queries is a bit annoying
localNameOf :: LocatedEntity -> Text
localNameOf ((_ent, _file, _span, name), _sym) = name

symIdOf :: LocatedEntity -> SymbolId
symIdOf (_, sym) = sym

--
-- We then need to create synthetic `extends` facts for method overrides
-- Each key in `overrides` extends its value. We patch the child with
-- the symbol id and qname of the parent method it extends
--
patchDescriptions
  :: Map Text SymbolDescription
  -> HashMap SymbolId SymbolId
  -> Map Text SymbolDescription
patchDescriptions allsyms overrides = HashMap.foldlWithKey go allsyms overrides
  where
    go syms (SymbolId child) parent@(SymbolId p) = fromMaybe syms $ do
      desc <- Map.lookup child syms
      parentDescription <- Map.lookup p syms
      let qname = symbolDescription_name parentDescription
          childDesc = desc { symbolDescription_extends_relation =
            (symbolDescription_extends_relation desc) {
                relationDescription_firstParent = Just parent,
                relationDescription_firstParentName = Just qname
            }
          }
      return $ Map.insert child childDesc syms
