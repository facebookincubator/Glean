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
difference
  :: [RelatedLocatedEntities]
  -> [InheritedContainer]
  -> ([InheritedContainer], Map SymbolId SymbolId)
difference locals inherited = partitionOverrides localNames inherited
  where
    localNames = Map.fromList [ (nameOf ent, ent) | ent <- map childRL locals ]

-- | Knowing what is locally defined and what is inherited
-- Remove overridden inherited methods, and note the relationship
-- as a synthetic searchRelated `extends` result
partitionOverrides
  :: Map.Map Text LocatedEntity
  -> [InheritedContainer]
  -> ([InheritedContainer], Map SymbolId SymbolId)
partitionOverrides locals inherited =
    (map fst results, Map.unions (map snd results))
  where
    results = map filterParent inherited

    filterParent (parent, children) = ((parent, children'), overrides)
      where
        (children', overrides) = go [] Map.empty children

    -- paritiion inherited symbols into those that are and are not overridden
    go children overrides [] = (children, overrides)
    go children overrides (ent : ents) = case Map.lookup (nameOf ent) locals of
      Just this -> -- exact name locally! shadows the inherited symbol
        go children (Map.insert (symIdOf this) (symIdOf ent) overrides) ents
      Nothing -> go (ent : children) overrides ents
        -- no override, retain `ent` as an inherited child

-- the raw marshalled tuple from the search result queries is a bit annoying
nameOf :: LocatedEntity -> Text
nameOf ((_ent, _file, _span, name), _sym) = name

symIdOf :: LocatedEntity -> SymbolId
symIdOf (_, sym) = sym

--
-- We then need to create synthetic `extends` facts for method overrides
-- Each key in `overrides` extends its value. We patch the child with
-- the symbol id and qname of the parent method it extends
--
patchDescriptions
  :: Map Text SymbolDescription
  -> Map SymbolId SymbolId
  -> Map Text SymbolDescription
patchDescriptions allsyms overrides = Map.foldlWithKey go allsyms overrides
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
