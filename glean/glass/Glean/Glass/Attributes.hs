{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Attributes
  ( -- * export known attribute types
    SymbolKind.SymbolKindAttr(..)
    -- * class
  , module Glean.Glass.Attributes.Class

    -- * operating with attributes
  , extendAttributes
  , attrListToMap
  , attrMapToList
  ) where

import qualified Data.Map.Strict as Map

import Glean.Glass.Attributes.Class
    ( ToAttributes(..), SymbolIdentifier, toAttrMap )
import Glean.Glass.Attributes.SymbolKind as SymbolKind
    ( SymbolKindAttr(..) )
import Glean.Glass.Types
    ( KeyedAttribute(KeyedAttribute),
      AttributeList(AttributeList),
      SymbolId,
      DefinitionSymbolX(..),
      ReferenceRangeSymbolX(..),
      Attributes(Attributes) )
import qualified Glean.Schema.Code.Types as Code

type RefEntitySymbol = (Code.Entity, ReferenceRangeSymbolX)
type DefEntitySymbol = (Code.Entity, DefinitionSymbolX)

-- | Given some definitions, combine their attributes from any additional
-- ones in the attribute maps
extendAttributes
  :: (SymbolId -> Code.Entity -> SymbolIdentifier)
  -> Map.Map SymbolIdentifier Attributes
  -> [RefEntitySymbol]
  -> [DefEntitySymbol]
  -> ([RefEntitySymbol], [DefEntitySymbol])
extendAttributes keyFn attrMap theRefs theDefs = (refs, defs)
  where
    defs = map (uncurry extendDef) theDefs
    refs = map (uncurry extendRef) theRefs

    extend symId entity def = case Map.lookup (keyFn symId entity) attrMap of
      Nothing -> def
      Just attr -> attrMapToList attr `combineList` def

    extendRef entity ref@ReferenceRangeSymbolX{..} = (entity,) $
        ref { referenceRangeSymbolX_attributes = attrs }
      where
        attrs = extend referenceRangeSymbolX_sym entity
          referenceRangeSymbolX_attributes

    extendDef entity def@DefinitionSymbolX{..} = (entity,) $
        def { definitionSymbolX_attributes = attrs }
      where
        attrs = extend definitionSymbolX_sym entity
          definitionSymbolX_attributes

-- | Combining attributes as lists
combineList :: AttributeList -> AttributeList -> AttributeList
combineList (AttributeList a) (AttributeList b) = AttributeList (a <> b)

-- | Convert between attribute bag representations
attrMapToList :: Attributes -> AttributeList
attrMapToList (Attributes attrMap) = AttributeList $
    map pair $ Map.toList attrMap
  where
    pair (k,v) = KeyedAttribute k v

-- | Convert attribute list to map keyed by attr key
attrListToMap :: AttributeList -> Attributes
attrListToMap (AttributeList elems) = Attributes $
    Map.fromList $ map unpair elems
  where
    unpair (KeyedAttribute k v) = (k,v)
