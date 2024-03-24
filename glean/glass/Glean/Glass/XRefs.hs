{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}

module Glean.Glass.XRefs
 (
    GenXRef(..),
    XRef,
   fetchCxxIdlXRefs
  ) where

import Data.Map.Strict ( Map )
import qualified Data.Map as Map
import Data.Bifunctor ( bimap )

import qualified Glean
import Glean.Angle
import Glean.Haxl.Repos as Glean
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.CodemarkupCxx.Types as Code
import Glean.Glass.Utils

type XRef = (Code.XRefLocation, Code.Entity)
type IdlXRef = (Code.RangeSpan, Code.IdlEntity)
data GenXRef = PlainXRef XRef | IdlXRef IdlXRef

type EntityIdlMap = Map Code.Entity Code.IdlEntity

-- | extract idl xrefs from the regular ones
-- simply ignore idl xrefs which don't have an entity.
extractIdlXRefs
  :: EntityIdlMap -> [(Code.XRefLocation, Code.Entity)] -> [GenXRef]
extractIdlXRefs entityIdlMap xRefs =
  let pred (loc, ent) = case Map.lookup ent entityIdlMap of
        Just idl ->
          [PlainXRef (loc, ent), IdlXRef (Code.xRefLocation_source loc, idl)]
        _ -> [PlainXRef (loc, ent)] in
  concatMap pred xRefs

entityIdlCxxMap
  :: Maybe Int
  -> Glean.IdOf Cxx.FileXRefs
  -> Glean.RepoHaxl u w (EntityIdlMap, Bool)
entityIdlCxxMap mlimit xrefId = do
  (rows, truncated) <- searchRecursiveWithLimit mlimit
    (cxxFileEntityIdl xrefId)
  return (Map.fromList rows, truncated)
  where
    cxxFileEntityIdl
      :: Glean.IdOf Cxx.FileXRefs
      -> Angle (Code.Entity, Code.IdlEntity)
    cxxFileEntityIdl xrefId =
      vars $ \(ent :: Angle Code.Entity)
          (idl :: Angle Code.IdlEntity) ->
        tuple (ent, idl) `where_` [
          wild .= predicate @Code.CxxFileEntityIdl (
            rec $
              field @"trace" (asPredicate (factId xrefId)) $
              field @"ent" ent $
              field @"idlEnt" idl
            end)
          ]

-- fetch from Glean the entity-idl map and returns
-- an idl extractor which extract the idl xrefs from
-- the regular ones. Also propagate the "trunc" status.
fetchCxxIdlXRefs :: Maybe Int
  -> Glean.IdOf Cxx.FileXRefs
  -> Glean.RepoHaxl u w
    (([XRef], Bool) -> ([GenXRef], Bool))
fetchCxxIdlXRefs mlimit xrefId =
  do
    (map, trunc) <- entityIdlCxxMap mlimit xrefId
    return $ Data.Bifunctor.bimap (extractIdlXRefs map)(trunc ||)
