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
    resolveEntitiesRange,
    buildGenXRefs,
    fetchCxxIdlXRefs
  ) where

import Data.Map.Strict ( Map )
import qualified Data.Map as Map
import Data.Bifunctor ( bimap, Bifunctor (first) )
import Data.List ( foldl' )
import Data.List.NonEmpty

import qualified Glean
import Glean.Angle
import Glean.Util.ToAngle ( ToAngleFull(toAngleFull), Normalize(normalize) )
import Glean.Haxl.Repos as Glean
import Glean.Glass.Range ( rangeSpanToLocationRange )
import Glean.Glass.Utils
import Glean.Glass.Types ( LocationRange, RepoName(..) )
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Codemarkup.Types as Code
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.CodemarkupCxx.Types as Code
import qualified Glean.Schema.Src.Types as Src
import Data.Maybe (catMaybes)

type XRef = (Code.XRefLocation, Code.Entity)
type IdlXRef = (Code.RangeSpan, Code.IdlEntity)
data GenXRef = PlainXRef XRef | IdlXRef IdlXRef

buildGenXRefs :: Code.GenericEntity -> Maybe GenXRef
buildGenXRefs genEntity = case genEntity of
  Code.GenericEntity_xlangEntity (Code.GenericEntity_xlangEntity_ source entity)
    -> Just $ IdlXRef (source, entity)
  Code.GenericEntity_plainEntity (Code.GenericEntity_plainEntity_ xref entity)
    -> Just $ PlainXRef (xref, entity)
  Code.GenericEntity_EMPTY -> Nothing

type EntityIdlMap = Map Code.Entity Code.IdlEntity

-- | extract idl xrefs from the regular ones
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

fetchEntityLocation
 :: [Code.Entity]
 -> Glean.RepoHaxl u w [(Code.Entity, Code.Location)]
fetchEntityLocation ents = do
  -- careful to not rely on fact ids in this query
  let angleEntities = toAngleFull <$> ents
  case angleEntities of
    [] -> return []
    hd : tl ->
      fst <$> searchRecursiveWithLimit Nothing (declarationLocation (hd :| tl))
  where
    declarationLocation
      :: NonEmpty (Angle Code.Entity)
      -> Angle (Code.Entity, Code.Location)
    declarationLocation (hd :| tl) =
      let or_ents = foldl' (.|) hd tl in
      vars $ \ent loc ->
        tuple (ent, loc) `where_` [
          or_ents .= ent,
          wild .= predicate @Code.EntityLocation (
            rec $
              field @"entity" ent $
              field @"location" loc
            end)
          ]

-- | Annotate a list of items keyed by an entity, with the
--  LocationRange/File for that entity. This can't be done
--  in Codemarkup for xlang entities as the entity facts
--  and ent EntityLocation facts may live in different dbs.
resolveEntitiesRange
 :: RepoName
 -> (a -> Code.Entity)
 -> [a]
 -> Glean.RepoHaxl u w [(a, (Src.File, LocationRange))]
resolveEntitiesRange repo key xrefs = do
    let entsToResolve = key <$> xrefs
    entsRange <- entityLocRange repo entsToResolve
    -- normalize fact ids so we can compare entities from
    -- different dbs
    let normalizedEnts = first normalize <$> entsRange
        entityLocRangeMap = Map.fromList normalizedEnts
    return $ catMaybes $ annotate entityLocRangeMap <$> xrefs
    where
      annotate map item =
        let ent = key item in
        (item,) <$> Map.lookup (normalize ent) map

      -- | fetch idl entity location and compute location range
      entityLocRange reponame ents =
          fetchEntityLocation ents >>= mapM convertSpan
        where
          convertSpan (ent, loc) = do
            let file = Code.location_file loc
            range <- rangeSpanToLocationRange reponame file
              (Code.location_location loc)
            return (ent, (file, range))
