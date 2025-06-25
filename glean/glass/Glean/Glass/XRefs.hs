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
    XlangXRef,
    resolveEntitiesRange,
    buildGenXRefs,
    fetchCxxIdlXRefs
  ) where

import Data.Map.Strict ( Map )

import qualified Data.Map as Map
import Data.Bifunctor ( bimap, Bifunctor (first) )
import Data.List ( foldl' )
import Data.List.NonEmpty ( NonEmpty(..) )
import Util.List ( uniq )

import qualified Glean
import Glean.Angle
import Glean.Util.ToAngle ( ToAngleFull(toAngleFull), Normalize(normalize) )
import Glean.Haxl.Repos as Glean ( RepoHaxl )
import Glean.Glass.Range ( rangeSpanToLocationRange )
import Glean.Glass.Utils
import Glean.Glass.Types ( LocationRange, RepoName(..) )
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Codemarkup.Types as Code
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.CodemarkupCxx.Types as CodeCxx
import qualified Glean.Schema.Src.Types as Src
import Glean.Glass.Query (symbolToEntity)
import Data.Maybe (mapMaybe)
import Control.Monad.Extra (mapMaybeM)

type XRef = (Code.XRefLocation, Code.Entity)
type XlangXRef = (Code.RangeSpan, Either Code.IdlEntity Code.SymbolId)
data GenXRef = PlainXRef XRef | XlangXRef XlangXRef

-- Turn a Codemarkup "genericEntity" (plain or xlang) into
-- the corresponding Glass datastructure
buildGenXRefs :: Code.GenericEntity -> Maybe GenXRef
buildGenXRefs genEntity = case genEntity of
  Code.GenericEntity_xlangEntity (Code.GenericEntity_xlangEntity_ source entity)
    -> Just $ XlangXRef (source, Left entity)
  Code.GenericEntity_xlangSymbol (Code.GenericEntity_xlangSymbol_ source symbol)
    -> Just $ XlangXRef (source, Right symbol)
  Code.GenericEntity_plainEntity (Code.GenericEntity_plainEntity_ xref entity)
    -> Just $ PlainXRef (xref, entity)
  Code.GenericEntity_EMPTY -> Nothing

-- | First fetch the entities for a given list of symbols.
--  Then annotate all the entities with the
--  LocationRange/File of the definitions of these entities.
--
--  Note that the rangespans from the parameters are spans for the
--  references to the entities, not the spans of the entity definitions,
--  but we want to preserve the correspondence.
--
--  This can't be done in Codemarkup for xlang entities as the entity facts
--  and EntityLocation facts may live in different dbs.
resolveEntitiesRange
 :: RepoName
 -> [(Code.SymbolId, Code.RangeSpan)]
 -> [(Code.Entity, Code.RangeSpan)]
 -> Glean.RepoHaxl u w
      [((Code.Entity, Code.RangeSpan), (Src.File, LocationRange))]
resolveEntitiesRange repo symbols ents = do
    entsFromSymbols <- resolveSymbolsToEntities symbols
    resolveEntitiesRanges repo $ uniq $ ents ++ entsFromSymbols

resolveSymbolsToEntities ::
  [(Code.SymbolId, Code.RangeSpan)]
  -> Glean.RepoHaxl u w [(Code.Entity, Code.RangeSpan)]
resolveSymbolsToEntities symbolRanges = do
  -- We want to query Glean for the entities corresponding to the symbols.
  -- This is more efficiently done in bulk so we need to extract
  -- the symbol list from `symbolRanges`.
  -- To preserve the correspondence between symbols and ranges, we
  -- have to build a map from symbol to entities which we look up at the end.
  let symbols = uniq $ fst <$> symbolRanges
  symbolEnts <- fetchSymbolEntities symbols
  let symbolEntsMap = Map.fromList symbolEnts
  pure $ mapMaybe (mapMaybeFst symbolEntsMap) symbolRanges
  where
    mapMaybeFst :: forall k v a. Ord k => Map k v -> (k, a) -> Maybe (v, a)
    mapMaybeFst map (k, x) = (, x) <$> Map.lookup k map

    -- | Query Glean for the entities corresponding to symbols
    fetchSymbolEntities ::
      [Code.SymbolId] -> Glean.RepoHaxl u w [(Code.SymbolId, Code.Entity)]
    fetchSymbolEntities symbols =
      mapMaybeM (fetchDataRecursive . symbolToEntity . toAngleFull) symbols

resolveEntitiesRanges ::
  RepoName
  -> [(Code.Entity, Code.RangeSpan)]
  -> Glean.RepoHaxl u w
      [((Code.Entity, Code.RangeSpan), (Src.File, LocationRange))]
resolveEntitiesRanges repo entRanges = do
  -- We want to query Glean for the locations corresponding to the entities.
  -- This is more efficiently done in bulk so we need to extract
  -- the entity list from `entRanges`.
  -- To preserve the correspondence between entities and their reference ranges,
  -- we have to build a map from entity to range which we look up at the end.
  entityLocs <- entityLocRange repo (fst <$> entRanges)
  let entityLocsMap = Map.fromList $ first normalize <$> entityLocs
  pure $ mapMaybe (withLoc entityLocsMap) entRanges
  where
    withLoc map entRange@(ent, _) =
      (entRange,) <$> Map.lookup (normalize ent) map

    -- | query glean for idl entity locations and
    --   convert those to location ranges
    entityLocRange :: RepoName
      -> [Code.Entity]
      -> RepoHaxl u w [(Code.Entity, (Src.File, LocationRange))]
    entityLocRange reponame refs =
        fetchEntityLocations refs >>= mapM convertSpan
      where
        convertSpan :: (Code.Entity, Code.Location)
          -> RepoHaxl u w (Code.Entity, (Src.File, LocationRange))
        convertSpan (ent, loc) = do
          let file = Code.location_file loc
          range <- rangeSpanToLocationRange reponame file
            (Code.location_location loc)
          return (ent, (file, range))

    -- | query glean for idl entity locations
    fetchEntityLocations ::
      [Code.Entity] -> Glean.RepoHaxl u w [(Code.Entity, Code.Location)]
    fetchEntityLocations ents = do
      -- careful to not rely on fact ids in this query
      let angleEntities = toAngleFull <$> ents
      case angleEntities of
        [] -> return []
        hd : tl -> fst <$>
          searchRecursiveWithLimit Nothing (declarationLocation (hd :| tl))
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

--  Cxx has a specific logic: The only supported xlang xrefs are "IDL" based,
--  and rely on a mapping "generated entity -> idl entity"
type EntityIdlMap = Map Code.Entity Code.IdlEntity

-- For Cxx, fetch from Glean the "generated entity -> idl entity" map
-- and returns an idl extractor which extracts the idl xrefs from
-- the regular ones. Also propagate the "trunc" status.
fetchCxxIdlXRefs :: Maybe Int
  -> Glean.IdOf Cxx.FileXRefs
  -> Glean.RepoHaxl u w
    (([XRef], Bool) -> ([GenXRef], Bool))
fetchCxxIdlXRefs mlimit xrefId =
  do
    (map, trunc) <- entityIdlCxxMap mlimit xrefId
    return $ Data.Bifunctor.bimap (extractIdlXRefs map)(trunc ||)
  where
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
              wild .= predicate @CodeCxx.CxxFileEntityIdl (
                rec $
                  field @"trace" (asPredicate (factId xrefId)) $
                  field @"ent" ent $
                  field @"idlEnt" idl
                end)
              ]

    -- | extract idl xrefs from the regular ones
    extractIdlXRefs
      :: EntityIdlMap -> [(Code.XRefLocation, Code.Entity)] -> [GenXRef]
    extractIdlXRefs entityIdlMap xRefs =
      let pred (loc, ent) = case Map.lookup ent entityIdlMap of
            Just idl ->
              [PlainXRef (loc, ent),
                XlangXRef (Code.xRefLocation_source loc, Left idl)]
            _ -> [PlainXRef (loc, ent)] in
      concatMap pred xRefs
