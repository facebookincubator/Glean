{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Neighborhood ( searchNeighborhood) where

import Data.Ord ( comparing )
import Data.Text ( Text )
import qualified Data.Map.Strict as Map
import Data.HashMap.Strict ( HashMap )
import Data.HashSet ( HashSet )
import qualified Data.HashMap.Strict as HashMap

import Util.List ( uniq, uniqBy )

import Glean.Haxl.Repos as Glean ( withRepo, RepoHaxl, ReposHaxl )

import qualified Glean.Schema.Code.Types as Code

import Glean.Glass.Utils ( fst4 )
import Glean.Glass.Describe
    ( mkSymbolDescription, mkBriefSymbolDescription )
import Glean.Glass.Repos ( Language(Language_Hack), ScmRevisions )
import qualified Glean.Glass.Relations.Hack as Hack
import Glean.Glass.SymbolKind ( findSymbolKind )
import Glean.Glass.Types
    ( SymbolId(SymbolId),
      RelatedNeighborhoodRequest(..),
      SymbolDescription,
      RelatedNeighborhoodResult(..),
      InheritedSymbols(..),
      RelatedSymbols(RelatedSymbols),
      RelationDirection(RelationDirection_Parent,
                        RelationDirection_Child),
      RelationType(RelationType_Extends, RelationType_Contains,
                   RelationType_RequireExtends, RelationType_RequireImplements,
                   RelationType_RequireClass),
      RepoName,
      SymbolBasicDescription,
      SymbolKind )
import Glean.Glass.Search as Search
    ( CodeEntityLocation(CodeEntityLocation, entityName, entityRange,
                         entityFile, entity),
      SearchEntity(..) )
import Glean.Glass.SearchRelated (InheritedContainer)
import qualified Glean.Glass.SearchRelated as Search
import Glean.Glass.Search.Class ( ResultLocation )

type SearchRelatedQuery u w = Code.Entity -> RepoName
      -> RepoHaxl u w [Search.RelatedLocatedEntities]
type SearchRelatedList u w = Code.Entity -> RepoName
      -> RepoHaxl u w [Search.LocatedEntity]

-- being careful to keep things with clear data dependencies
searchNeighborhood
  :: Int
  -> RelatedNeighborhoodRequest
  -> SymbolId
  -> RepoName
  -> ScmRevisions -> Language -> SearchEntity (ResultLocation Code.Entity)
  -> ReposHaxl u w RelatedNeighborhoodResult
searchNeighborhood limit
    RelatedNeighborhoodRequest{..} sym repo scmRevs lang baseEntity =
  withRepo (entityRepo baseEntity) $ do
    let entity = fst4 $ decl baseEntity
    a <- childrenContains1Level
          (fromIntegral relatedNeighborhoodRequest_children_limit) entity repo
    b <- childrenExtends1Level
          (fromIntegral relatedNeighborhoodRequest_inherited_limit) entity repo
    c <- parentContainsNLevel
          (fromIntegral relatedNeighborhoodRequest_parent_depth) entity repo
    d <- parentExtends1Level limit entity repo
    e <- parentRequire RelationType_RequireExtends limit entity repo
    f <- parentRequire RelationType_RequireImplements limit entity repo
    g <- parentRequire RelationType_RequireClass limit entity repo
    -- all contained symbols of inherited parents
    -- n.b. not all are actually in scope after we resolve names
    (eFull, edges, kinds) <- inheritedNLevel limit
        (fromIntegral relatedNeighborhoodRequest_inherited_limit)
        (if relatedNeighborhoodRequest_hide_uninteresting
          then Search.HideUninteresting
          else Search.ShowAll)
        entity repo
    -- now filter out any names that are shadowed
    let (!eFinal, _) = partitionInheritedScopes lang sym edges
          kinds a eFull
    -- syms visible to the client, we need their full details
    let !syms = uniqBy (comparing snd) $ fromSearchEntity sym baseEntity :
            a ++ flattenEdges c ++ concatMap snd eFinal
              -- full descriptions of final methods
    descriptions <- Map.fromAscList <$> mapM (mkDescribe repo scmRevs) syms
    -- brief descriptions for inherited things
    basics <- Map.fromAscList <$> mapM (mkBriefDescribe repo scmRevs)
      (uniqBy (comparing snd) $ (b ++ d ++ e ++ f ++ g) ++ map fst eFinal)

    return $ RelatedNeighborhoodResult {
        relatedNeighborhoodResult_childrenContained = map snd a,
        relatedNeighborhoodResult_childrenExtended = map snd b,
        relatedNeighborhoodResult_containsParents = symbolIdPairs c,
        relatedNeighborhoodResult_parentsExtended = map snd d,
        relatedNeighborhoodResult_inheritedSymbols =
          map inheritedSymbolIdSets eFinal,
        relatedNeighborhoodResult_symbolDetails = descriptions,
        relatedNeighborhoodResult_symbolBasicDetails = basics,
        relatedNeighborhoodResult_requireExtends = map snd e,
        relatedNeighborhoodResult_requireImplements = map snd f,
        relatedNeighborhoodResult_requireClass = map snd g
      }


-- building map of sym id -> descriptions, by first occurence
mkDescribe
  :: RepoName
  -> ScmRevisions
  -> Search.LocatedEntity
  -> Glean.RepoHaxl u w (Text, SymbolDescription)
mkDescribe repo scmRevs e@(_,SymbolId rawSymId) =
  (rawSymId,) <$> describe repo scmRevs e

describe
   :: RepoName
  -> ScmRevisions
  -> Search.LocatedEntity
  -> Glean.RepoHaxl u w SymbolDescription
describe repo scmRevs ((entity, entityFile, entityRange, entityName), symId)
  = mkSymbolDescription symId scmRevs repo CodeEntityLocation{..} Nothing

-- for children by inheritance, we only need name, kind, parent name, sym id
-- and signature. In particular, we don't e.g. need comments or annotations
-- or type xrefs
mkBriefDescribe
  :: RepoName
  -> ScmRevisions
  -> Search.LocatedEntity
  -> Glean.RepoHaxl u w (Text, SymbolBasicDescription)
mkBriefDescribe repo scmRevs e@(_,SymbolId rawSymId) =
  (rawSymId,) <$> briefDescribe repo scmRevs e

briefDescribe
  :: RepoName
  -> ScmRevisions
  -> Search.LocatedEntity
  -> Glean.RepoHaxl u w SymbolBasicDescription
briefDescribe repo scmRevs
    ((entity, entityFile, entityRange, entityName), symId)
  = mkBriefSymbolDescription symId scmRevs repo CodeEntityLocation{..}
    Nothing

childrenContains1Level :: Int -> SearchRelatedList u w
childrenContains1Level limit baseEntity repo = map Search.childRL <$>
  Search.searchRelatedEntities
    limit
    Search.ShowAll
    Search.NotRecursive
    RelationDirection_Child
    RelationType_Contains
    baseEntity
    repo

-- children by `extends`. typically a list. this includes method overrides
-- note that some methods have a lot of overrides (50k+) so be careful with
-- the limit values
childrenExtends1Level :: Int -> SearchRelatedList u w
childrenExtends1Level limit baseEntity repo = map Search.childRL <$>
  Search.searchRelatedEntities
    limit
    Search.ShowAll
    Search.NotRecursive
    RelationDirection_Child
    RelationType_Extends
    baseEntity
    repo

-- Direct inheritance parents
parentExtends1Level :: Int -> SearchRelatedList u w
parentExtends1Level limit baseEntity repo = map Search.parentRL <$>
  Search.searchRelatedEntities
    limit
    Search.ShowAll
    Search.NotRecursive
    RelationDirection_Parent
    RelationType_Extends
    baseEntity
    repo

-- N levels of container hierarchy
parentContainsNLevel :: Int -> SearchRelatedQuery u w
parentContainsNLevel limit baseEntity repo = Search.searchRelatedEntities
  limit
  Search.ShowAll
  Search.Recursive
  RelationDirection_Parent
  RelationType_Contains
  baseEntity
  repo

-- Inherited symbols: the contained children of N levels of extended parents
inheritedNLevel :: Int -> Int -> Search.SearchStyle -> Code.Entity -> RepoName
    -> RepoHaxl u w
        ([InheritedContainer]
          ,HashMap SymbolId (HashSet SymbolId)
          ,HashMap SymbolId SymbolKind
          )
inheritedNLevel memberLimit inheritedLimit concise baseEntity repo = do
  topoEdges <- Search.searchRelatedEntities
    inheritedLimit
    concise
    Search.Recursive
    RelationDirection_Parent
    RelationType_Extends
    baseEntity
    repo
  -- keep topological ordering handy
  let symTable = Search.edgesToTopoMap topoEdges
  -- reduce to just the unique parent symbols
  let parents = uniq (map Search.parentRL topoEdges)
  -- fetch the parent kinds (class, trait etc)
  kinds <- mapM (\e -> (snd e,) <$> findSymbolKind (toEntity e)) parents
  -- and fetch their children concurrently
  inherited <- mapM (childrenOf memberLimit repo) parents
  return (inherited,symTable,toKindTable kinds)

-- Fetch the "required" parent entities
-- This only exists for Hack entity so we can avoid
-- useless queries for other languages
parentRequire :: RelationType -> Int -> SearchRelatedList u w
parentRequire relationType limit baseEntity repo =
  case baseEntity of
    Code.Entity_hack _ ->
        map Search.parentRL <$>
          Search.searchRelatedEntities
            limit
            Search.ShowAll
            Search.NotRecursive
            RelationDirection_Parent
            relationType
            baseEntity
            repo
    _ -> return []

childrenOf
  :: Int -> RepoName -> Search.LocatedEntity
  -> Glean.RepoHaxl u w (Search.LocatedEntity, [Search.LocatedEntity])
childrenOf limit repo parent = (parent,) <$>
  childrenContains1Level limit (toEntity parent) repo

toEntity :: Search.LocatedEntity -> Code.Entity
toEntity ((entity, _file, _rangespan, _name), _symId) = entity

symbolIdPairs :: [Search.RelatedLocatedEntities] -> [RelatedSymbols]
symbolIdPairs = map (\Search.RelatedLocatedEntities{..} ->
  RelatedSymbols (snd parentRL) (snd childRL) Nothing)

inheritedSymbolIdSets :: InheritedContainer -> InheritedSymbols
inheritedSymbolIdSets (parent, children) = InheritedSymbols {
    inheritedSymbols_base = snd parent,
    inheritedSymbols_provides = map snd children
  }

fromSearchEntity
  :: SymbolId
  -> SearchEntity (ResultLocation Code.Entity)
  -> Search.LocatedEntity
fromSearchEntity symId SearchEntity{..} = (decl, symId)

flattenEdges :: [Search.RelatedLocatedEntities] -> [Search.LocatedEntity]
flattenEdges pairs = concat
  [ [ e1, e2 ] | Search.RelatedLocatedEntities e1 e2 <- pairs ]

toKindTable :: [(SymbolId,Either Text SymbolKind)] ->HashMap SymbolId SymbolKind
toKindTable xs = HashMap.fromList [ (symId, kind) | (symId, Right kind) <- xs ]

-- | Apply any additional client-side filtering of what is in scope,
-- according to language rules
partitionInheritedScopes
  :: Language
  -> SymbolId
  -> HashMap SymbolId (HashSet SymbolId)
  -> HashMap SymbolId SymbolKind
  -> [Search.LocatedEntity]
  -> [InheritedContainer]
  -> ([InheritedContainer], HashMap SymbolId Search.LocatedEntity)
partitionInheritedScopes lang symId edges kinds locals inherited = case lang of
  Language_Hack -> Hack.difference edges kinds symId locals inherited
  _ -> (inherited, mempty)
