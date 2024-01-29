{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Glean.Glass.SearchRelated
  ( searchRelatedEntities
  , searchRecursiveEntities
  , Recursive(..)
  , SearchStyle(..)
  , RelatedLocatedEntities(..)
  , RelatedEntities(..)
  , LocatedEntity
  , InheritedContainer
  , edgesToTopoMap
  ) where

import Control.Monad (forM)
import Control.Monad.Catch (MonadThrow(throwM))
import Data.Hashable ( Hashable(..) )
import Data.HashSet ( HashSet )
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import GHC.Generics (Generic)
import qualified Data.HashSet as HashSet

import qualified Glean
import Glean.Angle as Angle
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Codemarkup.Types as Code
import qualified Glean.Schema.Code.Types as Code

import Glean.Glass.Search.Class
import Glean.Glass.Base (GleanPath (..))
import Glean.Glass.Path
import Glean.Glass.SymbolId (entityToAngle, toSymbolId)
import Glean.Glass.Types
import Glean.Glass.Utils (searchRecursiveWithLimit)
import Glean.Haxl.Repos (RepoHaxl)

-- | Whether to expand relationships recursively
data Recursive
  = Recursive
  | NotRecursive
  deriving Eq

-- | Whether to hide uninteresting things
data SearchStyle
  = ShowAll
  | HideUninteresting

-- | Pairs of edges of related entities
data RelatedEntities = RelatedEntities
  { parentEntity :: !Code.Entity
  , parentLocation :: {-# UNPACK #-}!Code.Location
  , childEntity :: !Code.Entity
  , childLocation :: {-# UNPACK #-}!Code.Location
  } deriving (Eq,Generic,Hashable)

-- | Pairs of edges of related entities, with all metadata
data RelatedLocatedEntities = RelatedLocatedEntities
  { parentRL :: LocatedEntity
  , childRL :: LocatedEntity
  } deriving (Generic,Hashable)

type LocatedEntity = (ResultLocation Code.Entity, SymbolId)

-- Convenience type for a parent with a set of contained children
type InheritedContainer = (LocatedEntity, [LocatedEntity])

-- | Flatten results into the container-level topological ordering
-- from child to set of parents it inherits from.
edgesToTopoMap
  :: [RelatedLocatedEntities] -> HashMap SymbolId (HashSet SymbolId)
edgesToTopoMap edges = HM.fromListWith HashSet.union
  [ (snd (childRL e), HashSet.singleton (snd (parentRL e))) | e <- edges ]

--
-- Given some search parameters, find entities by relation
--
searchRelatedEntities
  :: Int
  -> SearchStyle
  -> Recursive
  -> RelationDirection
  -> RelationType
  -> Code.Entity
  -> RepoName
  -> RepoHaxl u w [RelatedLocatedEntities]
searchRelatedEntities limit hide recurse dir rel entity repo =
  toSymbolIds repo =<< searchRelation opts limit [entity] HashSet.empty
  where
    opts = SearchOptions limit hide recurse rel dir

-- | For internal searches, we don't need the symbol id. So we can be slightly
-- more efficient. TODO: we could also avoid the location in the result entirely
-- used for lucky search/context resolution
searchRecursiveEntities
  :: Int -> RelationDirection -> RelationType -> Code.Entity
  -> RepoHaxl u w [RelatedEntities]
searchRecursiveEntities limit dir rel entity =
    searchRelation opts limit [entity] HashSet.empty
  where
    opts = SearchOptions limit ShowAll Recursive rel dir

-- | Lift entity search results into pairs of entities that we found,
-- along with their location and symbol id
--
-- TODO: we will be recomputing these for some symbols many times,
-- and in some cases we are dropping them as well. Can we defer this until
-- later?
--
toSymbolIds
  :: RepoName -> [RelatedEntities] -> RepoHaxl u w [RelatedLocatedEntities]
toSymbolIds repo edges = mapM locatePairs edges
  where
    locatePairs RelatedEntities{..} = RelatedLocatedEntities
      <$> mkLocate parentEntity parentLocation
      <*> mkLocate childEntity childLocation
    mkLocate entity Code.Location{..} = do
      symId <- symbol entity location_file
      return ((entity,location_file,location_location,location_name), symId)
    symbol entity file = do
      path <- GleanPath <$> Glean.keyOf file
      toSymbolId (fromGleanPath repo path) entity

-- | Constants to determine how we search
data SearchOptions = SearchOptions {
  totalLimit :: Int,
  style :: SearchStyle,
  recursive :: Recursive,
  relation :: RelationType,
  direction :: RelationDirection
}

--
-- Search driver, expand search until done, returning pairs of edges
-- of entity relationships.
--
searchRelation
  :: SearchOptions
  -> Int
  -> [Code.Entity]
  -> HashSet RelatedEntities
  -> RepoHaxl u w [RelatedEntities]
searchRelation opts@SearchOptions{..} limit toVisit visited =
    runSearch searchFn opts limit toVisit visited
  where
    -- the search flavor is known up front, so we can partially apply
    baseSearchFn = \ty angle -> runSearchRelated totalLimit style angle ty
    searchFn = case (relation, direction) of
      (RelationType_Extends, RelationDirection_Parent) ->
        baseSearchFn Code.RelationType_ExtendsParentOfChild
      (RelationType_Extends, RelationDirection_Child) ->
        baseSearchFn Code.RelationType_ExtendsChildOfParent
      (RelationType_Contains, RelationDirection_Parent) ->
        baseSearchFn Code.RelationType_ContainsParentOfChild
      (RelationType_Contains, RelationDirection_Child) ->
        baseSearchFn Code.RelationType_ContainsChildOfParent
      (RelationType_RequireExtends, RelationDirection_Parent) ->
        baseSearchFn Code.RelationType_RequireExtendsParentOfChild
      (RelationType_RequireImplements, RelationDirection_Parent) ->
        baseSearchFn Code.RelationType_RequireImplementsParentOfChild
      (RelationType_RequireClass, RelationDirection_Parent) ->
        baseSearchFn Code.RelationType_RequireClassParentOfChild
      _ -> \_angle -> pure []

-- recursively search up to limit
runSearch
  :: ([Angle Code.Entity] -> RepoHaxl u w [RelatedEntities])
  -> SearchOptions
  -> Int
  -> [Code.Entity]
  -> HashSet RelatedEntities
  -> RepoHaxl u w [RelatedEntities]
runSearch searchFn opts@SearchOptions{..} !limit toVisit visited = do
  angle <- forM toVisit $ \entity ->
    case entityToAngle entity of
      Right angle -> return angle
      Left t -> throwM (ServerException t)
  justVisited <- searchFn angle
  let newlyVisited = HashSet.fromList justVisited `HashSet.difference` visited
      visited' = visited `HashSet.union` newlyVisited
      toVisit = HashSet.toList $ case direction of
        RelationDirection_Parent -> HashSet.map parentEntity newlyVisited
        RelationDirection_Child -> HashSet.map childEntity newlyVisited
        _ -> HashSet.empty -- unknown direction
      recLimit = limit - length visited'
  if
    recursive == Recursive && recLimit > 0 && recLimit < limit &&
    not (null toVisit)
  then
    runSearch searchFn opts recLimit toVisit visited'
  else
    return $! HashSet.toList visited'

runSearchRelated
  :: Int
  -> SearchStyle
  -> [Angle Code.Entity]
  -> Code.RelationType
  -> RepoHaxl u w [RelatedEntities]
runSearchRelated limit style angle searchType = do
  (entities, _truncated) <- searchRecursiveWithLimit (Just limit) $
    searchRelatedEntitiesQ searchType styleType entities
  pure $
    [ RelatedEntities
      { parentEntity = parentEntity_parent
      , childEntity = childEntity_child
      , parentLocation = parentEntity_location
      , childLocation = childEntity_location
      }
    | Code.SearchRelatedEntities{..} <- entities
    , Just Code.SearchRelatedEntities_key{..} <- [searchRelatedEntities_key]
    , let Code.ParentEntity{..} = searchRelatedEntities_key_parent
    , let Code.ChildEntity{..} = searchRelatedEntities_key_child
    ]
  where
    entities = elementsOf (array angle)
    styleType = case style of
      ShowAll -> Code.SearchStyle_ShowAll
      HideUninteresting -> Code.SearchStyle_HideUninteresting

--
-- unified search by relation
--
searchRelatedEntitiesQ
  :: Code.RelationType
  -> Code.SearchStyle
  -> Angle Code.Entity
  -> Angle Code.SearchRelatedEntities
searchRelatedEntitiesQ queryTy styleTy entity = case queryTy of
    Code.RelationType_ExtendsParentOfChild -> keyedByChild
    Code.RelationType_ContainsParentOfChild -> keyedByChild
    Code.RelationType_ExtendsChildOfParent -> keyedByParent
    Code.RelationType_ContainsChildOfParent -> keyedByParent
    Code.RelationType_RequireClassParentOfChild -> keyedByChild
    Code.RelationType_RequireImplementsParentOfChild -> keyedByChild
    Code.RelationType_RequireExtendsParentOfChild -> keyedByChild
    Code.RelationType__UNKNOWN _ -> error "Unkonwn Code.RelationType"
  where
    keyedByChild = predicate @Code.SearchRelatedEntities $
      rec $ -- bind to child entity
        field @"query" (enum queryTy) $
        field @"style" (enum styleTy) $
        field @"child" (rec $ field @"child" entity end)
      end
    keyedByParent = predicate @Code.SearchRelatedEntities $
      rec $ -- search by parent entities
        field @"query" (enum queryTy) $
        field @"style" (enum styleTy) $
        field @"parent" (rec $ field @"parent" entity end)
      end
