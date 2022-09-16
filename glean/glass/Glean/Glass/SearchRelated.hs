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
  , Recursive(..)
  , RelatedLocatedEntities(..)
  ) where

import Control.Monad (forM)
import Control.Monad.Catch (MonadThrow(throwM))
import Data.Hashable (Hashable(..))
import Data.HashSet (HashSet)
import GHC.Generics (Generic)
import qualified Data.HashSet as HashSet

import qualified Glean
import Glean.Angle as Angle
import Glean.Haxl.Repos (RepoHaxl, ReposHaxl, withRepo)
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Codemarkup.Types as Code
import qualified Glean.Schema.Code.Types as Code

import qualified Glean.Glass.Search as Search
import Glean.Glass.Search.Class
import Glean.Glass.Base (GleanPath (..))
import Glean.Glass.Path
import Glean.Glass.SymbolId (entityToAngle, toSymbolId)
import Glean.Glass.Types
import Glean.Glass.Utils (searchRecursiveWithLimit)

-- | Whether to expand relationships recursively
data Recursive
  = Recursive
  | NotRecursive
  deriving Eq

-- | Pairs of edges of related entities
data RelatedEntities = RelatedEntities
  { parentEntity :: Code.Entity
  , parentLocation :: Code.Location
  , childEntity :: Code.Entity
  , childLocation :: Code.Location
  } deriving (Eq,Generic,Hashable)

-- | Pairs of edges of related entities, with all metadata
data RelatedLocatedEntities = RelatedLocatedEntities
  { parentRL :: LocatedEntity
  , childRL :: LocatedEntity
  } deriving (Eq,Generic,Hashable)

type LocatedEntity = (ResultLocation Code.Entity, SymbolId)

--
-- Given some search parameters, find entities by relation
--
searchRelatedEntities
  :: Int
  -> Recursive
  -> RelationDirection
  -> RelationType
  -> Search.SearchEntity Code.Entity
  -> RepoName
  -> ReposHaxl u w [RelatedLocatedEntities]
searchRelatedEntities limit recurse dir rel SearchEntity{..} repo =
  withRepo entityRepo $ toSymbolIds repo =<<
    searchRelation limit limit recurse rel dir repo [decl] HashSet.empty

-- | Lift entity search results into pairs of entities that we found,
-- along with their location and symbol id
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

--
-- Search driver, expand search until done, returning pairs of edges
-- of entity relationships.
--
searchRelation
  :: Int
  -> Int
  -> Recursive
  -> RelationType
  -> RelationDirection
  -> RepoName
  -> [Code.Entity]
  -> HashSet RelatedEntities
  -> RepoHaxl u w [RelatedEntities]
searchRelation
  totalLimit limit recursive relation direction repo toVisit visited = do
    angle <- forM toVisit $ \entity ->
      case entityToAngle entity of
        Right angle -> return angle
        Left t -> throwM (ServerException t)
    justVisited <- case (relation, direction) of
      (RelationType_Extends, RelationDirection_Parent) -> runSearchRelated
        totalLimit angle Code.RelationType_ExtendsParentOfChild
      (RelationType_Extends, RelationDirection_Child) -> runSearchRelated
        totalLimit angle Code.RelationType_ExtendsChildOfParent
      (RelationType_Contains, RelationDirection_Parent) -> runSearchRelated
        totalLimit angle Code.RelationType_ContainsParentOfChild
      (RelationType_Contains, RelationDirection_Child) -> runSearchRelated
        totalLimit angle Code.RelationType_ContainsChildOfParent
      _ -> pure [] -- unknown thrift case
    let
      newlyVisited = HashSet.fromList justVisited `HashSet.difference` visited
      visited' = visited `HashSet.union` newlyVisited
      toVisit = HashSet.toList $ case direction of
        RelationDirection_Parent -> HashSet.map parentEntity newlyVisited
        RelationDirection_Child -> HashSet.map childEntity newlyVisited
        _ -> HashSet.empty -- unknown direction
      recLimit = limit - length visited'
    if
      recursive == Recursive &&
      recLimit > 0 &&
      recLimit < limit &&
      not (null toVisit)
    then
      searchRelation
        totalLimit recLimit recursive relation direction repo toVisit visited'
    else
      pure $ HashSet.toList visited'

runSearchRelated
  :: Int
  -> [Angle Code.Entity]
  -> Code.RelationType
  -> RepoHaxl u w [RelatedEntities]
runSearchRelated limit angle searchType = do
  entities <-
    searchRecursiveWithLimit (Just limit) $
        searchRelatedEntitiesQ searchType $
      elementsOf $ array angle
  pure $
    [ RelatedEntities
      { parentEntity = parentEntity_parent
      , childEntity = childEntity_child
      , parentLocation = parentEntity_location
      , childLocation = childEntity_location
      }
    | Code.SearchRelatedEntities{..} <- entities
    , Just Code.SearchRelatedEntities_key{..}
       <- [searchRelatedEntities_key]
    , let Code.ParentEntity{..} = searchRelatedEntities_key_parent
    , let Code.ChildEntity{..} = searchRelatedEntities_key_child
    ]

--
-- unified search by relation
--
searchRelatedEntitiesQ
  :: Code.RelationType -> Angle Code.Entity -> Angle Code.SearchRelatedEntities
searchRelatedEntitiesQ queryTy entity = predicate @Code.SearchRelatedEntities $
  if queryTy `elem`
     [Code.RelationType_ExtendsParentOfChild,
      Code.RelationType_ContainsParentOfChild]
  then
    rec $ -- bind to child entity
      field @"query" (enum queryTy) $
      field @"child" (rec $ field @"child" entity end)
    end
  else
    rec $ -- search by parent entities
      field @"query" (enum queryTy) $
      field @"parent" (rec $ field @"parent" entity end)
    end
