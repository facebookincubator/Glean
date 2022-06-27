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

import Util.Text (textShow)

import qualified Glean
import Glean.Angle as Angle
import Glean.Haxl.Repos (RepoHaxl, ReposHaxl, withRepo)
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Codemarkup.Types as Code
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.Src.Types as Src

import qualified Glean.Glass.Search as Search
import Glean.Glass.Search.Class
import Glean.Glass.Base (GleanPath (..))
import qualified Glean.Glass.Query as Query
import Glean.Glass.Path
import Glean.Glass.SymbolId (entityToAngle, toSymbolId)
import Glean.Glass.Types
import Glean.Glass.Utils (fetchData, searchRecursiveWithLimit)

-- | Whether to expand relationships recursively
data Recursive
  = Recursive
  | NotRecursive
  deriving Eq

-- | What kind of relationship
data Relation
  = Extends
  | Contains
  deriving Eq

-- | Direction of the relationship arrow to search
data Direction
  = Parent
  | Child
  deriving Eq

-- | Pairs of edges of related entities
data RelatedEntities = RelatedEntities
  { parent :: Code.Entity
  , child :: Code.Entity
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
  withRepo entityRepo $ locateEntities repo =<< case (dir, rel) of
    (RelationDirection_Parent, RelationType_Extends) ->
      searchRelation limit recurse Extends Parent repo [decl] HashSet.empty
    (RelationDirection_Child, RelationType_Extends) ->
      searchRelation limit recurse Extends Child repo [decl] HashSet.empty
    (RelationDirection_Parent, RelationType_Contains) ->
      searchRelation limit recurse Contains Parent repo [decl] HashSet.empty
    (RelationDirection_Child, RelationType_Contains) ->
      searchRelation limit recurse Contains Child repo [decl] HashSet.empty
    _ ->
      pure []

-- | Lift entity search results into pairs of entities that we found,
-- along with their location and symbol id
locateEntities
  :: RepoName -> [RelatedEntities] -> RepoHaxl u w [RelatedLocatedEntities]
locateEntities repo edges = mapM locatePairs edges
  where
    locatePairs RelatedEntities{..} = RelatedLocatedEntities
      <$> mkLocate parent
      <*> mkLocate child

    mkLocate entity = do
      loc <- locate entity
      sym <- symbol loc
      return (loc, sym)

    symbol (entity, file, _span) = do
      path <- GleanPath <$> Glean.keyOf file
      toSymbolId (fromGleanPath repo path) entity

-- | Could batch this up with the original search
locate :: Code.Entity -> RepoHaxl u w (ResultLocation Code.Entity)
locate entity = case entityToAngle entity of
  Right q -> do
    mLoc <- fetchData (locationAngle q)
    case mLoc of
      Just loc -> pure loc
      Nothing -> throwM $ ServerException $
        "Failed to get location for: " <> textShow entity
  Left t -> throwM (ServerException t)

--
-- Search driver, expand search until done, returning pairs of edges
-- of entity relationships.
--
searchRelation
  :: Int
  -> Recursive
  -> Relation
  -> Direction
  -> RepoName
  -> [Code.Entity]
  -> HashSet RelatedEntities
  -> RepoHaxl u w [RelatedEntities]
searchRelation limit recursive relation direction repo toVisit visited = do
  angle <- forM toVisit $ \entity ->
     case entityToAngle entity of
      Right angle -> return angle
      Left t -> throwM (ServerException t)
  justVisited <- case (relation, direction) of
    (Extends, Parent) -> searchParentExtends limit angle
    (Extends, Child) -> searchChildExtends limit angle
    (Contains, Parent) -> return [] -- todo
    (Contains, Child) -> searchChildContains limit angle
  let
    newlyVisited = HashSet.fromList justVisited `HashSet.difference` visited
    visited' = visited `HashSet.union` newlyVisited
    toVisit = HashSet.toList $ case direction of
      Parent -> HashSet.map parent newlyVisited
      Child -> HashSet.map child newlyVisited
    recLimit = limit - length visited'
  if
    recursive == Recursive &&
    recLimit > 0 &&
    recLimit < limit &&
    not (null toVisit)
  then
    searchRelation recLimit recursive relation direction repo toVisit visited'
  else
   pure $ HashSet.toList visited'

searchChildContains
  :: Int
  -> [Angle Code.Entity]
  -> RepoHaxl u w [RelatedEntities]
searchChildContains limit angle = do
  entities <-
    searchRecursiveWithLimit (Just limit) $ searchChildContainsAngle $
      elementsOf $ array angle
  pure $
    [ RelatedEntities
      { parent = Code.containsChildEntity_key_parent contains
      , child = Code.containsChildEntity_key_child contains
      }
    | Code.ContainsChildEntity {..} <- entities
    , Just contains <- [containsChildEntity_key]
    ]

searchParentExtends
  :: Int
  -> [Angle Code.Entity]
  -> RepoHaxl u w [RelatedEntities]
searchParentExtends limit angle = do
  entities <-
    searchRecursiveWithLimit (Just limit) $ searchParentExtendsAngle $
      elementsOf $ array angle
  pure $
    [ RelatedEntities
      { parent = Code.extendsParentEntity_key_parent extends
      , child = Code.extendsParentEntity_key_child extends
      }
    | Code.ExtendsParentEntity {..} <- entities
    , Just extends <- [extendsParentEntity_key]
    ]

searchChildExtends
  :: Int
  -> [Angle Code.Entity]
  -> RepoHaxl u w [RelatedEntities]
searchChildExtends limit angle = do
  entities <-
    searchRecursiveWithLimit (Just limit) $ searchChildExtendsAngle $
      elementsOf $ array angle
  pure $
    [ RelatedEntities
      { parent = Code.extendsChildEntity_key_parent extends
      , child = Code.extendsChildEntity_key_child extends
      }
    | Code.ExtendsChildEntity {..} <- entities
    , Just extends <- [extendsChildEntity_key]
    ]

searchChildContainsAngle :: Angle Code.Entity -> Angle Code.ContainsChildEntity
searchChildContainsAngle entity =
  predicate @Code.ContainsChildEntity (
    rec $
      field @"parent" entity
    end)

searchParentExtendsAngle :: Angle Code.Entity -> Angle Code.ExtendsParentEntity
searchParentExtendsAngle entity =
  predicate @Code.ExtendsParentEntity (
    rec $
      field @"child" entity
    end)

searchChildExtendsAngle :: Angle Code.Entity -> Angle Code.ExtendsChildEntity
searchChildExtendsAngle entity =
  predicate @Code.ExtendsChildEntity (
    rec $
      field @"parent" entity
    end)

locationAngle :: Angle Code.Entity -> Angle (ResultLocation Code.Entity)
locationAngle entity =
  vars $ \(entity' :: Angle Code.Entity) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) ->
    tuple (entity', file, rangespan) `where_` [
      entity' .= sig @Code.Entity entity,
      Query.entityLocation entity file rangespan
    ]
