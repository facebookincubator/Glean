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
  ( searchRelatedSymbols
  , Recursive(..)
  ) where

import Control.Monad (forM)
import Control.Monad.Catch (MonadThrow(throwM))
import Data.Hashable (Hashable(..))
import Data.HashSet (HashSet)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Data.HashSet as HashSet

import Glean.Angle as Angle
import Glean.Haxl.Repos (RepoHaxl, ReposHaxl, withRepo)
import qualified Glean.Schema.Codemarkup.Types as Code
import qualified Glean.Schema.Code.Types as Code

import Glean.Glass.Logging (ErrorLogger)
import Glean.Glass.Search (SearchEntity(..), SearchResult(..))
import Glean.Glass.SymbolId (entityToAngle, toSymbolId)
import qualified Glean.Glass.Search as Search
import Glean.Glass.Types
import Glean.Glass.Utils (searchRecursiveWithLimit)

data Recursive
  = Recursive
  | NotRecursive
  deriving (Eq,Show)

data Direction
  = Parent
  | Child
  deriving (Eq,Show)

data RelatedEntities = RelatedEntities
  { parent :: Code.Entity
  , child :: Code.Entity
  } deriving (Eq,Show,Generic,Hashable)

searchRelatedSymbols
  :: Int
  -> Recursive
  -> RelationDirection
  -> RelationType
  -> (RepoName, Language, [Text])
  -> ReposHaxl u w ([RelatedSymbols], Maybe ErrorLogger)
searchRelatedSymbols limit recurse dir rel (repo, lang, toks) = do
  r <- Search.searchEntity lang toks
  (SearchEntity {..}, err) <- case r of
    None t -> throwM (ServerException t)
    One e -> return (e, Nothing)
    Many e _t -> return (e, Nothing)
  edges <- withRepo entityRepo $ case (dir, rel) of
    (RelationDirection_Parent, RelationType_Extends) ->
      searchExtends limit recurse Parent repo [decl] HashSet.empty
    (RelationDirection_Child, RelationType_Extends) ->
      searchExtends limit recurse Child repo [decl] HashSet.empty
    _ ->
      return []
  return (edges, err)

toSymbolIds
  :: RepoName
  -> RelatedEntities
  -> RepoHaxl u w RelatedSymbols
toSymbolIds repo RelatedEntities{..} = do
  relatedSymbols_parent <- toSymbolId repo parent
  relatedSymbols_child <- toSymbolId repo child
  pure $ RelatedSymbols {..}

searchExtends
  :: Int
  -> Recursive
  -> Direction
  -> RepoName
  -> [Code.Entity]
  -> HashSet RelatedEntities
  -> RepoHaxl u w [RelatedSymbols]
searchExtends limit recursive direction repo toVisit visited = do
  justVisited <- case direction of
    Parent -> searchParentExtends limit toVisit
    Child -> searchChildExtends limit toVisit
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
    searchExtends recLimit recursive direction repo toVisit visited'
  else
    mapM (toSymbolIds repo) $ HashSet.toList visited'

searchParentExtends
  :: Int
  -> [Code.Entity]
  -> RepoHaxl u w [RelatedEntities]
searchParentExtends limit toVisit = do
  angle <- forM toVisit $ \entity ->
     case entityToAngle entity of
      Right angle -> return angle
      Left t -> throwM (ServerException t)
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
  -> [Code.Entity]
  -> RepoHaxl u w [RelatedEntities]
searchChildExtends limit toVisit = do
  angle <- forM toVisit $ \entity ->
     case entityToAngle entity of
      Right angle -> return angle
      Left t -> throwM (ServerException t)
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
