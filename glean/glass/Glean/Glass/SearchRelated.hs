{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeApplications #-}

module Glean.Glass.SearchRelated
  ( searchRelatedSymbols
  ) where

import Glean.Angle as Angle
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.Codemarkup.Types as Code
import Glean.Glass.SymbolId (entityToAngle, toSymbolId)
import Glean.Glass.Types
import Glean.Haxl.Repos (RepoHaxl, ReposHaxl, withRepo)
import Data.Text (Text)
import Glean.Glass.Search (SearchEntity(..), SearchResult (..))
import qualified Glean.Glass.Search as Search
import Control.Monad.Catch (MonadThrow(throwM))
import Glean.Glass.Utils (searchRecursiveWithLimit)
import Glean.Glass.Logging (ErrorLogger)

searchRelatedSymbols
  :: Int
  -> RelationDirection
  -> RelationType
  -> (RepoName, Language, [Text])
  -> ReposHaxl u w ([RelatedSymbols], Maybe ErrorLogger)
searchRelatedSymbols limit dir rel (repo, lang, toks) = do
  r <- Search.searchEntity lang toks
  (SearchEntity {..}, err) <- case r of
    None t -> throwM (ServerException t)
    One e -> return (e, Nothing)
    Many e _t -> return (e, Nothing)
  edges <- withRepo entityRepo $ case (dir, rel) of
    (RelationDirection_Parent, RelationType_Extends) ->
      searchParentExtends limit repo decl
    (RelationDirection_Child, RelationType_Extends) ->
      searchChildExtends limit repo decl
    _ ->
      return []
  return (edges, err)

toSymbolIds
  :: RepoName
  -> (Code.Entity, Code.Entity)
  -> RepoHaxl u w RelatedSymbols
toSymbolIds repo (parent, child) = do
  relatedSymbols_parent <- toSymbolId repo parent
  relatedSymbols_child <- toSymbolId repo child
  pure $ RelatedSymbols {..}

searchParentExtends
  :: Int
  -> RepoName
  -> Code.Entity
  -> RepoHaxl u w [RelatedSymbols]
searchParentExtends limit repo entity = do
  let
  angle <- case entityToAngle entity of
    Right angle -> return angle
    Left t -> throwM (ServerException t)
  entities <-
    searchRecursiveWithLimit (Just limit) $ searchParentExtendsAngle angle
  mapM (toSymbolIds repo)
    [ (Code.extendsParentEntity_key_parent extends,
       Code.extendsParentEntity_key_child extends)
    | Code.ExtendsParentEntity {..} <- entities
    , Just extends <- [extendsParentEntity_key]
    ]

searchChildExtends
  :: Int
  -> RepoName
  -> Code.Entity
  -> RepoHaxl u w [RelatedSymbols]
searchChildExtends limit repo entity = do
  let
  angle <- case entityToAngle entity of
    Right angle -> return angle
    Left t -> throwM (ServerException t)
  entities <-
    searchRecursiveWithLimit (Just limit) $ searchChildExtendsAngle angle
  mapM (toSymbolIds repo)
    [ (Code.extendsChildEntity_key_parent extends,
       Code.extendsChildEntity_key_child extends)
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
