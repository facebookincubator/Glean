{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Describe (
  -- * symbol information API
    mkSymbolDescription,
    mkBriefSymbolDescription,
    describeEntity,
    briefDescribeEntity
  ) where

import Data.Maybe ( listToMaybe )
import Data.Either.Extra ( eitherToMaybe )

import qualified Glean
import Glean.Haxl.Repos as Glean ( RepoHaxl )

import Glean.Glass.Range ( rangeSpanToLocationRange )
import Glean.Glass.Types
import Glean.Glass.SymbolId ( toQualifiedName, entityLanguage )
import Glean.Glass.SymbolKind ( findSymbolKind )
import Glean.Glass.Search ( CodeEntityLocation(..) )
import Glean.Glass.SymbolSig ( toSymbolSignatureText )
import Glean.Glass.Annotations ( getAnnotationsForEntity )
import Glean.Glass.Visibility ( getInfoForEntity )
import Glean.Glass.Comments ( getCommentsForEntity )
import Glean.Glass.Repos ( ScmRevisions, getRepoHashForLocation )
import Glean.Glass.Utils ( eThrow, fst4 )
import Glean.Glass.Pretty.Cxx as Cxx (Qualified(..))
import qualified Glean.Glass.SearchRelated as Search

import qualified Glean.Schema.Code.Types as Code

-- Helper to fill out symbol description metadata uniformly
mkSymbolDescription
  :: SymbolId
  -> ScmRevisions
  -> RepoName
  -> CodeEntityLocation
  -> Maybe SymbolContext
  -> Glean.RepoHaxl u w SymbolDescription
mkSymbolDescription  = mkDescription describeEntity

-- Helper to fill out _basic_ symbol description metadata uniformly
mkBriefSymbolDescription
  :: SymbolId
  -> ScmRevisions
  -> RepoName
  -> CodeEntityLocation
  -> Maybe SymbolContext
  -> Glean.RepoHaxl u w SymbolBasicDescription
mkBriefSymbolDescription = mkDescription (const briefDescribeEntity)

--- Worker to fill out symbol description metadata uniformly
mkDescription
  :: (ScmRevisions -> Code.Entity -> SymbolResult -> Glean.RepoHaxl u w t)
  -> SymbolId
  -> ScmRevisions
  -> RepoName
  -> CodeEntityLocation
  -> Maybe SymbolContext
  -> Glean.RepoHaxl u w t
mkDescription fn symbolId scmRevs repo CodeEntityLocation{..} mctx = do
  range <- rangeSpanToLocationRange repo entityFile entityRange
  kind <- eitherToMaybe <$> findSymbolKind entity
  qname <- eThrow =<< toQualifiedName entity -- non-optional now
  let lang = entityLanguage entity
      score = mempty
  fn scmRevs entity $
    SymbolResult symbolId range lang kind entityName score qname mctx

-- | Return a brief description with only required fields set.
-- Far more efficient for cases where we don't need everything
briefDescribeEntity
  :: Code.Entity -> SymbolResult -> Glean.RepoHaxl u w SymbolBasicDescription
briefDescribeEntity ent SymbolResult{..} = do
  symbolBasicDescription_signature <- fst <$> toSymbolSignatureText ent repo
    symbolResult_symbol Cxx.Qualified
  pure SymbolBasicDescription{..}
  where
    symbolBasicDescription_sym = symbolResult_symbol
    symbolBasicDescription_name = symbolResult_qname
    symbolBasicDescription_kind = symbolResult_kind
    symbolBasicDescription_language = symbolResult_language
    repo = locationRange_repository symbolResult_location

-- | Return a description for a single Entity with a unique location.
describeEntity
  :: ScmRevisions
  -> Code.Entity
  -> SymbolResult
  -> Glean.RepoHaxl u w SymbolDescription
describeEntity scmRevs ent SymbolResult{..} = do
  symbolDescription_repo_hash <-
    getRepoHashForLocation symbolResult_location scmRevs <$> Glean.haxlRepo
  let symbolDescription_name = symbolResult_qname
  symbolDescription_annotations <- eThrow =<< getAnnotationsForEntity repo ent
  symbolDescription_pretty_comments <- eThrow =<< getCommentsForEntity repo ent
  -- backwards compat until deprecated, we just make a copy
  let symbolDescription_comments = map symbolComment_location
        symbolDescription_pretty_comments
  (symbolDescription_visibility, symbolDescription_modifiers)
     <- eThrow =<< getInfoForEntity ent
  (symbolDescription_signature, symbolDescription_type_xrefs)
    <- toSymbolSignatureText ent repo symbolResult_symbol Cxx.Qualified
  symbolDescription_extends_relation <-
    relationDescription RelationType_Extends
  symbolDescription_contains_relation <-
    relationDescription RelationType_Contains
  pure SymbolDescription{..}
  where
    symbolDescription_sym = symbolResult_symbol
    symbolDescription_kind = symbolResult_kind
    symbolDescription_language = symbolResult_language

    symbolDescription_sym_location = symbolResult_location
    symbolDescription_sym_other_locations = []

    repo = locationRange_repository symbolResult_location

    -- deprecated. we already have sym_location
    symbolDescription_location = SymbolPath {
      symbolPath_range = locationRange_range symbolResult_location,
      symbolPath_repository = locationRange_repository symbolResult_location,
      symbolPath_filepath = locationRange_filepath symbolResult_location
    }

    relationDescription relatedBy = do
      parents <- describeRelation RelationDirection_Parent
      children <- describeRelation RelationDirection_Child

      let firstParent = Search.parentRL <$> listToMaybe parents
          firstChild = Search.childRL <$> listToMaybe children
          relationDescription_firstParent = snd <$> firstParent
          relationDescription_firstChild = snd <$> firstChild
          relationDescription_hasMoreParents =  length parents > 1
          relationDescription_hasMoreChildren = length children > 1

      relationDescription_firstParentName <- case firstParent of
        Nothing -> pure Nothing
        Just (p,_) -> eitherToMaybe <$> toQualifiedName (fst4 p)
      relationDescription_firstChildName <- case firstChild of
        Nothing -> pure Nothing
        Just (p,_) -> eitherToMaybe <$> toQualifiedName (fst4 p)

      pure RelationDescription{..}
      where
        describeRelation relation = Search.searchRelatedEntities 2
          Search.NotRecursive relation relatedBy ent repo
