{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeApplications #-}

module Glean.Glass.Comments (
    getCommentsForEntity
  ) where

import Data.Text (Text)

import qualified Glean
import Glean.Angle as Angle
import Glean.Glass.Range ( rangeSpanToLocationRange )
import qualified Glean.Haxl.Repos as Glean
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.Src.Types as Src
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Codemarkup.Types as Code
import qualified Glean.Schema.CodeScip.Types as CodeScip
import qualified Glean.Schema.Scip.Types as Scip

import Glean.Glass.SymbolId (entityToAngle)
import Glean.Glass.Utils ( searchRecursiveWithLimit )
import qualified Glean.Glass.Types as Glass

-- | Set a low bound on maximum results so we don't search too long
mAX_COMMENTS :: Int
mAX_COMMENTS = 3

getCommentsForEntity
  :: Glass.RepoName
  -> Glass.LocationRange -- ^ definition location of the entity
  -> Code.Entity
  -> Glean.RepoHaxl u w (Either Text [Glass.SymbolComment])
getCommentsForEntity repo location entity = case entity of
  Code.Entity_scip e -> Right <$> getScipComments location (scipEntity e)
  _ -> case entityToAngle entity of
    Left err -> pure (Left err)
    Right q -> Right <$> getEntityComments repo q

-- | SCIP documentation has no span of its own, so attribute it to the
-- definition
getScipComments
  :: Glass.LocationRange
  -> Maybe Scip.SomeEntity
  -> Glean.RepoHaxl u w [Glass.SymbolComment]
getScipComments _ Nothing = return []
getScipComments location (Just (Scip.SomeEntity defn)) = do
  (docs, _truncated) <- searchRecursiveWithLimit (Just mAX_COMMENTS) $
    scipDocumentation (Glean.getId defn)
  return [Glass.SymbolComment location (Just doc) | doc <- docs]

scipDocumentation :: Glean.IdOf Scip.Definition -> Angle Text
scipDocumentation defnId =
  vars $ \(text :: Angle Text) (docs :: Angle Scip.Documentation) ->
  text `where_` [
    wild .= Angle.predicate @Scip.DefinitionDocumentation (
      rec $
        field @"defn" (asPredicate (factId defnId)) $
        field @"docs" (asPredicate docs)
      end
    ),
    docs .= Angle.predicate @Scip.Documentation text
  ]

-- scip languages, just enumerate completely to stay total
scipEntity :: CodeScip.Entity -> Maybe Scip.SomeEntity
scipEntity e = case e of
  CodeScip.Entity_rust x -> Just x
  CodeScip.Entity_go x -> Just x
  CodeScip.Entity_typescript x -> Just x
  CodeScip.Entity_java x -> Just x
  CodeScip.Entity_kotlin x -> Just x
  CodeScip.Entity_swift x -> Just x
  CodeScip.Entity_python x -> Just x
  CodeScip.Entity_typescriptReact x -> Just x
  CodeScip.Entity_javascript x -> Just x
  CodeScip.Entity_javascriptReact x -> Just x
  CodeScip.Entity_c x -> Just x
  CodeScip.Entity_cpp x -> Just x
  CodeScip.Entity_ruby x -> Just x
  CodeScip.Entity_csharp x -> Just x
  CodeScip.Entity_visualBasic x -> Just x
  CodeScip.Entity_dart x -> Just x
  CodeScip.Entity_php x -> Just x
  CodeScip.Entity_EMPTY -> Nothing

getEntityComments
  :: Glass.RepoName
  -> Angle Code.Entity
  -> Glean.RepoHaxl u w [Glass.SymbolComment]
getEntityComments repo entity = do
    (res, _truncated) <- searchRecursiveWithLimit (Just mAX_COMMENTS) $
      entityComments entity
    mapM toLocation res
  where
    toLocation (file, span, mtext) = do
      range <- mkLocationRange repo file span
      return (Glass.SymbolComment range mtext)

-- | Docs are always bytespans at the moment. Convert them out.
-- these files should be in the src.FileLines cache as they're almost always
-- the same file as the declaration location, which is already processed
mkLocationRange
   :: Glass.RepoName -> Src.File -> Src.ByteSpan
   -> Glean.RepoHaxl u w Glass.LocationRange
mkLocationRange repo file span = rangeSpanToLocationRange repo file
  (Code.RangeSpan_span span)

entityComments
  :: Angle Code.Entity -> Angle (Src.File, Src.ByteSpan, Maybe Text)
entityComments entity =
  vars $ \ (file :: Angle Src.File) (span :: Angle Src.ByteSpan)
    (mtext :: Angle (Maybe Text)) ->
  tuple (file, span, mtext) `where_` [
    wild .= Angle.predicate @Code.EntityComments (
      rec $
        field @"entity" entity $
        field @"file" (asPredicate file) $
        field @"span" span $
        field @"text" mtext
      end
    )
  ]
