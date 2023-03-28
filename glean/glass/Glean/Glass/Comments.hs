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

import Glean.Angle as Angle
import Glean.Glass.Range ( rangeSpanToLocationRange )
import qualified Glean.Haxl.Repos as Glean
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.Src.Types as Src
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Codemarkup.Types as Code

import Glean.Glass.SymbolId (entityToAngle)
import Glean.Glass.Utils ( searchRecursiveWithLimit )
import qualified Glean.Glass.Types as Glass

-- | Set a low bound on maximum results so we don't search too long
mAX_COMMENTS :: Int
mAX_COMMENTS = 3

getCommentsForEntity
  :: Glass.RepoName
  -> Code.Entity
  -> Glean.RepoHaxl u w (Either Text [Glass.SymbolComment])
getCommentsForEntity repo entity = case entityToAngle entity of
  Left err -> pure (Left err)
  Right q -> Right <$> getEntityComments repo q

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
