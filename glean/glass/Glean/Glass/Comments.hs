{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeApplications #-}

module Glean.Glass.Comments
  ( getCommentsForEntity
  ) where

import Data.Text (Text)

import qualified Glean
import Glean.Angle as Angle
import Glean.Glass.Range
import qualified Glean.Haxl.Repos as Glean
import qualified Glean.Schema.Docmarkup.Types as Docmarkup
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.CodemarkupTypes.Types as Code

import Glean.Glass.SymbolId (entityToAngle)
import qualified Glean.Glass.Types as Glass

getCommentsForEntity
  :: Glass.RepoName
  -> Code.Entity
  -> Glean.RepoHaxl u w (Either Text [Glass.LocationRange])
getCommentsForEntity repo entity = do
  mapM anns $ entityToAngle entity
  where
    anns entityAngle = do
      preds <- Glean.search_ $ Glean.recursive $ Angle.query $
          Angle.predicate @Docmarkup.EntityComments (
            rec $
              field @"entity" entityAngle
            end)
      keys <- mapM Glean.keyOf preds
      mapM getLocationRange keys
    getLocationRange Docmarkup.EntityComments_key{..} =
      locationRangeFromCodeLocation
        repo
        entityComments_key_file $
          Code.RangeSpan_span
          entityComments_key_span
