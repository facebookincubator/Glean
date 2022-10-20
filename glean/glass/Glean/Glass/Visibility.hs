{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeApplications #-}

module Glean.Glass.Visibility
  ( getVisibilityForEntity
  ) where

import Data.Text

import qualified Glean
import Glean.Angle as Angle
import Glean.Glass.SymbolId (entityToAngle)
import qualified Glean.Haxl.Repos as Glean

import Glean.Glass.Types as Glass
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.Codemarkup.Types as Code
import qualified Glean.Schema.CodemarkupTypes.Types as Code

getVisibilityForEntity
  :: Code.Entity
  -> Glean.RepoHaxl u w (Either Text (Maybe Visibility))
getVisibilityForEntity entity = case entityToAngle entity of
  Left t -> return $ Left t
  Right entityAngle -> Right <$> do
    maybeResult <- Glean.getFirstResult $ Glean.recursive $ Angle.query $
      Angle.predicate @Code.EntityVisibility (entityAngle .-> wild)
    return $ do
      result <- maybeResult
      value <- Glean.getFactValue result
      return $ case value of
        Code.Visibility_Public -> Visibility_Public
        Code.Visibility_Protected -> Visibility_Protected
        Code.Visibility_Private -> Visibility_Private
        Code.Visibility_Internal -> Visibility_Internal
        Code.Visibility__UNKNOWN i -> Visibility__UNKNOWN i
