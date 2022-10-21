{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeApplications #-}

module Glean.Glass.Visibility ( getInfoForEntity ) where

import Data.Text
import qualified Data.Set as Set
import Data.Set ( Set )
import Data.Maybe

import Glean.Angle as Angle
import Glean.Glass.SymbolId ( entityToAngle )
import Glean.Glass.Utils
import qualified Glean.Haxl.Repos as Glean

import Glean.Glass.Types as Glass
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.Codemarkup.Types as Code
import qualified Glean.Schema.CodemarkupTypes.Types as Code

getInfoForEntity
  :: Code.Entity
  -> Glean.RepoHaxl u w (Either Text (Maybe Visibility, Set Modifier))
getInfoForEntity entity = case entityToAngle entity of
  Left t -> return $ Left t
  Right ent -> do
    mVisibility <- fetchData $ angleVisibility ent
    mModifiers <- fetchData $ angleModifiers ent
    return $ Right
      (visibilityToGlassType <$> mVisibility,
       maybe Set.empty modifiersToSet mModifiers)

-- | Glean type to Glass external type
visibilityToGlassType :: Code.Visibility -> Visibility
visibilityToGlassType x = case x of
  Code.Visibility_Public -> Visibility_Public
  Code.Visibility_Protected -> Visibility_Protected
  Code.Visibility_Private -> Visibility_Private
  Code.Visibility_Internal -> Visibility_Internal
  Code.Visibility__UNKNOWN i -> Visibility__UNKNOWN i

modifiersToSet :: Code.Modifiers -> Set Modifier
modifiersToSet Code.Modifiers{..} = Set.fromList $ catMaybes
  [ modifiers_isAbstract *-> Modifier_ABSTRACT
  , modifiers_isFinal *-> Modifier_FINAL
  , modifiers_isAsync *-> Modifier_ASYNC
  , modifiers_isStatic *-> Modifier_STATIC
  , modifiers_isReadonly *-> Modifier_READONLY
  , modifiers_isConst *-> Modifier_CONST
  , modifiers_isMutable *-> Modifier_MUTABLE
  , modifiers_isVolatile *-> Modifier_VOLATILE
  , modifiers_isVirtual *-> Modifier_VIRTUAL
  , modifiers_isInline *-> Modifier_INLINE
  ]
  where
    x *-> y = if x then Just y else Nothing

-- | Avoid recursively expanding the entity, just return the tag
angleVisibility :: Angle Code.Entity -> Angle Code.Visibility
angleVisibility entity = var $ \visibility ->
  visibility `where_` [
    wild .= Angle.predicate @Code.EntityVisibility (
        entity .-> visibility
      )
  ]

angleModifiers :: Angle Code.Entity -> Angle Code.Modifiers
angleModifiers entity = var $ \modifiers ->
  modifiers `where_` [
    wild .= Angle.predicate @Code.EntityModifiers (
      rec $
        field @"entity" entity $
        field @"modifiers" modifiers
      end
    )
  ]
