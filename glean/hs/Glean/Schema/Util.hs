{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Utilities for working with Glean.Angle.Types
module Glean.Schema.Util
  ( unit
  , lowerBool
  , lowerMaybe
  , lowerEnum
  , boolFields
  , maybeFields
  , enumFields
  , tupleSchema
  , parseRef
  , convertRef
  , ShowRef(..)
  , NameSpaces
  , splitDot
  , SourceRef(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import TextShow

import Util.Text

import Glean.Angle.Types

unit :: Type_ s a b
unit = RecordTy []

-- | A maybe type is equivalent to a sum type with {nothing,just} fields.
lowerMaybe :: Type_ s a b -> Type_ s a b
lowerMaybe param = SumTy (maybeFields param)

maybeFields :: Type_ s a b -> [FieldDef_ s a b]
maybeFields param =
  [ FieldDef "nothing" unit
  , FieldDef "just" param ]

lowerBool :: Type_ s a b
lowerBool = SumTy boolFields

boolFields :: [FieldDef_ s a b]
boolFields = [FieldDef "false" unit, FieldDef "true" unit]

lowerEnum :: [Name] -> Type_ s a b
lowerEnum ides = SumTy (enumFields ides)

enumFields :: [Name] -> [FieldDef_ s a b]
enumFields ides = [FieldDef ide unit | ide <- ides]

tupleSchema :: [Type_ s a b] -> Type_ s a b
tupleSchema tys = RecordTy
  [ FieldDef (tupleField <> Text.pack (show n)) ty
  | (n,ty) <- zip [0::Int ..] tys ]

-- | Parse a predicate reference of the form @<predicate>[.<version>]@
-- If the version is omitted, it defaults to the most recent version
-- of that predicate.
--
-- This syntax is used in:
--  * The JSON format for writing facts
--  * Referring to predicates in the shell
--
parseRef :: Text -> SourceRef
parseRef txt
  | Right ver <- textToInt after =
    SourceRef (Text.init before) (Just (fromIntegral ver))
  | otherwise =
    SourceRef txt Nothing
  where
   (before,after) = Text.breakOnEnd "." txt

-- | Convert from a 'PredicateRef' (oftem from 'getName') to 'SourceRef'
-- (which is guaranteed to have a 'Version')
convertRef :: PredicateRef -> SourceRef
convertRef p = SourceRef
  { sourceRefName = predicateRef_name p
  , sourceRefVersion = Just (fromIntegral (predicateRef_version p)) }

class ShowRef t where
  showRef :: t -> Text

-- | Render the SourceRef to @name@ or @name.ver@
instance ShowRef SourceRef where
  showRef (SourceRef name Nothing) = name
  showRef (SourceRef name (Just ver)) = name <> "." <> showt ver

instance ShowRef TypeRef where
  showRef (TypeRef name ver) = name <> "." <> showt ver

instance ShowRef PredicateRef where
  showRef = showRef . convertRef

instance ShowRef PredicateId where
  showRef (PredicateId name hash) = showRef name <> "." <> Text.pack (show hash)

instance ShowRef TypeId where
  showRef (TypeId name hash) = showRef name <> "." <> Text.pack (show hash)

type NameSpaces = [Text]

splitDot :: Name -> (NameSpaces, Text)
splitDot x =
  let pieces = Text.split ('.' ==) x
  in case reverse pieces of
      [] -> ([], "")
      x : ys -> (reverse ys, x)
