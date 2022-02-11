{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Base
  ( GleanDBName(..)
  , GleanPath(..)
  , GleanDBAttrName(..)
  ) where

import Data.String
import Data.Text (Text)

import Glean.Glass.Attributes.Class as Attributes
import Glean.Glass.Utils

-- | Type of glean dbs
newtype GleanDBName = GleanDBName { unGleanDBName :: Text }
  deriving (Eq, Ord)

instance IsString GleanDBName where fromString = GleanDBName . fromString

--
-- | A glean path for www is prefixed with www/
-- For all other repos, it is relative to the repo root.
--
newtype GleanPath = GleanPath { gleanPath :: Text }
  deriving Eq

-- | A little existential type key for attributes
data GleanDBAttrName =
   forall attr .
     ( QueryType (Attributes.AttrRep attr)
     , Attributes.ToAttributes attr)
   =>
  GleanDBAttrName {
    gleanAttrDBName :: GleanDBName,
    attributeKey :: attr
  }
