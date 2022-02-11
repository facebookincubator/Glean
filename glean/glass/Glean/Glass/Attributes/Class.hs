{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
-- Copyright 2004-present Facebook. All Rights Reserved.

module Glean.Glass.Attributes.Class
  ( ToAttributes(..)
  , SymbolIdentifier(..)

  -- * Generic methods
  , toAttrMap
  ) where

import Data.Text ( Text )
import Data.Maybe ( mapMaybe )
import qualified Data.Map.Strict as Map

import qualified Glean
import qualified Glean.Angle as Angle
import qualified Glean.Haxl.Repos as Glean

import Glean.Glass.Types ( Attributes, SymbolId )
import Glean.Glass.Utils ( QueryType )
import qualified Glean.Schema.Src.Types as Src ( File )
import qualified Glean.Schema.Code.Types as Code

-- | Key for identifying the 'entity' the attribute is associated with
-- For 3rd party dbs, this is something that can be derived from a SymbolId
--
-- Otherwise, we expect a code.Entity value to compare for equality
--
data SymbolIdentifier
   = Identifier Text -- convertible to symbol ids for matching
   | Entity Code.Entity -- actual entity value
  deriving (Eq, Ord, Show)

-- | Class for querying attributes and converting them to thrift
class ToAttributes key where

  type AttrRep key :: *
  type AttrOut key :: *

  -- | Search method (recursive, data, with limit ...)
  searchBy
    :: QueryType (AttrRep key)
    => key
    -> Maybe Int
    -> Angle.Angle (AttrRep key)
    -> Glean.RepoHaxl u w [AttrRep key]

  -- | Register a query function for this attribute type
  queryFileAttributes
    :: QueryType (AttrRep key)
    => key -> Glean.IdOf Src.File -> Angle.Angle (AttrRep key)

  -- | Convert raw Angle type to a higher level type
  fromAngleType
    :: key -> AttrRep key -> Maybe (SymbolIdentifier, AttrOut key)

  -- | Translate higher level type to Attributes bag
  toAttributes
    :: key -> AttrOut key -> Attributes

  -- | How to match the attribute key against a correspoonding symbol or entity
  -- This is a pass through value for entities, or a text conversion on SymbolId
  -- to the 3rd party symbol format
  fromSymbolId
    :: key -> SymbolId -> Code.Entity -> SymbolIdentifier

-- | Convert query result into attribute bag
convertAttributes
  :: ToAttributes key
  => key -> AttrRep key -> Maybe (SymbolIdentifier, Attributes)
convertAttributes key rep = case fromAngleType key rep of
  Nothing -> Nothing
  Just (ident, row) -> Just (ident, toAttributes key row)

-- | Process raw data into attributes map keyed by eq/ord on symbol identifiers
-- This is used for 'joins', i.e. lookup by converted id to find values.
--
-- If the attribute type requires entities, the values in the map will be
-- entities If the attribute type requires something else, fromSymbolId will
-- generate that
--
toAttrMap
  :: ToAttributes key
  => key -> [AttrRep key] -> Map.Map SymbolIdentifier Attributes
toAttrMap key dat = Map.fromList $ mapMaybe (convertAttributes key) dat
