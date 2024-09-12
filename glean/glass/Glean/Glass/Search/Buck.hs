{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-uni-patterns #-}

module Glean.Glass.Search.Buck
  ( {- instances -}
  ) where

import Data.Text ( Text )
import Data.List.Split ( splitOn )

import Glean.Angle as Angle

import Glean.Glass.Search.Class
    ( searchSymbolId, ResultLocation, Search(..), SearchResult(None) )
import Glean.Glass.Query ( entityLocation )

import qualified Glean.Schema.CodeBuck.Types as Buck
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.SearchBuck.Types as Buck
import qualified Glean.Schema.Src.Types as Src

import Glean.Glass.Utils as Utils


parse :: [Text] -> Maybe (Maybe Text, Text, Text)
parse locator = do
  let name : rest = reverse locator
  let [subdir, path] = splitOn ["PATH"] (reverse rest)
  let mSubdir = case subdir of
          [] -> Nothing
          _ -> Just $ Utils.joinFragments subdir
  return (mSubdir, Utils.joinFragments path, name)


instance Search (ResultLocation Buck.Entity) where
  symbolSearch toks = case toks of
    ("t" : locator) -> do
      case parse locator of
        Just (subdir, path, name) ->
          searchSymbolId toks $ searchByFQN subdir path name
        Nothing -> invalid_symbol
    ("f" : path) -> do
      searchSymbolId toks $ searchFile (Utils.joinFragments path)
    ("d" : module_name) -> do
      case reverse module_name of
        name : rev_module -> searchSymbolId toks $ searchDefinition
          (Utils.joinFragments $ reverse rev_module) name
        _ -> invalid_symbol
    _ -> invalid_symbol
    where
      invalid_symbol = return $ None "Buck.symbolSearch: invalid symbol"


searchByFQN :: Maybe Text -> Text -> Text -> Angle (ResultLocation Buck.Entity)
searchByFQN mSubdir path name =
  vars $ \(ent :: Angle Buck.Entity) (file :: Angle Src.File)
    (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
  tuple (ent, file, rangespan, lname) `where_` [
    wild .= predicate @Buck.SearchByFQN (
      rec $
        field @"subdir" subDirQ $
        field @"path" (string path) $
        field @"name" (string name) $
        field @"entity" ent
      end),
      entityLocation (alt @"buck" ent) file rangespan lname
  ]
  where
    subDirQ = case mSubdir of
      Just str -> just (string str)
      Nothing -> nothing

searchFile :: Text -> Angle (ResultLocation Buck.Entity)
searchFile path =
  vars $ \(ent :: Angle Buck.Entity) (file :: Angle Src.File)
    (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
  tuple (ent, file, rangespan, lname) `where_` [
    wild .= predicate @Buck.SearchFile (
      rec $
        field @"file" (string path) $
        field @"entity" ent
      end),
      entityLocation (alt @"buck" ent) file rangespan lname
  ]

searchDefinition :: Text -> Text -> Angle (ResultLocation Buck.Entity)
searchDefinition module_ name =
  vars $ \(ent :: Angle Buck.Entity) (file :: Angle Src.File)
    (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
  tuple (ent, file, rangespan, lname) `where_` [
    wild .= predicate @Buck.SearchDefinition (
      rec $
        field @"module" (string module_) $
        field @"name" (string name) $
        field @"entity" ent
      end),
      entityLocation (alt @"buck" ent) file rangespan lname
  ]
