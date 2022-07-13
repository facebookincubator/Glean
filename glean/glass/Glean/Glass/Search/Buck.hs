{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.Search.Buck
  ( {- instances -}
  ) where

import Data.Text ( Text )

import Glean.Angle as Angle

import Glean.Glass.Search.Class
    ( runSearch, ResultLocation, Search(..), SearchResult(None) )
import Glean.Glass.Query ( entityLocation )

import qualified Glean.Schema.CodeBuck.Types as Buck
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.SearchBuck.Types as Buck
import qualified Glean.Schema.Src.Types as Src

instance Search Buck.Entity where
  symbolSearch toks@[path, name] = do
    runSearch toks $ searchByFQN Nothing path name
  symbolSearch toks@[subdir, path, name] = do
    runSearch toks $ searchByFQN (Just subdir) path name
  symbolSearch _ = return $ None "Buck.symbolSearch: invalid symbol"

searchByFQN :: Maybe Text -> Text -> Text -> Angle (ResultLocation Buck.Entity)
searchByFQN mSubdir path name =
  vars $ \(ent :: Angle Buck.Entity) (file :: Angle Src.File)
    (rangespan :: Angle Code.RangeSpan) ->
  tuple (ent, file, rangespan) `where_` [
    wild .= predicate @Buck.SearchByFQN (
      rec $
        field @"subdir" subDirQ $
        field @"path" (string path) $
        field @"name" (string name) $
        field @"entity" ent
      end),
      entityLocation (alt @"buck" ent) file rangespan
  ]
  where
    subDirQ = case mSubdir of
      Just str -> just (string str)
      Nothing -> nothing
