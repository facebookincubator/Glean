{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.Search.Haskell
  ( {- instances -}
  ) where

import Data.Text ( Text )
import qualified Data.Text as Text ( intercalate )

import Glean.Angle as Angle

import Glean.Glass.Search.Class
import Glean.Glass.Query ( entityLocation )

import qualified Glean.Schema.CodeHs.Types as Haskell
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.SearchHs.Types as Haskell
import qualified Glean.Schema.Src.Types as Src

instance Search Haskell.Entity where
  symbolSearch [] = return $ None "Haskell.symbolSearch: empty"
  symbolSearch toks = do
    runSearch toks $ searchByName $ Text.intercalate "." toks

-- code.hs:searchByName
searchByName :: Text -> Angle (ResultLocation Haskell.Entity)
searchByName sym =
  vars $ \(ent :: Angle Haskell.Entity) (file :: Angle Src.File)
    (rangespan :: Angle Code.RangeSpan) ->
  tuple (ent, file, rangespan) `where_` [
    wild .= predicate @Haskell.SearchByName (
      rec $
        field @"name" (string sym) $
        field @"entity" ent
      end),
    entityLocation (alt @"hs" ent) file rangespan
  ]
