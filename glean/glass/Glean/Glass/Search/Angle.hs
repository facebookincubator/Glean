{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.Search.Angle
  ( {- instances -}
  ) where

import Glean.Angle as Angle
import Glean.Glass.Search.Class
import Glean.Glass.Query ( entityLocation )
import qualified Glean.Schema.CodeAnglelang.Types as A
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.SearchAnglelang.Types as A
import qualified Glean.Schema.Src.Types as Src
import Data.Text ( Text )

instance Search (ResultLocation A.Entity) where
  symbolSearch toks = case reverse toks of
    [] -> return $ None "Anglelang.symbolSearch: empty"
    -- name is the last token
    (name:_) -> searchSymbolId toks $ searchByName name

searchByName :: Text -> Angle (ResultLocation A.Entity)
searchByName sym =
  vars $ \(ent :: Angle A.Entity) (file :: Angle Src.File)
    (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
  tuple (ent, file, rangespan, lname) `where_` [
    wild .= predicate @A.SearchByName (
      rec $
        field @"name" (string sym) $
        field @"entity" ent
      end),
    entityLocation (alt @"angle" ent) file rangespan lname
  ]
