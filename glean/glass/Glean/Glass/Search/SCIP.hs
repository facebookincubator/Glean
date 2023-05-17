{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.Search.SCIP
  ( {- instances -}
  ) where

import Data.Text as Text ( Text )
import qualified Data.Text as Text
import Data.Coerce

import Glean.Angle as Angle

import Glean.Glass.Search.Class
import Glean.Glass.Query ( entityLocation )

import qualified Glean.Schema.CodeScip.Types as SCIP ( Entity(..) )
import qualified Glean.Schema.Scip.Types as Scip
import qualified Glean.Schema.Src.Types as Src
import qualified Glean.Schema.CodemarkupTypes.Types as Code

instance Search SCIP.Entity where
  symbolSearch [] = return $ None "LSIF.symbolSearch: empty"
  -- global symbols
  symbolSearch toks = searchSymbolId toks $
    searchBySymbol (Text.intercalate " " toks)

searchBySymbol :: Text -> Angle (ResultLocation SCIP.Entity)
searchBySymbol sym =
  vars $ \(ent :: Angle Scip.Entity) (file :: Angle Src.File)
    (symbol :: Angle Scip.Symbol)
      (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
    tuple (cast ent,file,rangespan,lname) `where_` [
      symbol .= predicate @Scip.Symbol (string sym),
      wild .= predicate @Scip.SearchBySymbol (
        rec $
          field @"symbol" (asPredicate symbol) $
          field @"entity" ent
        end),
      entityLocation (alt @"scip" (cast ent)) file rangespan lname
    ]

-- we are coercing (silently) scip.Entity to code.scip.Entity
-- We do this due to the file -> language tag conversion (see LSIF)
cast :: Angle Scip.Entity -> Angle SCIP.Entity
cast = coerce
