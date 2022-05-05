{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.Search.LSIF
  ( {- instances -}
  ) where

import Data.Text as Text ( Text )
import Data.Coerce

import Glean.Angle as Angle

import Glean.Glass.Search.Class
import Glean.Glass.Query ( entityLocation )
import Glean.Glass.Utils ( joinFragments )

import qualified Glean.Schema.CodeLsif.Types as Lsif ( Entity(..) )
import qualified Glean.Schema.Lsif.Types as LSIF
import qualified Glean.Schema.Src.Types as Src
import qualified Glean.Schema.CodemarkupTypes.Types as Code

instance Search Lsif.Entity where
  symbolSearch [] = return $ None "LSIF.symbolSearch: empty"

  -- case 2: purely a moniker
  symbolSearch toks@("lsif":rest) =
    runSearch toks $ searchByMoniker (joinFragments rest)

  -- case 1: <local> with path/ name and moniker
  symbolSearch toks
    | ident:"<local>":name:revpath <- rtoks
    = runSearch toks $ searchByLocationAndMoniker
        (joinFragments (reverse revpath)) name ident

  -- case 3: only path and name
    | name:revpath <- rtoks
    = runSearch toks $ searchNonLocalByLocation
        (joinFragments (reverse revpath)) name
    where
      rtoks = reverse toks

  -- anything else is malformed
  symbolSearch _ = return $ None "LSIF.symbolSearch: malformed symbol id"

searchByMoniker :: Text -> Angle (ResultLocation Lsif.Entity)
searchByMoniker ident =
  vars $ \(ent :: Angle Lsif.Entity) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) ->
    tuple (ent,file,rangespan) `where_` [
      wild .= predicate @LSIF.SearchByMoniker (
        rec $
          field @"ident" (string ident) $
          field @"entity" (cast ent)
        end),
      entityLocation (alt @"lsif" ent) file rangespan
    ]

searchByLocationAndMoniker
  :: Text -> Text -> Text -> Angle (ResultLocation Lsif.Entity)
searchByLocationAndMoniker path name ident =
  vars $ \(ent :: Angle Lsif.Entity) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) ->
    tuple (ent,file,rangespan) `where_` [
      file .= predicate @Src.File (string path),
      wild .= predicate @LSIF.SearchByLocationAndMoniker (
        rec $
          field @"file" (asPredicate file) $
          field @"name" (string name) $
          field @"ident" (string ident) $
          field @"entity" (cast ent)
        end),
      entityLocation (alt @"lsif" ent) file rangespan
    ]

searchNonLocalByLocation
  :: Text -> Text -> Angle (ResultLocation Lsif.Entity)
searchNonLocalByLocation path name =
  vars $ \(ent :: Angle Lsif.Entity) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) ->
    tuple (ent,file,rangespan) `where_` [
      file .= predicate @Src.File (string path),
      wild .= predicate @LSIF.SearchNonLocalByLocation (
        rec $
          field @"file" (asPredicate file) $
          field @"name" (string name) $
          field @"entity" (cast ent)
        end),
      entityLocation (alt @"lsif" ent) file rangespan
    ]

-- we are coercing (silently) lsif.Entity to code.lsif.Entity
cast :: Angle Lsif.Entity -> Angle LSIF.Entity
cast = coerce
