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
import Util.Text

import Glean.Angle as Angle

import Glean.Glass.Search.Class
import Glean.Glass.Query ( entityLocation )
import Glean.Glass.Utils ( joinFragments )

import qualified Glean.Schema.CodeLsif.Types as Lsif ( Entity(..) )
import qualified Glean.Schema.Lsif.Types as LSIF
import qualified Glean.Schema.Src.Types as Src
import qualified Glean.Schema.CodemarkupTypes.Types as Code

instance Search (ResultLocation Lsif.Entity) where
  symbolSearch [] = return $ None "LSIF.symbolSearch: empty"

  -- case 2: purely a moniker
  symbolSearch toks@("lsif":rest) =
    searchSymbolId toks $ searchByMoniker (joinFragments rest)

  -- case 1a) <local> with no identifier
  symbolSearch toks
    | ceT:leT:cbT:lbT:"<local>":revpath <- reverse toks
    , Right lb <- textToInt lbT
    , Right cb <- textToInt cbT
    , Right le <- textToInt leT
    , Right ce <- textToInt ceT
    = searchSymbolId toks $ searchByExactLocation
        (joinFragments (reverse revpath)) lb cb le ce

  -- case 1b) <local> with identifier
  symbolSearch toks
    | ceT:leT:cbT:lbT:name:"<local>":revpath <- reverse toks
    , Right lb <- textToInt lbT
    , Right cb <- textToInt cbT
    , Right le <- textToInt leT
    , Right ce <- textToInt ceT
    = searchSymbolId toks $ searchByExactLocationAndName
        (joinFragments (reverse revpath)) name lb cb le ce

  -- case 3: only path and name
    | name:revpath <- rtoks
    = searchSymbolId toks $ searchNonLocalByLocation
        (joinFragments (reverse revpath)) name
    where
      rtoks = reverse toks

  -- anything else is malformed
  symbolSearch _ = return $ None "LSIF.symbolSearch: malformed symbol id"

searchByMoniker :: Text -> Angle (ResultLocation Lsif.Entity)
searchByMoniker ident =
  vars $ \(ent :: Angle Lsif.Entity) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
    tuple (ent,file,rangespan,lname) `where_` [
      wild .= predicate @LSIF.SearchByMoniker (
        rec $
          field @"ident" (string ident) $
          field @"entity" (cast ent)
        end),
      entityLocation (alt @"lsif" ent) file rangespan lname
    ]

searchNonLocalByLocation
  :: Text -> Text -> Angle (ResultLocation Lsif.Entity)
searchNonLocalByLocation path name =
  vars $ \(ent :: Angle Lsif.Entity) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
    tuple (ent,file,rangespan,lname) `where_` [
      file .= predicate @Src.File (string path),
      wild .= predicate @LSIF.SearchNonLocalByLocation (
        rec $
          field @"file" (asPredicate file) $
          field @"name" (string name) $
          field @"entity" (cast ent)
        end),
      entityLocation (alt @"lsif" ent) file rangespan lname
    ]

searchByExactLocation
  :: Text -> Int -> Int -> Int -> Int
  -> Angle (ResultLocation Lsif.Entity)
searchByExactLocation path lb cb le ce =
  vars $ \(ent :: Angle Lsif.Entity) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
    tuple (ent,file,rangespan,lname) `where_` [
      file .= predicate @Src.File (string path),
      wild .= predicate @LSIF.SearchByExactLocation (
        rec $
          field @"file" (asPredicate file) $
          field @"span" (
            rec $
              field @"lineBegin" (nat (fromIntegral lb)) $
              field @"columnBegin" (nat (fromIntegral cb)) $
              field @"lineEnd" (nat (fromIntegral le)) $
              field @"columnEnd" (nat (fromIntegral ce - 1))
            end) $
          field @"entity" (cast ent)
        end),
      entityLocation (alt @"lsif" ent) file rangespan lname
    ]

searchByExactLocationAndName
  :: Text -> Text -> Int -> Int -> Int -> Int
  -> Angle (ResultLocation Lsif.Entity)
searchByExactLocationAndName path name lb cb le ce =
  vars $ \(ent :: Angle Lsif.Entity) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
    tuple (ent,file,rangespan,lname) `where_` [
      file .= predicate @Src.File (string path),
      wild .= predicate @LSIF.SearchByExactLocationAndName (
        rec $
          field @"file" (asPredicate file) $
          field @"name" (string name) $
          field @"span" (
            rec $
              field @"lineBegin" (nat (fromIntegral lb)) $
              field @"columnBegin" (nat (fromIntegral cb)) $
              field @"lineEnd" (nat (fromIntegral le)) $
              field @"columnEnd" (nat (fromIntegral ce - 1))
            end) $
          field @"entity" (cast ent)
        end),
      entityLocation (alt @"lsif" ent) file rangespan lname
    ]

-- we are coercing (silently) lsif.Entity to code.lsif.Entity
cast :: Angle Lsif.Entity -> Angle LSIF.Entity
cast = coerce
