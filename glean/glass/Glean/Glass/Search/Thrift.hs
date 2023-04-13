{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Glean.Glass.Search.Thrift
  ( {- instances -}
  ) where

import Data.Coerce ( coerce )
import Data.Text ( Text )

import Glean.Angle as Angle

import Glean.Glass.Search.Class
import Glean.Glass.Query ( entityLocation )
import Glean.Glass.Utils ( joinFragments )

import qualified Glean.Schema.CodeThrift.Types as Thrift
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Thrift.Types as Thrift
import qualified Glean.Schema.Src.Types as Src

instance Search Thrift.Entity where
  symbolSearch toks = case toks of
    [] -> return $ None "Thrift.symbolSearch: empty query"
    _ -> case (init toks, last toks) of
      (pieces, name) -> do
        let path = joinFragments pieces
        searchSymbolId toks $ searchQName path name

-- A basic entity lookup:  this will find named decls, exceptions, constants and
-- service names, which are indexd by QualName
-- The separate thrift.File fact is slightly annoying.
searchQName :: Text -> Text -> Angle (ResultLocation Thrift.Entity)
searchQName path name =
  vars $ \(ent :: Angle Thrift.Entity) (file :: Angle Src.File)
     (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text)
     (qname :: Angle Thrift.QualName)
     (thriftFile :: Angle Thrift.File)
     (decl :: Angle Thrift.Declaration) ->

  tuple (ent, file, rangespan, lname) `where_` [
    file .= predicate @Src.File (string path),
    thriftFile .= predicate @Thrift.File file,
    qname .= predicate @Thrift.QualName (
      rec $
        field @"file" (asPredicate thriftFile) $
        field @"name" (string name)
      end
    ),
    wild .= predicate @Thrift.DeclarationName (
      rec $
        field @"qname" (asPredicate qname) $
        field @"decl" decl
      end),
    ent .= cast decl,
    entityLocation (alt @"thrift" ent) file rangespan lname
  ]

-- we are coercing (silently) thrift.Declaration to code.thrift.Entity
-- these are the same shape but defined in two places
cast :: Angle Thrift.Declaration -> Angle Thrift.Entity
cast = coerce
