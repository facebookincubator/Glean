{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.Search.GraphQL
  ( {- instances -}
  ) where

import Data.Text ( Text )

import Glean.Angle as Angle

import Glean.Glass.Search.Class
import Glean.Glass.Query ( entityLocation )

import qualified Glean.Schema.CodeGraphql.Types as GraphQL
import qualified Glean.Schema.Graphql.Types as GraphQL
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Src.Types as Src

--
-- Recover GraphQL entities from their Symbol ID encoding
--

instance Search (ResultLocation GraphQL.Entity) where
  symbolSearch toks
    | [name] <- toks -- fragment
    = searchSymbolId toks $ searchFragment name

    | otherwise = return $ None "GraphQL.symbolSearch: unsupported symbol id"

searchFragment :: Text -> Angle (ResultLocation GraphQL.Entity)
searchFragment fname =
  vars $ \(ent :: Angle GraphQL.Entity) (file :: Angle Src.File)
    (fragment :: Angle GraphQL.Fragment) (decl :: Angle GraphQL.Declaration)
    (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
  tuple (ent, file, rangespan, lname) `where_` [

    fragment .= predicate @GraphQL.Fragment (
      rec $
        field @"name" (string fname)
      end),
    alt @"fragment_" (asPredicate fragment) .= sig decl,
    alt @"decl" decl .= sig ent,
    entityLocation (alt @"graphql" ent) file rangespan lname
  ]
