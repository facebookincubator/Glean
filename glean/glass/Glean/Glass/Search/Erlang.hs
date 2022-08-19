{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Glean.Glass.Search.Erlang
  ( {- instances -}
  ) where

import Data.Text ( Text, unpack )

import Glean.Angle as Angle

import Glean.Glass.Search.Class
import Glean.Glass.Query ( entityLocation )

import qualified Glean.Schema.CodeErlang.Types as Erlang
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.SearchErlang.Types as Erlang
import qualified Glean.Schema.Src.Types as Src
import Text.Read ( readMaybe )
import Data.Word ( Word64 )

instance Search Erlang.Entity where
  symbolSearch toks
    | [module_, name, arity] <- toks
    , Just arityNum <- readMaybe $ unpack arity = do
        searchSymbolId toks $ searchByFQN module_ name arityNum
    | otherwise = return $ None "Erlang.symbolSearch: invalid query"

searchByFQN :: Text -> Text -> Word64 -> Angle (ResultLocation Erlang.Entity)
searchByFQN module_ name arity =
  vars $ \(ent :: Angle Erlang.Entity) (file :: Angle Src.File)
    (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
  tuple (ent, file, rangespan, lname) `where_` [
    wild .= predicate @Erlang.SearchByFQN (
      rec $
        field @"module" (string module_) $
        field @"name" (string name) $
        field @"arity" (nat arity) $
        field @"entity" ent
      end),
    entityLocation (alt @"erlang" ent) file rangespan lname
  ]
