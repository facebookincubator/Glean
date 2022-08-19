{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications, ApplicativeDo, PartialTypeSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.Search.Cxx
  ( {- instances -}
  ) where

import Data.Text ( Text )

import Glean.Angle as Angle

import Glean.Glass.Search.Class
import Glean.Glass.Query ( entityLocation )

import qualified Glean.Schema.CodeCxx.Types as Cxx
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.SearchCxx.Types as Cxx
import qualified Glean.Schema.Src.Types as Src

instance Search Cxx.Entity where
  symbolSearch [] =
    return $ None "Cxx.symbolSearch: empty path"
  symbolSearch t@[name] = do
    searchSymbolId t $
      searchByScope t .|
      searchByNameAndScope [] name
  symbolSearch t@(_:_) = do
    searchSymbolId t $
      searchByScope t .|
      searchByNameAndScope (init t) (last t)

--
-- Entity search for C++. These should decode precisely
--
searchByScope
  :: [Text] -> Angle (ResultLocation Cxx.Entity)
searchByScope ns =
  vars $ \(entity :: Angle Cxx.Entity) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
    tuple (entity, file, rangespan, lname) `where_` [
      wild .= predicate @Cxx.SearchByScope (
        rec $
          field @"scope" (scopeQuery ns) $
          field @"entity" entity
        end),
      entityLocation (alt @"cxx" entity) file rangespan lname
    ]
  where
    scopeQuery ns = scope (reverse ns)

searchByNameAndScope
  :: [Text] -> Text -> Angle (ResultLocation Cxx.Entity)
searchByNameAndScope ns name =
  vars $ \(entity :: Angle Cxx.Entity) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
    tuple (entity, file, rangespan, lname) `where_` [
      wild .= predicate @Cxx.SearchByNameAndScope (
        rec $
          field @"name" (string name) $
          field @"scope" (scopeQuery ns) $
          field @"entity" entity
        end),
      entityLocation (alt @"cxx" entity) file rangespan lname
    ]
  where
    scopeQuery ns = scope (reverse ns)

--
-- Scope queries. There's lots of flavors.
--
scope :: [Text] -> Angle Cxx.Scope
scope [] = alt @"global_" wild
scope ns@(n:ns') =
  alt @"namespace_" (namespaceQName ns)
  .|
  alt @"recordWithAccess" (rec $
      field @"record" (rec $
        field @"name" (string n) $
        field @"scope" (scope ns')
      end)
    end)
  .|
  alt @"local" (functionQName n ns')

  where
    namespaceQName [] =
      rec $
        field @"name" nothing $
        field @"parent" nothing
      end
    namespaceQName (n:ns) =
      rec $
        field @"name" (if n == "" then nothing else alt @"just" (string n)) $
        field @"parent"
          (if null ns
            then nothing
            else just (namespaceQName ns))
      end

    functionQName n ns =
      rec $
        field @"name" (alt @"name" (string n)) $
        field @"scope" (scope ns)
      end
