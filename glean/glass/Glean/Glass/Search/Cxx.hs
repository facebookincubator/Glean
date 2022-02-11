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
import qualified Glean.Schema.Codemarkup.Types as Code
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.SearchCxx.Types as Cxx
import qualified Glean.Schema.Src.Types as Src

instance Search Cxx.Entity where
  symbolSearch [] =
    return $ None "Cxx.symbolSearch: empty path"
  symbolSearch t@[name] = do
    runSearch t $ searchByNameAndScope False [] name
  symbolSearch t@(_:_) = do
    runSearch t $ searchByNameAndScope False (init t) (last t)

--
-- Entity search for C++
--
searchByNameAndScope
  :: Bool -> [Text] -> Text -> Angle (ResultLocation Cxx.Entity)
searchByNameAndScope isPrefix ns name =
  vars $ \(entity :: Angle Cxx.Entity) (file :: Angle Src.File)
      (rangespan :: Angle Code.RangeSpan) ->
    tuple (entity, file, rangespan) `where_` [
      wild .= predicate @Cxx.SearchByNameAndScope (
        rec $
          field @"name" (stringOrPrefix name) $
          field @"scope" (scopeQuery ns) $
          field @"entity" entity
        end),
      entityLocation (alt @"cxx" entity) file rangespan
    ]
  where
    stringOrPrefix = if isPrefix then stringPrefix else string
    scopeQuery ns = scope (reverse ns)

-- | Varieties of namespaces and scopes
scope :: [Text] -> Angle Cxx.Scope
scope ns = case ns of
  [] -> alt @"global_" wild
  n:ns' ->
    alt @"namespace_" (namespaceQName ns)
    .|
    alt @"recordWithAccess" (rec $
        field @"record" (rec $
          field @"name" (string n) $
          field @"scope" (scope ns')
        end)
      end)
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
