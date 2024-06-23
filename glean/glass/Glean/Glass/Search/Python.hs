{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}

module Glean.Glass.Search.Python
  ( {- instances -}
  ) where

import Glean.Angle as Angle
    ( AngleVars(vars),
      AngleStatement,
      Angle,
      predicate,
      where_,
      (.=),
      string,
      stringPrefix,
      tuple,
      rec,
      alt,
      field,
      end,
      wild )

import Glean.Glass.Search.Class
import Glean.Glass.Query ( entityLocation )

import qualified Glean.Schema.CodePython.Types as Py
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Python.Types as Py
import qualified Glean.Schema.Src.Types as Src
import Data.Text (Text)

instance Search (ResultLocation Py.Entity) where
  symbolSearch params =
    fmap (mapResultLocation Py.Entity_decl) <$> symbolSearch params

instance Search (ResultLocation Py.Declaration) where
  symbolSearch t@[fqname] = searchSymbolId t $
    findByFQName fqname Nothing
  symbolSearch t@[loc, fqname] = searchSymbolId t $
    findByFQName fqname (Just loc)
  symbolSearch _ = return $
    None "Python.symbolSearch: invalid query"

findByFQName
  :: Text -> Maybe Text
  -> Angle (ResultLocation Py.Declaration)
findByFQName = searchByFQName False

searchByFQName
  :: Bool -> Text -> Maybe Text
  -> Angle (ResultLocation Py.Declaration)
searchByFQName isPrefix fqname mloc =
  vars $ \decl (file :: Angle Src.File) (rangespan :: Angle Code.RangeSpan)
      (lname :: Angle Text) ->
    tuple (decl, file, rangespan, lname) `where_` ([
        wild .= predicate @Py.DeclarationWithName (
          rec $
              field @"name" (stringOrPrefix fqname) $
              field @"declaration" decl
          end),
        entityLocation (alt @"python" (alt @"decl" decl)) file rangespan lname
      ] <> location file)
  where
    location :: Angle Src.File -> [AngleStatement]
    location file
      | Just "." <- mloc
      = []
      | Just loc <- mloc
      = [file .= predicate @Src.File (stringPrefix loc)]
      | otherwise
      = []
    stringOrPrefix = if isPrefix then stringPrefix else string
