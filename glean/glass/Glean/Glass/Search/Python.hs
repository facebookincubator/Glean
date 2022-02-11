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

import Control.Monad.Catch (throwM)
import Glean.Angle as Angle

import Glean.Glass.Search.Class
import Glean.Glass.Query ( entityLocation )
import Glean.Glass.Types (ServerException(ServerException))
import Glean.Glass.Utils (searchWithLimit)

import qualified Glean.Schema.CodePython.Types as Py
import qualified Glean.Schema.Codemarkup.Types as Code
import qualified Glean.Schema.Python.Types as Py
import qualified Glean.Schema.Src.Types as Src
import Data.Text (Text)

instance Search Py.Entity where
  symbolSearch params = fmap Py.Entity_decl <$> symbolSearch params

instance Search Py.Declaration where
  symbolSearch t@[fqname] = runSearch t $
    findByFQName fqname Nothing
  symbolSearch t@[loc, fqname] = runSearch t $
    findByFQName fqname (Just loc)
  symbolSearch _ = return $
    None "Python.symbolSearch: invalid query"

instance PrefixSearch Py.Entity where
  prefixSearch lim params = fmap Py.Entity_decl <$> prefixSearch lim params

instance PrefixSearch Py.Declaration where
  prefixSearch lim [loc] = fmap resultToDecl <$> searchWithLimit (Just lim) $
    prefixSearchByFQName "" (Just loc)
  prefixSearch lim [loc, pfqname] = fmap resultToDecl <$> searchWithLimit (Just lim) $
    prefixSearchByFQName pfqname (Just loc)
  prefixSearch _ _ = throwM $ ServerException "Python.prefixSearch: too many /"

findByFQName
  :: Text -> Maybe Text
  -> Angle (ResultLocation Py.Declaration)
findByFQName = searchByFQName False

prefixSearchByFQName
  :: Text -> Maybe Text
  -> Angle (ResultLocation Py.Declaration)
prefixSearchByFQName = searchByFQName True

searchByFQName
  :: Bool -> Text -> Maybe Text
  -> Angle (ResultLocation Py.Declaration)
searchByFQName isPrefix fqname mloc =
  vars $ \decl (file :: Angle Src.File) (rangespan :: Angle Code.RangeSpan) ->
    tuple (decl, file, rangespan) `where_` ([
        wild .= predicate @Py.DeclarationWithName (
          rec $
              field @"name" (stringOrPrefix fqname) $
              field @"declaration" decl
          end),
        entityLocation (alt @"python" (alt @"decl" decl)) file rangespan
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
