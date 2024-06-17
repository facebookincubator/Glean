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

import Data.Text ( Text )

import Glean.Angle as Angle
    ( AngleVars(vars),
      Angle,
      predicate,
      where_,
      (.=),
      string,
      tuple,
      rec,
      alt,
      field,
      end,
      wild,
      asPredicate,
      sig )

import Glean.Glass.Search.Class
import Glean.Glass.Query ( entityLocation )
import Glean.Glass.Utils ( joinFragments )

import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Fbthrift.Types as Thrift
import qualified Glean.Schema.CodeFbthrift.Types as Thrift
import qualified Glean.Schema.Src.Types as Src

instance Search (ResultLocation Thrift.Entity) where
  symbolSearch toks =
    fmap (mapResultLocation Thrift.Entity_decl) <$> symbolSearch toks

instance Search (ResultLocation Thrift.Declaration) where
  symbolSearch toks = case toks of
    [] -> return $ None "Thrift.symbolSearch: empty query"
    _ -> case (init toks, last toks) of
      (pieces, name) -> do
        let path = joinFragments pieces
        result <- searchSymbolId toks $ searchQName path name
        case result of
          None{} -> case (init pieces, last pieces) of
            (morePieces, serviceName) -> do
              let path = joinFragments morePieces
              moreResult <- searchSymbolId toks $ searchFunctionName
                  path serviceName name
              case moreResult of
                None{} ->
                  searchSymbolId toks $ searchThriftFile (joinFragments toks)
                r -> return r
          r -> return r

-- A basic entity lookup:  this will find named decls, exceptions, constants and
-- service names, which are indexd by QualName
-- The separate thrift.File fact is slightly annoying.
searchQName :: Text -> Text -> Angle (ResultLocation Thrift.Declaration)
searchQName path name =
  vars $ \(file :: Angle Src.File)
     (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text)
     (qname :: Angle Thrift.QualName)
     (thriftFile :: Angle Thrift.File) (decl :: Angle Thrift.Declaration) ->

  tuple (decl, file, rangespan, lname) `where_` [
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
    entityLocation (alt @"fbthrift" (alt @"decl" decl)) file rangespan lname
  ]

searchFunctionName
  :: Text -> Text -> Text -> Angle (ResultLocation Thrift.Declaration)
searchFunctionName path servicename name =
  vars $ \(file :: Angle Src.File)
     (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text)
     (qname :: Angle Thrift.QualName)
     (thriftFile :: Angle Thrift.File) (decl :: Angle Thrift.Declaration) ->

  tuple (decl, file, rangespan, lname) `where_` [
    file .= predicate @Src.File (string path),
    thriftFile .= predicate @Thrift.File file,
    qname .= predicate @Thrift.QualName (
      rec $
        field @"file" (asPredicate thriftFile) $
        field @"name" (string servicename)
      end
    ),
    wild .= predicate @Thrift.FunctionDeclarationName (
      rec $
        field @"qname" (asPredicate qname) $
        field @"name" (string name) $
        field @"decl" decl
      end),
    entityLocation (alt @"fbthrift" (alt @"decl" decl)) file rangespan lname
  ]

searchThriftFile :: Text -> Angle (ResultLocation Thrift.Declaration)
searchThriftFile path = vars $ \(file :: Angle Src.File)
  (thriftFile :: Angle Thrift.File) (decl :: Angle Thrift.Declaration)
       (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
    tuple (decl, file, rangespan, lname) `where_` [
      file .= predicate @Src.File (string path),
      thriftFile .= predicate @Thrift.File file,
      decl .= sig (alt @"include_"
              (asPredicate thriftFile) :: Angle Thrift.Declaration),
      entityLocation (alt @"fbthrift" (alt @"decl" decl)) file rangespan lname
    ]
