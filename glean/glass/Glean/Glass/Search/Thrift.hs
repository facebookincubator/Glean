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
      sig,
      AngleStatement
      )

import Glean.Glass.Search.Class
import Glean.Glass.Query ( entityLocation )
import Glean.Glass.Utils ( joinFragments )

import Glean.Angle (Type)
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Fbthrift.Types as Thrift
import qualified Glean.Schema.CodeFbthrift.Types as Thrift
import qualified Glean.Schema.Src.Types as Src
import Glean.Haxl.Repos as Glean
import Data.Typeable (Typeable)

-- Depending on context (e.g. find-references or describe), we want to
--  decode a symbol id into an entity with our without its location.
--
--  This module defines the corresponding Search instances and Angle queries

instance Search (ResultLocation Thrift.Entity) where
  symbolSearch toks =
    fmap (mapResultLocation Thrift.Entity_decl) <$> symbolSearch toks

instance Search Thrift.Entity where
  symbolSearch toks = fmap Thrift.Entity_decl <$> symbolSearch toks

instance Search (ResultLocation Thrift.Declaration) where
  symbolSearch = symbolSearchGen searchQNameWithLoc searchThriftFileWithLoc
    searchMemberDeclWithLoc

instance Search Thrift.Declaration where
  symbolSearch =
    symbolSearchGen searchQName searchThriftFile searchMemberDecl

-- Resolve symbol id tokens into an entity, with or without location.
symbolSearchGen ::
  (Typeable t, Show t, Type t, QueryRepos u)
  => (Text -> Text -> Angle t)
  -> (Text -> Angle t)
  -> (Text -> Text -> Text -> Angle t)
  -> [Text]
  -> ReposHaxl u v (SearchResult t)
symbolSearchGen searchQName searchThriftFile searchMemberDecl toks =
  case toks of
  [] -> return $ None "Thrift.symbolSearch: empty query"
  _ -> case (init toks, last toks) of
    (pieces, name) -> do
      let path = joinFragments pieces
      result <- searchSymbolId toks $ searchQName path name
      case result of
        None{} -> case (init pieces, last pieces) of
          (morePieces, serviceName) -> do
            let path = joinFragments morePieces
            moreResult <-
              searchSymbolId toks $ searchMemberDecl path serviceName name
            case moreResult of
              None{} ->
                searchSymbolId toks $ searchThriftFile (joinFragments toks)
              r -> return r
        r -> return r

thriftFileDecl
  :: Text -> Angle Src.File -> Angle Thrift.File -> Angle Thrift.Declaration
  -> [AngleStatement]
thriftFileDecl path file thriftFile decl =
    [
      file .= predicate @Src.File (string path),
      thriftFile .= predicate @Thrift.File file,
      decl .= sig (alt @"include_"
              (asPredicate thriftFile) :: Angle Thrift.Declaration)
    ]

searchThriftFileWithLoc :: Text -> Angle (ResultLocation Thrift.Declaration)
searchThriftFileWithLoc path = vars $ \(file :: Angle Src.File)
  (thriftFile :: Angle Thrift.File) (decl :: Angle Thrift.Declaration)
       (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text) ->
    tuple (decl, file, rangespan, lname) `where_` (
    thriftFileDecl path file thriftFile decl ++ [
      entityLocation (alt @"fbthrift" (alt @"decl" decl)) file rangespan lname
    ])

searchThriftFile :: Text -> Angle Thrift.Declaration
searchThriftFile path = vars $ \(file :: Angle Src.File)
  (thriftFile :: Angle Thrift.File) (decl :: Angle Thrift.Declaration) ->
    decl `where_` thriftFileDecl path file thriftFile decl

thriftMemberDecl
  :: Text -> Text -> Text -> Angle Src.File -> Angle Thrift.File
  -> Angle Thrift.QualName -> Angle Thrift.Identifier -> Angle Thrift.XRefTarget
  -> [AngleStatement]
thriftMemberDecl
  path typename name file thriftFile qname thriftIdentifier decl =
 [
    file .= predicate @Src.File (string path),
    thriftFile .= predicate @Thrift.File file,
    thriftIdentifier .= predicate @Thrift.Identifier (string name),
    qname .= predicate @Thrift.QualName (
      rec $
        field @"file" (asPredicate thriftFile) $
        field @"name" (string typename)
      end
    ),
    wild .= predicate @Thrift.DeclarationMember (
      rec $
        field @"qname" (asPredicate qname) $
        field @"member" (asPredicate thriftIdentifier) $
        field @"decl" decl
      end)
  ]

-- Member lookup. This will find fields (struct, exception, union), enum values,
-- and functions. They are uniquely indentified by a triple
-- (path, typename, name)
searchMemberDecl
  :: Text -> Text -> Text -> Angle Thrift.Declaration
searchMemberDecl path typename name =
  vars $ \(file :: Angle Src.File) (qname :: Angle Thrift.QualName)
     (thriftFile :: Angle Thrift.File) (identifier :: Angle Thrift.Identifier)
     (decl :: Angle Thrift.Declaration) ->
  decl `where_`
    thriftMemberDecl path typename name file thriftFile qname identifier decl

-- Same as searchMemberDecl but with location
searchMemberDeclWithLoc
  :: Text -> Text -> Text -> Angle (ResultLocation Thrift.Declaration)
searchMemberDeclWithLoc path typename name =
  vars $ \(file :: Angle Src.File)
     (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text)
     (qname :: Angle Thrift.QualName)
     (thriftFile :: Angle Thrift.File) (identifier :: Angle Thrift.Identifier)
     (decl :: Angle Thrift.Declaration) ->

  tuple (decl, file, rangespan, lname) `where_`
  (thriftMemberDecl path typename name file thriftFile qname identifier decl
  ++ [
    entityLocation (alt @"fbthrift" (alt @"decl" decl)) file rangespan lname
  ])

qNameDecl
  :: Text -> Text -> Angle Thrift.File -> Angle Thrift.QualName
  -> Angle Src.File -> Angle Thrift.XRefTarget -> [AngleStatement]
qNameDecl path name thriftFile qname file decl =
  [ file .= predicate @Src.File (string path),
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
    end)]

-- Entity lookup: this will find type declarations (union, struct,
-- typedef), exceptions, constants and service names, which are uniquely
-- identified by a qualified name (path, name)
searchQName :: Text -> Text -> Angle Thrift.Declaration
searchQName path name =
  vars $ \(file :: Angle Src.File) (qname :: Angle Thrift.QualName)
     (thriftFile :: Angle Thrift.File) (decl :: Angle Thrift.Declaration) ->
  decl `where_` qNameDecl path name thriftFile qname file decl

-- Same as searchQName but also get location
searchQNameWithLoc :: Text -> Text -> Angle (ResultLocation Thrift.Declaration)
searchQNameWithLoc path name =
  vars $ \(file :: Angle Src.File)
     (rangespan :: Angle Code.RangeSpan) (lname :: Angle Text)
     (qname :: Angle Thrift.QualName)
     (thriftFile :: Angle Thrift.File) (decl :: Angle Thrift.Declaration) ->

  tuple (decl, file, rangespan, lname) `where_`
    (qNameDecl path name thriftFile qname file decl ++ [
    entityLocation (alt @"fbthrift" (alt @"decl" decl)) file rangespan lname
  ])
