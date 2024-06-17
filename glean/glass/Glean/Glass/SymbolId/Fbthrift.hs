{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.SymbolId.Fbthrift
  ({- instances -})
  where

import Data.Maybe
import Data.Text ( intercalate, stripSuffix )

import Glean.Glass.SymbolId.Class
import Glean.Glass.Types (Name(..))

import qualified Glean

import qualified Glean.Schema.Fbthrift.Types as Thrift

import Glean.Glass.Utils ( pathFragments )
import qualified Glean.Schema.Src.Types as Src

instance Symbol Thrift.XRefTarget where
  toSymbol x = case x of
    Thrift.XRefTarget_include_ thrift_file -> toSymbolPredicate thrift_file
    Thrift.XRefTarget_named named -> toSymbolPredicate named
    Thrift.XRefTarget_exception_ exception_ -> toSymbolPredicate exception_
    Thrift.XRefTarget_service_ service_ -> toSymbolPredicate service_
    Thrift.XRefTarget_constant constant -> toSymbolPredicate constant
    Thrift.XRefTarget_enumValue enumvalue -> toSymbolPredicate enumvalue
    Thrift.XRefTarget_function_ fn -> toSymbolPredicate fn
    Thrift.XRefTarget_field fn -> toSymbolPredicate fn
    Thrift.XRefTarget_EMPTY -> return []

instance Symbol Src.File where
  toSymbol k = pathFragments <$> Glean.keyOf k

instance Symbol Thrift.File where
  toSymbol f = toSymbolPredicate f

instance Symbol Thrift.NamedDecl_key where
  toSymbol (Thrift.NamedDecl_key (Thrift.NamedType qualname _kind)) =
    toSymbolPredicate qualname

instance Symbol Thrift.EnumValue_key where
  toSymbol (Thrift.EnumValue_key (Thrift.NamedType qualname _kind) name) =
    qualname <:> name

instance Symbol Thrift.FieldDecl_key where
  toSymbol (Thrift.FieldDecl_key qualname _kind name) =
    qualname <:> name

instance Symbol Thrift.Constant_key where
  toSymbol (Thrift.Constant_key qualname) =
    toSymbolPredicate qualname

instance Symbol Thrift.ServiceName where
  toSymbol (Thrift.ServiceName _ key) = toSymbol key

instance Symbol Thrift.ServiceName_key where
  toSymbol (Thrift.ServiceName_key qualname) =
    toSymbolPredicate qualname

instance Symbol Thrift.FunctionName_key where
  toSymbol (Thrift.FunctionName_key service name) = service <:> name

instance Symbol Thrift.ExceptionName_key where
  toSymbol (Thrift.ExceptionName_key qualname) =
    toSymbolPredicate qualname

instance Symbol Thrift.QualName_key where
  toSymbol (Thrift.QualName_key file name) = file <:> name

instance Symbol Thrift.QualName where
  toSymbol = toSymbolPredicate

instance Symbol Thrift.Identifier where
  toSymbol identifier = do
    n <- Glean.keyOf identifier
    return [n]

instance ToQName Thrift.XRefTarget where
  toQName e = case e of
    Thrift.XRefTarget_include_ file -> do
      thriftFile <- Glean.keyOf file
      path <- Glean.keyOf thriftFile
      return $ case reverse (pathFragments path) of
        [] -> Left "QName not supported for empty thrift path"
        (h:t) -> Right (Name h, Name (intercalate "." (reverse t)))
    Thrift.XRefTarget_named x -> Glean.keyOf x >>= toQName
    Thrift.XRefTarget_exception_ x -> Glean.keyOf x >>= toQName
    Thrift.XRefTarget_service_ x -> Glean.keyOf x >>= toQName
    Thrift.XRefTarget_constant x -> Glean.keyOf x >>= toQName
    Thrift.XRefTarget_enumValue x -> Glean.keyOf x >>= toQName
    Thrift.XRefTarget_function_ x -> Glean.keyOf x >>= toQName
    Thrift.XRefTarget_field x -> Glean.keyOf x >>= toQName
    Thrift.XRefTarget_EMPTY -> return $ Left "unknown thrift.Declaration"

instance ToQName Thrift.NamedDecl_key where
  toQName (Thrift.NamedDecl_key (Thrift.NamedType qname _kind)) = do
    toQName =<< Glean.keyOf qname

instance ToQName Thrift.ExceptionName_key where
  toQName (Thrift.ExceptionName_key qname ) = do
    toQName =<< Glean.keyOf qname

instance ToQName Thrift.ServiceName_key where
  toQName (Thrift.ServiceName_key qname ) = do
    toQName =<< Glean.keyOf qname

instance ToQName Thrift.FunctionName_key where
  toQName (Thrift.FunctionName_key service name ) = do
    eName <- toQName =<< Glean.keyOf service
    case eName of
      Left err -> pure (Left err)
      Right (service, _ ) -> do
        str <- Glean.keyOf name
        return $ Right (Name str, service)

instance ToQName Thrift.Constant_key where
  toQName (Thrift.Constant_key qname ) = do
    toQName =<< Glean.keyOf qname

instance ToQName Thrift.EnumValue_key where
  toQName (Thrift.EnumValue_key (Thrift.NamedType qname _kind) ident) = do
    Thrift.QualName_key _file parent <- Glean.keyOf qname
    name <- Glean.keyOf ident
    parentName <- Glean.keyOf parent
    return $ Right (Name name, Name parentName)

instance ToQName Thrift.FieldDecl_key where
  toQName (Thrift.FieldDecl_key qname _kind ident) = do
    Thrift.QualName_key _file parent <- Glean.keyOf qname
    name <- Glean.keyOf ident
    parentName <- Glean.keyOf parent
    return $ Right (Name name, Name parentName)

instance ToQName Thrift.QualName_key where
  toQName (Thrift.QualName_key file ident) = do
    path <- Glean.keyOf =<< Glean.keyOf file
    let parent = case pathFragments path of
          [] -> ""
          xs -> let f = last xs in fromMaybe f (stripSuffix ".thrift" f)
    str <- Glean.keyOf ident
    return (Right (Name str, Name parent))
