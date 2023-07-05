{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.SymbolId.Java
  ({- instances -})
  where

import qualified Glean
import Glean.Glass.Types
import Glean.Glass.SymbolId.Class
import Glean.Haxl.Repos (RepoHaxl)
import Data.Text ( Text )
import qualified Data.Text as Text

import qualified Glean.Schema.CodeJava.Types as Java
import qualified Glean.Schema.JavaAlpha.Types as Java
import qualified Glean.Schema.JavakotlinAlpha.Types as Java

instance Symbol Java.Entity where
  toSymbol e = case e of
    Java.Entity_decl d -> toSymbol d
    Java.Entity_EMPTY -> return []

instance Symbol Java.Declaration where
  toSymbol e = case e of
    Java.Declaration_class_ c -> toSymbolPredicate c
    Java.Declaration_interface_ i -> toSymbolPredicate i
    Java.Declaration_enum_ e -> toSymbolPredicate e
    Java.Declaration_method e -> toSymbolPredicate e
    Java.Declaration_ctor e -> toSymbolPredicate e
    Java.Declaration_local e -> toSymbolPredicate e
    Java.Declaration_field e -> toSymbolPredicate e
    Java.Declaration_param e -> toSymbolPredicate e
    Java.Declaration_EMPTY -> return []

instance Symbol Java.ClassDeclaration_key where
  toSymbol Java.ClassDeclaration_key{..} =
    toSymbolPredicate classDeclaration_key_name

instance Symbol Java.InterfaceDeclaration_key where
  toSymbol Java.InterfaceDeclaration_key{..} =
    toSymbolPredicate interfaceDeclaration_key_name

instance Symbol Java.EnumDeclaration_key where
  toSymbol Java.EnumDeclaration_key{..} =
    toSymbolPredicate enumDeclaration_key_name

instance Symbol Java.MethodDeclaration_key where
  toSymbol Java.MethodDeclaration_key{..} =
    toSymbolPredicate methodDeclaration_key_name

instance Symbol Java.ConstructorDeclaration_key where
  toSymbol Java.ConstructorDeclaration_key{..} =
    toSymbolPredicate constructorDeclaration_key_name

instance Symbol Java.LocalDeclaration_key where
  toSymbol Java.LocalDeclaration_key{..} =
    toSymbolPredicate localDeclaration_key_name

instance Symbol Java.FieldDeclaration_key where
  toSymbol Java.FieldDeclaration_key{..} =
    toSymbolPredicate fieldDeclaration_key_name

instance Symbol Java.ParameterDeclaration_key where
  toSymbol Java.ParameterDeclaration_key{..} =
    toSymbolPredicate parameterDeclaration_key_name

instance Symbol Java.MethodName_key where
  toSymbol Java.MethodName_key{..} = toSymbolPredicate methodName_key_name
    -- todo add method type signatures for overloading?

instance Symbol Java.Path_key where
  toSymbol Java.Path_key{..} = path_key_container <:> path_key_base

instance Symbol Java.Path where
  toSymbol path = toSymbol =<< Glean.keyOf path

instance Symbol Java.QName_key where
  toSymbol Java.QName_key{..} = qName_key_context <:> qName_key_name

instance Symbol Java.Name where
  toSymbol name = (:[]) <$> Glean.keyOf name

instance ToQName Java.Entity where
  toQName e = case e of
    Java.Entity_decl d -> toQName d
    Java.Entity_EMPTY -> return $ Left "unknown Java.Entity"

instance ToQName Java.Declaration where
  toQName d = case d of
    Java.Declaration_enum_ e -> Glean.keyOf e >>= toQName
    Java.Declaration_class_ e -> Glean.keyOf e >>= toQName
    Java.Declaration_interface_ e -> Glean.keyOf e >>= toQName
    Java.Declaration_method e -> Glean.keyOf e >>= toQName
    Java.Declaration_ctor e -> Glean.keyOf e >>= toQName
    Java.Declaration_field e -> Glean.keyOf e >>= toQName
    Java.Declaration_local e -> Glean.keyOf e >>= toQName
    Java.Declaration_param e -> Glean.keyOf e >>= toQName
    Java.Declaration_EMPTY -> return $ Left "Unknown Java.Declaration type"

instance ToQName Java.ClassDeclaration_key where
  toQName Java.ClassDeclaration_key{..} =
    Glean.keyOf classDeclaration_key_name >>= toQName

instance ToQName Java.InterfaceDeclaration_key where
  toQName Java.InterfaceDeclaration_key{..} =
    Glean.keyOf interfaceDeclaration_key_name >>= toQName

instance ToQName Java.EnumDeclaration_key where
  toQName Java.EnumDeclaration_key{..} =
    Glean.keyOf enumDeclaration_key_name >>= toQName

instance ToQName Java.MethodDeclaration_key where
  toQName Java.MethodDeclaration_key{..} =
    Glean.keyOf methodDeclaration_key_name >>= toQName

instance ToQName Java.ConstructorDeclaration_key where
  toQName Java.ConstructorDeclaration_key{..} =
    Glean.keyOf constructorDeclaration_key_name >>= toQName

instance ToQName Java.LocalDeclaration_key where
  toQName Java.LocalDeclaration_key{..} =
    Glean.keyOf localDeclaration_key_name >>= toQName

instance ToQName Java.FieldDeclaration_key where
  toQName Java.FieldDeclaration_key{..} =
    Glean.keyOf fieldDeclaration_key_name >>= toQName

instance ToQName Java.ParameterDeclaration_key where
  toQName Java.ParameterDeclaration_key{..} =
    Glean.keyOf parameterDeclaration_key_name >>= toQName

instance ToQName Java.MethodName_key where
  toQName Java.MethodName_key{..} = Glean.keyOf methodName_key_name >>= toQName

instance ToQName Java.QName_key where
  toQName Java.QName_key{..} = do
    nameStr <- Glean.keyOf qName_key_name
    context <- flattenContext =<< Glean.keyOf qName_key_context
    return $ Right (Name (Text.intercalate "." (reverse context)), Name nameStr)

flattenContext :: Java.Path_key -> RepoHaxl u w [Text]
flattenContext Java.Path_key{..} = do
  nameStr <- Glean.keyOf path_key_base
  case path_key_container of
    Nothing -> return [nameStr]
    Just path -> do
      rest <- flattenContext =<< Glean.keyOf path
      return (nameStr : rest)
