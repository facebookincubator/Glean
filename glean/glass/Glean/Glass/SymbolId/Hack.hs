{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.SymbolId.Hack ( {- instances -} ) where

import qualified Glean
import qualified Glean.Haxl.Repos as Glean

import Glean.Glass.SymbolId.Class
import qualified Glean.Schema.Hack.Types as Hack
import Glean.Glass.Types (Name(..))

import Data.Text (intercalate)

--
-- Hack language name boilerplate
--
instance Symbol Hack.Declaration where
  toSymbol e = case e of
    Hack.Declaration_method x -> toSymbolPredicate x
    Hack.Declaration_enumerator x -> toSymbolPredicate x
    Hack.Declaration_container x -> toSymbol x
    Hack.Declaration_function_ x -> toSymbolPredicate x
    Hack.Declaration_classConst x -> toSymbolPredicate x
    Hack.Declaration_property_ x -> toSymbolPredicate x
    Hack.Declaration_typeConst x -> toSymbolPredicate x
    Hack.Declaration_globalConst x -> toSymbolPredicate x
    Hack.Declaration_namespace_ x -> toSymbolPredicate x
    Hack.Declaration_typedef_ x -> toSymbolPredicate x
    Hack.Declaration_module x -> toSymbolPredicate x
    Hack.Declaration_EMPTY -> return []

instance Symbol Hack.TypeConstDeclaration_key where
  toSymbol (Hack.TypeConstDeclaration_key name container) = container <:> name

instance Symbol Hack.PropertyDeclaration_key where
  toSymbol (Hack.PropertyDeclaration_key name container) = container <:> name

instance Symbol Hack.ClassConstDeclaration_key where
  toSymbol (Hack.ClassConstDeclaration_key name container) = container <:> name

instance Symbol Hack.MethodDeclaration_key where
  toSymbol (Hack.MethodDeclaration_key name container) = container <:> name

instance Symbol Hack.ModuleDeclaration_key where
  toSymbol (Hack.ModuleDeclaration_key name) = toSymbol name

instance Symbol Hack.EnumDeclaration_key where
  toSymbol (Hack.EnumDeclaration_key qn) = toSymbol qn

instance Symbol Hack.EnumDeclaration where
  toSymbol k = Glean.keyOf k >>= toSymbol

instance Symbol Hack.Enumerator_key where
  toSymbol (Hack.Enumerator_key name decl) = decl <:> name

instance Symbol Hack.FunctionDeclaration_key where
  toSymbol (Hack.FunctionDeclaration_key qn) = toSymbol qn

instance Symbol Hack.ContainerDeclaration where
  toSymbol (Hack.ContainerDeclaration_class_ c) = toSymbolPredicate c
  toSymbol (Hack.ContainerDeclaration_enum_ c) = toSymbolPredicate c
  toSymbol (Hack.ContainerDeclaration_interface_ c) = toSymbolPredicate c
  toSymbol (Hack.ContainerDeclaration_trait c) = toSymbolPredicate c
  toSymbol Hack.ContainerDeclaration_EMPTY = return []

instance Symbol Hack.ClassDeclaration_key where
  toSymbol (Hack.ClassDeclaration_key qn) = toSymbol qn

instance Symbol Hack.InterfaceDeclaration_key where
  toSymbol (Hack.InterfaceDeclaration_key qn) = toSymbol qn

instance Symbol Hack.TraitDeclaration_key where
  toSymbol (Hack.TraitDeclaration_key qn) = toSymbol qn

instance Symbol Hack.TypedefDeclaration_key where
  toSymbol (Hack.TypedefDeclaration_key qn) = toSymbol qn

instance Symbol Hack.NamespaceDeclaration_key where
  toSymbol (Hack.NamespaceDeclaration_key qn) = ("ns" :) <$> toSymbol qn

instance Symbol Hack.NamespaceDeclaration where
  toSymbol k = Glean.keyOf k >>= toSymbol

instance Symbol Hack.NamespaceQName where
  toSymbol k = Glean.keyOf k >>= toSymbol

instance Symbol Hack.NamespaceQName_key where
  toSymbol (Hack.NamespaceQName_key name parent) = parent <:> name

instance Symbol Hack.GlobalConstDeclaration_key where
  toSymbol (Hack.GlobalConstDeclaration_key qn) = toSymbol qn

instance Symbol Hack.QName where
  toSymbol k = Glean.keyOf k >>= toSymbol

instance Symbol Hack.QName_key where
  toSymbol (Hack.QName_key name Nothing) = toSymbol name
  toSymbol (Hack.QName_key name (Just ns)) = ns <:> name

instance Symbol Hack.Name where
  toSymbol k = do
    v <- Glean.keyOf k
    return [v]

--
-- And searching for Hack Entities
--

instance ToQName Hack.Declaration where
  toQName e = case e of
    Hack.Declaration_classConst x -> Glean.keyOf x >>= toQName
    Hack.Declaration_container x -> toQName x
    Hack.Declaration_enumerator x -> Glean.keyOf x >>= toQName
    Hack.Declaration_function_ x -> Glean.keyOf x >>= toQName
    Hack.Declaration_globalConst x -> Glean.keyOf x >>= toQName
    Hack.Declaration_method x -> Glean.keyOf x >>= toQName
    Hack.Declaration_module x -> Glean.keyOf x >>= toQName
    Hack.Declaration_namespace_ x -> Glean.keyOf x >>= toQName
    Hack.Declaration_property_ x -> Glean.keyOf x >>= toQName
    Hack.Declaration_typeConst x -> Glean.keyOf x >>= toQName
    Hack.Declaration_typedef_ x -> Glean.keyOf x >>= toQName
    Hack.Declaration_EMPTY -> return $ Left "unknown Declaration"

instance ToQName Hack.ClassConstDeclaration_key where
  toQName (Hack.ClassConstDeclaration_key name con) = pairToQName name con

instance ToQName Hack.ModuleDeclaration_key where
  toQName (Hack.ModuleDeclaration_key name) =
      Right . (, Name "") . Name . intercalate "/" <$> toSymbol name

instance ToQName Hack.ContainerDeclaration where
  toQName (Hack.ContainerDeclaration_class_ x) = Glean.keyOf x >>= toQName
  toQName (Hack.ContainerDeclaration_enum_ x) = Glean.keyOf x >>= toQName
  toQName (Hack.ContainerDeclaration_interface_ x) = Glean.keyOf x >>= toQName
  toQName (Hack.ContainerDeclaration_trait x) = Glean.keyOf x >>= toQName
  toQName Hack.ContainerDeclaration_EMPTY =
    return $ Left "unknown ContainerDeclaration"

instance ToQName Hack.EnumDeclaration_key where
  toQName (Hack.EnumDeclaration_key qn) = toQName qn

instance ToQName Hack.Enumerator_key where
  toQName (Hack.Enumerator_key name con) = pairToQName name con

instance ToQName Hack.TypeConstDeclaration_key where
  toQName (Hack.TypeConstDeclaration_key name con) = pairToQName name con

instance ToQName Hack.GlobalConstDeclaration_key where
  toQName (Hack.GlobalConstDeclaration_key qn) = toQName qn

instance ToQName Hack.PropertyDeclaration_key where
  toQName (Hack.PropertyDeclaration_key name con) = pairToQName name con

instance ToQName Hack.MethodDeclaration_key where
  toQName (Hack.MethodDeclaration_key name con) = pairToQName name con

instance ToQName Hack.FunctionDeclaration_key where
  toQName (Hack.FunctionDeclaration_key qname) = toQName qname

instance ToQName Hack.ClassDeclaration_key where
  toQName (Hack.ClassDeclaration_key qn) = toQName qn

instance ToQName Hack.InterfaceDeclaration_key where
  toQName (Hack.InterfaceDeclaration_key qn) = toQName qn

instance ToQName Hack.TraitDeclaration_key where
  toQName (Hack.TraitDeclaration_key qn) = toQName qn

instance ToQName Hack.TypedefDeclaration_key where
  toQName (Hack.TypedefDeclaration_key qn) = toQName qn

instance ToQName Hack.NamespaceDeclaration_key where
  toQName (Hack.NamespaceDeclaration_key qn) = Glean.keyOf qn >>= toQName

instance ToQName Hack.NamespaceQName_key where
  toQName (Hack.NamespaceQName_key name parent) = pairToQName name parent

instance ToQName Hack.QName where
  toQName x = Glean.keyOf x >>= toQName

instance ToQName Hack.QName_key where
  toQName (Hack.QName_key name (Just ns)) = pairToQName name ns
  toQName (Hack.QName_key name Nothing) =
    Right . (, Name "") . Name . intercalate "/" <$> toSymbol name

pairToQName
  :: (Symbol name, Symbol container)
  => name
  -> container
  -> Glean.RepoHaxl u w (Either a (Name, Name))
pairToQName a b = Right <$> symbolPairToQName "/" a b
