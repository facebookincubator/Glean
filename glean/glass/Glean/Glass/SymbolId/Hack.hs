{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.SymbolId.Hack (
    {- instances and -} hackGlobalNamespaceAliases
  ) where

import Control.Monad (forM)
import qualified Data.Map.Strict as Map
import Data.Text (Text, intercalate)
import Data.Hashable

import qualified Glean

import Glean.Angle
import Glean.Glass.SymbolId.Class
import Glean.Glass.Types (Name(..))
import Glean.Haxl.Repos (RepoHaxl)
import qualified Glean.Glass.Utils as Utils
import qualified Glean.Schema.Hack.Types as Hack
import qualified Haxl.Core.Memo as Haxl

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
  toSymbol (Hack.PropertyDeclaration_key name container) = do
    xs <- toSymbol container
    x <- (":prop" :) <$> toSymbol name
    return $ xs ++ x

instance Symbol Hack.ClassConstDeclaration_key where
  toSymbol (Hack.ClassConstDeclaration_key name container) = container <:> name

instance Symbol Hack.MethodDeclaration_key where
  toSymbol (Hack.MethodDeclaration_key name container) = container <:> name

instance Symbol Hack.ModuleDeclaration_key where
  toSymbol (Hack.ModuleDeclaration_key name) =  ("module" :) <$> toSymbol name

instance Symbol Hack.EnumDeclaration_key where
  toSymbol (Hack.EnumDeclaration_key qn) = toSymbol qn

instance Symbol Hack.EnumDeclaration where
  toSymbol k = Glean.keyOf k >>= toSymbol

instance Symbol Hack.Enumerator_key where
  toSymbol (Hack.Enumerator_key name decl) = decl <:> name

instance Symbol Hack.FunctionDeclaration_key where
  toSymbol (Hack.FunctionDeclaration_key qn) =  ("fun" :) <$> toSymbol qn

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
  toSymbol (Hack.GlobalConstDeclaration_key qn) = ("const" :) <$> toSymbol qn

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
  toQName (Hack.ClassConstDeclaration_key name container) =
    Right <$> symbolPairToQName "\\" name container

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
  toQName (Hack.Enumerator_key name container) =
    Right <$> symbolPairToQName "\\" name container

instance ToQName Hack.TypeConstDeclaration_key where
  toQName (Hack.TypeConstDeclaration_key name container) =
    Right <$> symbolPairToQName "\\" name container

instance ToQName Hack.GlobalConstDeclaration_key where
  toQName (Hack.GlobalConstDeclaration_key qn) = toQName qn

instance ToQName Hack.PropertyDeclaration_key where
  toQName (Hack.PropertyDeclaration_key name container) =
    Right <$> symbolPairToQName "\\" name container

instance ToQName Hack.MethodDeclaration_key where
  toQName (Hack.MethodDeclaration_key name container) =
    Right <$> symbolPairToQName "\\" name container

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
  toQName (Hack.NamespaceDeclaration_key qn) = toQName qn

instance ToQName Hack.NamespaceQName where
  toQName nsqname@Hack.NamespaceQName{} = do
    mShort <- Map.lookup (Glean.getId nsqname) <$> hackGlobalNamespaceAliases
    case mShort of -- exact key is an alias (e.g. FlibSL\Vec)
      Just name -> return $ Right (Name name, Name "")
      Nothing -> Glean.keyOf nsqname >>= toQName -- break it apart and try again

instance ToQName Hack.NamespaceQName_key where
  toQName (Hack.NamespaceQName_key name Nothing) = do
    nameStr <- Name <$> Glean.keyOf name
    return $ Right (nameStr, Name "")
  toQName (Hack.NamespaceQName_key name (Just parent)) = do
    nameStr <- Name <$> Glean.keyOf name
    mParent <- toQName parent -- might need to shorten
    return $ case mParent of
      Right (parentName, Name "") -> Right (nameStr, parentName)
      Right (Name p, Name ps) -> Right (nameStr, Name (ps <> "\\" <> p))
      Left e -> Left e

instance ToQName Hack.QName where
  toQName x = Glean.keyOf x >>= toQName

instance ToQName Hack.QName_key where
  toQName (Hack.QName_key name Nothing) = do
    nameStr <- Name <$> Glean.keyOf name
    return $ Right (nameStr, Name "")
  toQName (Hack.QName_key name (Just nsqname)) = do
    nameStr <- Name <$> Glean.keyOf name
    mParent <- toQName nsqname
    return $ case mParent of
      Right (parentName, Name "") -> Right (nameStr, parentName)
      Right (Name p, Name ps) -> Right (nameStr, Name (ps <> "\\" <> p))
      Left e -> Left e

newtype Glass_SymbolId_Hack_ShortNamespace_Key =
    Glass_SymbolId_Hack_ShortNamespace_Key (Glean.Repo, Text)
  deriving (Eq, Hashable, Ord, Show)

-- Memoize implementation of short namespace lookup
--
hackGlobalNamespaceAliases
  :: RepoHaxl u w (Map.Map (Glean.IdOf Hack.NamespaceQName) Text)
hackGlobalNamespaceAliases = do
  repo <- Glean.haxlRepo -- use repo as memo hash as we store Ids
  Haxl.memo (Glass_SymbolId_Hack_ShortNamespace_Key
      (repo, "Glean.Glass.SymbolId.Hack"::Text)) $ do
    (pairs,_lim) <- Utils.searchRecursiveWithLimit (Just maxMappings) query
    Map.fromList <$> forM pairs (\alias -> do
        Hack.GlobalNamespaceAlias_key{..} <- Glean.keyOf alias
        let nsQName = Glean.getId globalNamespaceAlias_key_to
        shortName <- Glean.keyOf globalNamespaceAlias_key_from
        return (nsQName, shortName)
      )
  where
    query = predicate @Hack.GlobalNamespaceAlias wild
    maxMappings = 100 -- there's only about 15 actually
