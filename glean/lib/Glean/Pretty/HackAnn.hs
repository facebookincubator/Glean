{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Pretty.HackAnn
  ( -- * Entity util
    entityFromHackDeclaration
  , entityFromHackDecl
    -- * Pretty
  , prettyShortDeclaration
  , prettyScopedDeclaration
  ) where

import Data.Maybe (fromMaybe)
import Data.Text.Prettyprint.Doc

import Glean.Pretty.Shared
import Glean.Pretty.Styles
import Glean.Schema.Hack.Types as Hack
import Glean.Typed (Predicate(getFactKey, KeyType))
import Glean.Util.AnnMaker

import Glean.Pretty.Style (Style)

-- | There are two shapes of names, 'InQName' is for things in containers and
-- for enumerators. 'InNamespace' is for top-level things.
-- 'IsNamespace' is for actual namepsaces.
data HackNaming
  = IsNamespace Hack.NamespaceQName
  | InNamespace Style Hack.QName
  | InQName Style Hack.Name Style Hack.QName

-- | Helper to extract the name
hackNaming :: Hack.Declaration -> Maybe HackNaming
hackNaming = \case
  Hack.Declaration_classConst x -> inQ name_constant
    Hack.classConstDeclaration_key_name
    Hack.classConstDeclaration_key_container
    x
  Hack.Declaration_container x ->
    InNamespace name_class <$> containerQName x
  Hack.Declaration_enumerator x -> do
    ek <- getFactKey x
    edk <- getFactKey (Hack.enumerator_key_enumeration ek)
    pure (InQName name_constant (Hack.enumerator_key_name ek)
      name_class (Hack.enumDeclaration_key_name edk))
  Hack.Declaration_function_ x -> InNamespace name_function
    . Hack.functionDeclaration_key_name <$> getFactKey x
  Hack.Declaration_globalConst x -> InNamespace name_constant
    . Hack.globalConstDeclaration_key_name <$> getFactKey x
  Hack.Declaration_namespace_ x -> do
    nqn <- Hack.namespaceDeclaration_key_name <$> getFactKey x
    pure $ IsNamespace nqn
  Hack.Declaration_method x -> inQ name_function
    Hack.methodDeclaration_key_name Hack.methodDeclaration_key_container x
  Hack.Declaration_property_ x -> inQ name_variable_instance
    Hack.propertyDeclaration_key_name Hack.propertyDeclaration_key_container x
  Hack.Declaration_typeConst x -> inQ name_type
    Hack.typeConstDeclaration_key_name Hack.typeConstDeclaration_key_container x
  Hack.Declaration_typedef_ x -> InNamespace name_type
    . Hack.typedefDeclaration_key_name <$> getFactKey x
  where
    inQ
      :: Predicate p
      => Style
      -> (KeyType p -> Hack.Name)
      -> (KeyType p -> Hack.ContainerDeclaration)
      -> p
      -> Maybe HackNaming
    inQ s1 nameOf cdOf p = do
      k <- getFactKey p
      qn <- containerQName (cdOf k)
      pure (InQName s1 (nameOf k) name_class qn)

    containerQName :: Hack.ContainerDeclaration -> Maybe Hack.QName
    containerQName = \case
      Hack.ContainerDeclaration_class_ y ->
        Hack.classDeclaration_key_name <$> getFactKey y
      Hack.ContainerDeclaration_enum_ y ->
        Hack.enumDeclaration_key_name <$> getFactKey y
      Hack.ContainerDeclaration_interface_ y ->
        Hack.interfaceDeclaration_key_name <$> getFactKey y
      Hack.ContainerDeclaration_trait y ->
        Hack.traitDeclaration_key_name <$> getFactKey y

-- -----------------------------------------------------------------------------

prettyName :: Style -> Hack.Name -> Doc (Ann r)
prettyName style n = fromMaybe mempty $ do
  name <- getFactKey n
  pure (txt style name)

-- | Connect namespaces to names with backslash
withNamespace :: Maybe Hack.NamespaceQName -> Doc (Ann r) -> Doc (Ann r)
withNamespace m here = case prettyScopedNamespaceQName <$> m of
    Nothing -> here
    Just parent -> parent <//> txt operator "\\" <//> here

prettyScopedNamespaceQName :: Hack.NamespaceQName -> Doc (Ann r)
prettyScopedNamespaceQName nqn = fromMaybe mempty $ do
  Hack.NamespaceQName_key{..} <- getFactKey nqn
  let here = prettyName name_namespace namespaceQName_key_name
  pure $ withNamespace namespaceQName_key_parent here

prettyShortQName :: Style -> Hack.QName -> Doc (Ann r)
prettyShortQName style qn = fromMaybe mempty $ do
  Hack.QName_key{..} <- getFactKey qn
  pure $ prettyName style qName_key_name

prettyScopedQName :: Style -> Hack.QName -> Doc (Ann r)
prettyScopedQName style qn = fromMaybe mempty $ do
  Hack.QName_key{..} <- getFactKey qn
  pure $ withNamespace qName_key_namespace_ $ prettyShortQName style qn

-- | Connect (container or enum) to name with colons
prettyScopedInQName :: Style -> Hack.Name -> Style -> Hack.QName -> Doc (Ann r)
prettyScopedInQName s1 n s2 qn =
  let here = prettyName s1 n
      there = prettyScopedQName s2 qn
  in there <//> txt operator "::" <//> here

-- | Name without container or namespace or enum
prettyShortDeclaration :: Hack.Declaration -> Doc (Ann r)
prettyShortDeclaration d = case hackNaming d of
  Just (IsNamespace nqn) -> fromMaybe mempty $ do
    Hack.NamespaceQName_key{..} <- getFactKey nqn
    pure $ prettyName name_namespace namespaceQName_key_name
  Just (InNamespace s1 qn) -> prettyShortQName s1 qn
  Just (InQName s1 n _s2 _qn) -> prettyName s1 n
  Nothing -> mempty

-- | Name with container or namespace or enum
prettyScopedDeclaration :: Hack.Declaration -> Doc (Ann r)
prettyScopedDeclaration d = case hackNaming d of
  Just (IsNamespace nqn) -> prettyScopedNamespaceQName nqn
  Just (InNamespace s1 qn) -> prettyScopedQName s1 qn
  Just (InQName s1 n s2 qn) -> prettyScopedInQName s1 n s2 qn
  Nothing -> mempty
