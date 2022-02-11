{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module  Glean.Glass.Pretty.Hack
  ( prettyHackSignature
    -- for testing
  , prettyDecl
  , Decl(..)
  , Name(..)
  , Qual(..)
  , QualName(..)
  ) where

import qualified Glean.Schema.Hack.Types as Hack
import Glean.Schema.CodeHack.Types as Hack ( Entity(..) )
import Data.Text (Text)
import qualified Data.Text as Text

prettyHackSignature :: Hack.Entity -> Maybe Text
prettyHackSignature (Hack.Entity_decl d) = prettyDecl <$> decl d

(<+>) :: Text -> Text -> Text
"" <+> b = b
a <+> "" = a
a <+> b = a <> " " <> b

(<::>) :: Text -> Text -> Text
"" <::> b = b
a <::> "" = a
a <::> b = a <> "::" <> b

(<\>) :: Text -> Text -> Text
"" <\> b = b
a <\> "" = a
a <\> b = a <> "\\" <> b

newtype Name = Name Text
newtype Qual = Qual [Text]
newtype QualName = QualName ([Text], Text)
data Decl
  = ClassConst Name
  | Enum QualName
  | Trait QualName
  | Class QualName
  | Interface QualName
  | Enumerator QualName Name
  | Function QualName
  | GlobalConst QualName
  | Namespace Qual
  | Method QualName Name
  | Property Name
  | TypeConst Name
  | Typedef QualName

prettyDecl :: Decl -> Text
prettyDecl (ClassConst name) = "const" <+> ppName name
prettyDecl (Enum name) = "enum" <+> ppQualName name
prettyDecl (Trait name) = "trait" <+> ppQualName name
prettyDecl (Class name) = "class" <+> ppQualName name
prettyDecl (Interface name) = "interface" <+> ppQualName name
prettyDecl (Enumerator enum name) = ppQualName enum <::> ppName name
prettyDecl (Function name) = "function" <+> ppQualName name
prettyDecl (GlobalConst name) = "const" <+> ppQualName name
prettyDecl (Namespace name) = "namespace" <+> ppQual name
prettyDecl (Method container name) = ppQualName container <::> ppName name
prettyDecl (Property name) = ppName name
prettyDecl (TypeConst name) = "const" <+> ppName name
prettyDecl (Typedef name) = "type" <+> ppQualName name

ppName :: Name -> Text
ppName (Name n) = n
ppQualName :: QualName -> Text
ppQualName (QualName (namespace, name)) = ppQual (Qual namespace) <\> name
ppQual :: Qual -> Text
ppQual (Qual namespace) = Text.intercalate "\\" namespace

decl :: Hack.Declaration -> Maybe Decl
decl (Hack.Declaration_classConst Hack.ClassConstDeclaration{..}) = do
  Hack.ClassConstDeclaration_key{..} <- classConstDeclaration_key
  name <- Hack.name_key classConstDeclaration_key_name
  pure $ ClassConst $ Name name
decl (Hack.Declaration_container container) = containerDecl container
decl (Hack.Declaration_enumerator Hack.Enumerator{..}) = do
  Hack.Enumerator_key{..} <- enumerator_key
  Hack.EnumDeclaration{..} <- pure enumerator_key_enumeration
  Hack.EnumDeclaration_key{..} <- enumDeclaration_key
  enum <- qName enumDeclaration_key_name
  name <- Hack.name_key enumerator_key_name
  pure $ Enumerator (QualName enum) $ Name name
decl (Hack.Declaration_function_ Hack.FunctionDeclaration{..}) = do
  Hack.FunctionDeclaration_key{..} <- functionDeclaration_key
  name <- qName functionDeclaration_key_name
  pure $ Function $ QualName name
decl (Hack.Declaration_globalConst Hack.GlobalConstDeclaration{..}) = do
  Hack.GlobalConstDeclaration_key{..} <- globalConstDeclaration_key
  name <- qName globalConstDeclaration_key_name
  pure $ GlobalConst $ QualName name
decl (Hack.Declaration_namespace_ Hack.NamespaceDeclaration{..}) = do
  Hack.NamespaceDeclaration_key{..} <- namespaceDeclaration_key
  name <- namespaceQName $ Just namespaceDeclaration_key_name
  pure $ Namespace $ Qual name
decl (Hack.Declaration_method Hack.MethodDeclaration{..}) = do
  Hack.MethodDeclaration_key{..} <- methodDeclaration_key
  name <- Hack.name_key methodDeclaration_key_name
  container <- containerQualName methodDeclaration_key_container
  pure $ Method container $ Name name
decl (Hack.Declaration_property_ Hack.PropertyDeclaration{..}) = do
  Hack.PropertyDeclaration_key{..} <- propertyDeclaration_key
  name <- Hack.name_key propertyDeclaration_key_name
  pure $ Property $ Name name
decl (Hack.Declaration_typeConst Hack.TypeConstDeclaration{..}) = do
  Hack.TypeConstDeclaration_key{..} <- typeConstDeclaration_key
  name <- Hack.name_key typeConstDeclaration_key_name
  pure $ TypeConst $ Name name
decl (Hack.Declaration_typedef_ Hack.TypedefDeclaration{..}) = do
  Hack.TypedefDeclaration_key{..} <- typedefDeclaration_key
  name <- qName typedefDeclaration_key_name
  pure $ Typedef $ QualName name

containerDecl :: Hack.ContainerDeclaration -> Maybe Decl
containerDecl
  (Hack.ContainerDeclaration_enum_ Hack.EnumDeclaration{..}) = do
    Hack.EnumDeclaration_key{..} <- enumDeclaration_key
    name <- qName enumDeclaration_key_name
    pure $ Enum $ QualName name
containerDecl
  (Hack.ContainerDeclaration_trait Hack.TraitDeclaration{..}) = do
    Hack.TraitDeclaration_key{..} <- traitDeclaration_key
    name <- qName traitDeclaration_key_name
    pure $ Trait $ QualName name
containerDecl
  (Hack.ContainerDeclaration_class_ Hack.ClassDeclaration{..}) = do
    Hack.ClassDeclaration_key{..} <- classDeclaration_key
    name <- qName classDeclaration_key_name
    pure $ Class $ QualName name
containerDecl
  (Hack.ContainerDeclaration_interface_ Hack.InterfaceDeclaration{..}) = do
    Hack.InterfaceDeclaration_key{..} <- interfaceDeclaration_key
    name <- qName interfaceDeclaration_key_name
    pure $ Interface $ QualName name

containerQualName :: Hack.ContainerDeclaration -> Maybe QualName
containerQualName
  (Hack.ContainerDeclaration_enum_ Hack.EnumDeclaration{..}) = do
    Hack.EnumDeclaration_key{..} <- enumDeclaration_key
    name <- qName enumDeclaration_key_name
    pure $ QualName name
containerQualName
  (Hack.ContainerDeclaration_trait Hack.TraitDeclaration{..}) = do
    Hack.TraitDeclaration_key{..} <- traitDeclaration_key
    name <- qName traitDeclaration_key_name
    pure $ QualName name
containerQualName
  (Hack.ContainerDeclaration_class_ Hack.ClassDeclaration{..}) = do
    Hack.ClassDeclaration_key{..} <- classDeclaration_key
    name <- qName classDeclaration_key_name
    pure $ QualName name
containerQualName
  (Hack.ContainerDeclaration_interface_ Hack.InterfaceDeclaration{..}) = do
    Hack.InterfaceDeclaration_key{..} <- interfaceDeclaration_key
    name <- qName interfaceDeclaration_key_name
    pure $ QualName name

qName :: Hack.QName -> Maybe ([Text], Text)
qName Hack.QName{..} = do
 Hack.QName_key{..} <- qName_key
 (,) <$> namespaceQName qName_key_namespace_ <*> Hack.name_key qName_key_name

namespaceQName :: Maybe Hack.NamespaceQName -> Maybe [Text]
namespaceQName Nothing = Nothing
namespaceQName (Just name) = reverse <$> namespaceQNameInner (Just name) []
  where
    namespaceQNameInner
      :: Maybe Hack.NamespaceQName -> [Text] -> Maybe [Text]
    namespaceQNameInner Nothing children = Just children
    namespaceQNameInner (Just Hack.NamespaceQName{..}) children = do
    Hack.NamespaceQName_key{..} <- namespaceQName_key
    parent <- Hack.name_key namespaceQName_key_name
    namespaceQNameInner namespaceQName_key_parent (parent:children)
