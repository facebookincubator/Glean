{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}

module Glean.Glass.Pretty.Hack
  ( prettyHackSignature
    -- for testing
  , prettyDecl
  , Decl(..)
  , Name(..)
  , Qual(..)
  , QualName(..)
  , FunctionMod(..)
  , ClassMod(..)
  , MethodMod(..)
  , Abstract(..)
  , Final(..)
  , Visibility(..)
  , Static(..)
  , Async(..)
  , Signature (..)
  , HackType(..)
  , ReturnType(..)
  , DefaultValue(..)
  , Inout(..)
  , Parameter(..)
  ) where

import qualified Glean
import Glean.Angle as Angle
import qualified Glean.Haxl.Repos as Glean
import qualified Glean.Schema.Hack.Types as Hack
import Glean.Schema.CodeHack.Types as Hack ( Entity(..) )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Trans.Writer.Strict

prettyHackSignature :: Hack.Entity -> Glean.RepoHaxl u w (Maybe Text)
prettyHackSignature (Hack.Entity_decl d) = runMaybeT $ prettyDecl <$> decl d
prettyHackSignature Hack.Entity_EMPTY = return Nothing

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

-- Hack modifier orderings are not as strict as this but since we want
-- to print them in some consistent ordering we enforce the most popular
newtype FunctionMod = FunctionMod Async
data ClassMod    = ClassMod Abstract Final
data MethodMod   = MethodMod Abstract Final Visibility Static Async

data Abstract = Abstract | NotAbstract deriving (Eq)
data Final = Final | NotFinal deriving (Eq)
data Visibility = Public | Protected | Private | Internal deriving (Eq)
data Static = Static | NotStatic deriving (Eq)
data Async = Async | NotAsync deriving (Eq)

newtype HackType = HackType { unHackType :: Text }
newtype ReturnType = ReturnType { unReturnType :: Text }
newtype DefaultValue = DefaultValue Text
data Inout = Inout
data Parameter = Parameter Name HackType (Maybe Inout) (Maybe DefaultValue)
data Signature = Signature ReturnType [Parameter]

data Decl
  = ClassConst Name
  | Enum QualName
  | Trait QualName
  | Class ClassMod QualName
  | Interface QualName
  | Enumerator QualName Name
  | Function FunctionMod QualName Signature
  | GlobalConst QualName
  | Namespace Qual
  | Method MethodMod QualName Name Signature
  | Property Name
  | TypeConst Name
  | Typedef QualName

prettyDecl :: Decl -> Text
prettyDecl (ClassConst name) =
  "const" <+> ppName name
prettyDecl (Enum name) =
  "enum" <+> ppQualName name
prettyDecl (Trait name) =
  "trait" <+> ppQualName name
prettyDecl (Class modifiers name) =
  ppClassModifiers modifiers  <+> "class" <+> ppQualName name
prettyDecl (Interface name) =
  "interface" <+> ppQualName name
prettyDecl (Enumerator enum name) =
  ppQualName enum <::> ppName name
prettyDecl (Function modifiers name signature) =
  ppFunctionModifiers modifiers <+> "function" <+> ppQualName name <+>
  ppSignature signature
prettyDecl (GlobalConst name) =
  "const" <+> ppQualName name
prettyDecl (Namespace name) =
  "namespace" <+> ppQual name
prettyDecl (Method modifiers container name signature) =
  ppMethodModifiers modifiers <+> ppQualName container <::> ppName name <+>
  ppSignature signature
prettyDecl (Property name) =
  ppName name
prettyDecl (TypeConst name) =
  "const" <+> ppName name
prettyDecl (Typedef name) =
  "type" <+> ppQualName name

ppName :: Name -> Text
ppName (Name n) = n
ppQualName :: QualName -> Text
ppQualName (QualName (namespace, name)) = ppQual (Qual namespace) <\> name
ppQual :: Qual -> Text
ppQual (Qual namespace) = Text.intercalate "\\" namespace

ppFunctionModifiers :: FunctionMod -> Text
ppFunctionModifiers (FunctionMod async) =
  Text.unwords $ execWriter $ do
    when (async==Async) $ tell ["async"]

ppClassModifiers :: ClassMod -> Text
ppClassModifiers (ClassMod abstract final) =
  Text.unwords $ execWriter $ do
      when (abstract==Abstract) $ tell ["abstract"]
      when (final==Final) $ tell ["final"]

ppSignature :: Signature -> Text
ppSignature (Signature returnType params) =
  "(" <>  Text.intercalate ", " (map ppParameter params) <> "):"
   <+> ppReturnType returnType

ppType :: HackType -> Text
ppType (HackType t) = t

ppReturnType :: ReturnType -> Text
ppReturnType (ReturnType t) = ppType $ HackType t

ppParameter :: Parameter -> Text
ppParameter (Parameter name typeName inout defaultValue) =
  ppInout inout <+> ppType typeName <+> ppName name <+>
  ppDefaultValue defaultValue

ppDefaultValue :: Maybe DefaultValue -> Text
ppDefaultValue Nothing = ""
ppDefaultValue (Just (DefaultValue defaultValue)) = "= '" <> defaultValue <> "'"

ppInout :: Maybe Inout -> Text
ppInout Nothing = ""
ppInout (Just Inout) = "inout"

ppMethodModifiers :: MethodMod -> Text
ppMethodModifiers (MethodMod abstract final visibility static async) =
  Text.unwords $ execWriter $ do
    when (abstract==Abstract) $ tell ["abstract"]
    when (final==Final) $ tell ["final"]
    when (visibility==Public) $ tell ["public"]
    when (visibility==Protected) $ tell ["protected"]
    when (visibility==Private) $ tell ["private"]
    when (visibility==Internal) $ tell ["internal"]
    when (static==Static) $ tell ["static"]
    when (async==Async) $ tell ["async"]

decl :: Hack.Declaration -> Glean.MaybeTRepoHaxl u w Decl
decl (Hack.Declaration_classConst Hack.ClassConstDeclaration{..}) = do
  Hack.ClassConstDeclaration_key{..} <- liftMaybe classConstDeclaration_key
  name <- liftMaybe $ Hack.name_key classConstDeclaration_key_name
  pure $ ClassConst $ Name name
decl (Hack.Declaration_container container) = containerDecl container
decl (Hack.Declaration_enumerator Hack.Enumerator{..}) = do
  Hack.Enumerator_key{..} <- liftMaybe $enumerator_key
  Hack.EnumDeclaration{..} <- pure enumerator_key_enumeration
  Hack.EnumDeclaration_key{..} <- liftMaybe $enumDeclaration_key
  enum <- liftMaybe $ qName enumDeclaration_key_name
  name <- liftMaybe $ Hack.name_key enumerator_key_name
  pure $ Enumerator (QualName enum) $ Name name
decl (Hack.Declaration_function_ fun@Hack.FunctionDeclaration{..}) = do
  Hack.FunctionDeclaration_key{..} <- liftMaybe functionDeclaration_key
  Hack.FunctionDefinition{..} <- maybeT $
    Glean.getFirstResult $
    Glean.recursive $
    query @Hack.FunctionDefinition $
    predicate @Hack.FunctionDefinition $
      rec $
        field @"declaration"
          (Angle.asPredicate $ Angle.factId $ Glean.getId fun)
      end
  name <- liftMaybe $ qName functionDeclaration_key_name
  def <- liftMaybe functionDefinition_key
  let sign = Hack.functionDefinition_key_signature def
  pure $ Function (modifiersForFunction def) (QualName name)
    (toSignature sign)
decl (Hack.Declaration_globalConst Hack.GlobalConstDeclaration{..}) = do
  Hack.GlobalConstDeclaration_key{..} <- liftMaybe globalConstDeclaration_key
  name <- liftMaybe $ qName globalConstDeclaration_key_name
  pure $ GlobalConst $ QualName name
decl (Hack.Declaration_namespace_ Hack.NamespaceDeclaration{..}) = do
  Hack.NamespaceDeclaration_key{..} <- liftMaybe namespaceDeclaration_key
  name <- liftMaybe $ namespaceQName $ Just namespaceDeclaration_key_name
  pure $ Namespace $ Qual name
decl (Hack.Declaration_method meth@Hack.MethodDeclaration{..}) = do
  Hack.MethodDeclaration_key{..} <- liftMaybe methodDeclaration_key
  Hack.MethodDefinition{..} <- maybeT $
    Glean.getFirstResult $
    Glean.recursive $
    query @Hack.MethodDefinition $
    predicate @Hack.MethodDefinition $
      rec $
        field @"declaration"
          (Angle.asPredicate $ Angle.factId $ Glean.getId meth)
      end
  name <- liftMaybe $ Hack.name_key methodDeclaration_key_name
  def <- liftMaybe methodDefinition_key
  let sign = Hack.methodDefinition_key_signature def
  container <- liftMaybe $ containerQualName methodDeclaration_key_container
  pure $ Method (modifiersForMethod def) container (Name name)
    (toSignature sign)
decl (Hack.Declaration_property_ Hack.PropertyDeclaration{..}) = do
  Hack.PropertyDeclaration_key{..} <- liftMaybe propertyDeclaration_key
  name <- liftMaybe $ Hack.name_key propertyDeclaration_key_name
  pure $ Property $ Name name
decl (Hack.Declaration_typeConst Hack.TypeConstDeclaration{..}) = do
  Hack.TypeConstDeclaration_key{..} <- liftMaybe typeConstDeclaration_key
  name <- liftMaybe $ Hack.name_key typeConstDeclaration_key_name
  pure $ TypeConst $ Name name
decl (Hack.Declaration_typedef_ Hack.TypedefDeclaration{..}) = do
  Hack.TypedefDeclaration_key{..} <- liftMaybe typedefDeclaration_key
  name <- liftMaybe $ qName typedefDeclaration_key_name
  pure $ Typedef $ QualName name
decl Hack.Declaration_EMPTY = MaybeT (return Nothing)

containerDecl :: Hack.ContainerDeclaration -> Glean.MaybeTRepoHaxl u w Decl
containerDecl
  (Hack.ContainerDeclaration_enum_ Hack.EnumDeclaration{..}) = do
    Hack.EnumDeclaration_key{..} <- liftMaybe enumDeclaration_key
    name <- liftMaybe $ qName enumDeclaration_key_name
    pure $ Enum $ QualName name
containerDecl
  (Hack.ContainerDeclaration_trait Hack.TraitDeclaration{..}) = do
    Hack.TraitDeclaration_key{..} <- liftMaybe traitDeclaration_key
    name <- liftMaybe $ qName traitDeclaration_key_name
    pure $ Trait $ QualName name
containerDecl
  (Hack.ContainerDeclaration_class_ clas@Hack.ClassDeclaration{..}) = do
    Hack.ClassDeclaration_key{..} <- liftMaybe classDeclaration_key
    Hack.ClassDefinition{..} <- maybeT $
      Glean.getFirstResult $
      query @Hack.ClassDefinition $
      predicate @Hack.ClassDefinition $
        rec $
          field @"declaration"
            (Angle.asPredicate $ Angle.factId $ Glean.getId clas)
        end
    def <- liftMaybe classDefinition_key
    name <- liftMaybe $ qName classDeclaration_key_name
    pure $ Class (modifiersForClass def) $ QualName name
containerDecl
  (Hack.ContainerDeclaration_interface_ Hack.InterfaceDeclaration{..}) = do
    Hack.InterfaceDeclaration_key{..} <- liftMaybe interfaceDeclaration_key
    name <- liftMaybe $ qName interfaceDeclaration_key_name
    pure $ Interface $ QualName name
containerDecl Hack.ContainerDeclaration_EMPTY = MaybeT (return Nothing)

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
containerQualName Hack.ContainerDeclaration_EMPTY = Nothing

qName :: Hack.QName -> Maybe ([Text], Text)
qName Hack.QName{..} = do
 Hack.QName_key{..} <- qName_key
 (,) (fromMaybe [] $ namespaceQName qName_key_namespace_) <$>
  Hack.name_key qName_key_name

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

modifiersForFunction :: Hack.FunctionDefinition_key -> FunctionMod
modifiersForFunction Hack.FunctionDefinition_key {..} =
  FunctionMod
    (if functionDefinition_key_isAsync then Async else NotAsync)
modifiersForClass :: Hack.ClassDefinition_key -> ClassMod
modifiersForClass Hack.ClassDefinition_key {..} =
  ClassMod
    (if classDefinition_key_isAbstract then Abstract else NotAbstract)
    (if classDefinition_key_isFinal then Final else NotFinal)

modifiersForMethod :: Hack.MethodDefinition_key -> MethodMod
modifiersForMethod Hack.MethodDefinition_key {..} =
  MethodMod
  (if methodDefinition_key_isAbstract then Abstract else NotAbstract)
  (if methodDefinition_key_isFinal then Final else NotFinal)
  (case methodDefinition_key_visibility of
    Hack.Visibility_Public -> Public
    Hack.Visibility_Protected -> Protected
    Hack.Visibility_Private -> Private
    Hack.Visibility_Internal -> Internal
    Hack.Visibility__UNKNOWN{} -> error "unexpected visibility"
  )
  (if methodDefinition_key_isStatic then Static else NotStatic)
  (if methodDefinition_key_isAsync then Async else NotAsync)

toSignature :: Hack.Signature -> Signature
toSignature Hack.Signature {..} =
  Signature
  (ReturnType $ case signature_key of
    Nothing -> unknownType
    Just (Hack.Signature_key mtype _ _) -> unHackType $ toType mtype
  )
  (case signature_key of
    Nothing -> []
    Just (Hack.Signature_key _ params _) -> map toParameter params
  )

toType :: Maybe Hack.Type -> HackType
toType Nothing = HackType unknownType
toType (Just (Hack.Type _ mkey)) = HackType $ fromMaybe unknownType mkey

toParameter :: Hack.Parameter -> Parameter
toParameter (Hack.Parameter name mtype inout _ mdefaultValue _) =
  Parameter
  (toName name)
  (toType mtype)
  (if inout then Just Inout else Nothing)
  (DefaultValue <$> mdefaultValue)

toName :: Hack.Name -> Name
toName (Hack.Name _ mkey) = Name $ fromMaybe "(anonymous)" mkey

unknownType :: Text
unknownType = "<unknown-type>"

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

maybeT :: (MonadTrans t, Monad m, MonadPlus (t m)) => m (Maybe b) -> t m b
maybeT act = lift act >>= liftMaybe
