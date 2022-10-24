{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications, ApplicativeDo #-}

module Glean.Glass.Pretty.Hack
  ( prettyHackSignature
    -- for testing
  , prettyDecl
  , Decl(..)
  , Name(..)
  , Qual(..)
  , QualName(..)
  , FunctionMod(..)
  , PropertyMod(..)
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
  , Variance(..)
  , Reify(..)
  , TypeParameter(..)
  , Constraint(..)
  , ConstraintKind(..)
  , Container(..)
  , EnumKind(..)
  , EnumConstraint(..)
  , TypeConstKind(..)
  ) where

import qualified Glean
import Glean.Angle as Angle
import qualified Glean.Haxl.Repos as Glean
import qualified Glean.Schema.Hack.Types as Hack
import Glean.Schema.CodeHack.Types as Hack ( Entity(..) )
import Glean.Glass.Utils
import Data.Maybe (fromMaybe)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Trans.Writer.Strict

prettyHackSignature
  :: LayoutOptions
  -> Hack.Entity
  -> Glean.RepoHaxl u w (Maybe (SimpleDocStream ()))
prettyHackSignature opts (Hack.Entity_decl d) =
  runMaybeT $ layoutSmart opts . prettyDecl opts <$> decl d
prettyHackSignature _ Hack.Entity_EMPTY = return Nothing

newtype Name = Name Text
newtype Qual = Qual [Text]
newtype QualName = QualName ([Text], Text)

-- Hack modifier orderings are not as strict as this but since we want
-- to print them in some consistent ordering we enforce the most popular
newtype FunctionMod = FunctionMod Async
data ClassMod    = ClassMod Abstract Final
data MethodMod   = MethodMod Abstract Final Visibility Static Async
data PropertyMod   = PropertyMod Abstract Final Visibility Static

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
data Variance = Contravariant | Covariant | Invariant deriving (Eq)
data Reify = Erased | Reified | SoftReified deriving (Eq)
data ConstraintKind = As | Equal | Super deriving (Eq)
data Constraint = Constraint ConstraintKind HackType
data TypeParameter = TypeParameter Name Variance Reify [Constraint]
data Signature = Signature ReturnType [TypeParameter] [Parameter]
data Container
  = ClassContainer | InterfaceContainer | TraitContainer | EnumContainer
  deriving Eq
data EnumKind = IsClass | Regular
data EnumConstraint = EnumBase HackType | Constrained HackType HackType
data TypeConstKind = IsAbstract | Concrete | PartiallyAbstract

data Decl
  = ClassConst Name
  | Enum QualName EnumKind EnumConstraint
  | Trait QualName [TypeParameter]
  | Class ClassMod QualName [TypeParameter]
  | Interface QualName [TypeParameter]
  | Enumerator QualName Name
  | Function FunctionMod QualName Signature
  | GlobalConst QualName
  | Namespace Qual
  | Method MethodMod Container Name Signature
  | Property PropertyMod Container Name HackType
  | TypeConst Name TypeConstKind
  | Typedef QualName [TypeParameter]
  | Module Name

prettyDecl :: LayoutOptions -> Decl -> Doc ()
prettyDecl _ (ClassConst name) =
  "const" <+> ppName name
prettyDecl _ (Module name) =
  "module" <+> ppName name
prettyDecl _ (Enum name Regular (EnumBase ty1)) =
  "enum" <+> ppQualName name <+> ":" <+> ppType ty1
prettyDecl _ (Enum name Regular (Constrained ty1 ty2)) =
  "enum" <+> ppQualName name <+> ":" <+> ppConstraintTypes ty1 ty2
prettyDecl _ (Enum name IsClass _) =
  "enum" <+> "class" <+> ppQualName name
prettyDecl _ (Trait name typeParams) =
  "trait" <+> ppQualName name <> ppTypeParams typeParams
prettyDecl _ (Class modifiers name typeParams) =
  ppClassModifiers modifiers <+> ppQualName name <> ppTypeParams typeParams
prettyDecl _ (Interface name typeParams) =
  "interface" <+> ppQualName name <> ppTypeParams typeParams
prettyDecl _ (Enumerator enum name) =
  ppQualName enum <> "::" <> ppName name
prettyDecl opts (Function modifiers name sig) =
  ppFunctionModifiers modifiers <+> ppQualName name <>
  ppSignature opts sig
prettyDecl _ (GlobalConst name) =
  "const" <+> ppQualName name
prettyDecl _ (Namespace name) =
  "namespace" <+> ppQual name
prettyDecl opts (Method modifiers container name sig) =
  ppMethodModifiers container modifiers <+> ppName name <>
  ppSignature opts sig
prettyDecl _ (Property modifiers container name mhacktype) =
  ppPropertyModifiers container modifiers <+> ppType mhacktype <+> ppName name
prettyDecl _ (TypeConst name IsAbstract) =
  "abstract" <+> "const" <+> "type" <+> ppName name
prettyDecl _ (TypeConst name _) =
  "const" <+> "type" <+> ppName name
prettyDecl _ (Typedef name typeParams) =
  "type" <+> ppQualName name <> ppTypeParams typeParams

ppName :: Name -> Doc ()
ppName (Name n) = pretty n
ppQualName :: QualName -> Doc ()
ppQualName (QualName ([], name)) =
  pretty name
ppQualName (QualName (namespace, name)) =
  surround "\\" (ppQual (Qual namespace)) (pretty name)
ppQual :: Qual -> Doc ()
ppQual (Qual namespace) =
  concatWith (surround "\\") (pretty <$> namespace)

ppFunctionModifiers :: FunctionMod -> Doc ()
ppFunctionModifiers (FunctionMod async) =
  fillSep $ execWriter $ do
    when (async==Async) $ tell ["async"]
    tell ["function"]

ppClassModifiers :: ClassMod -> Doc ()
ppClassModifiers (ClassMod abstract final) =
  fillSep $ execWriter $ do
    when (abstract==Abstract) $ tell ["abstract"]
    when (final==Final) $ tell ["final"]
    tell ["class"]

ppSignature :: LayoutOptions -> Signature -> Doc ()
ppSignature opts (Signature returnType typeParams params) =
    if fitsOnOneLine then
      hcat
        [ onelineDoc
        , nest 4 (":" <+> ppReturnType returnType)
        ]
    else
      typeParamsDoc
      <> vcat
      [ nest 4 $ vcat
        [ "("
        , vcat $ map ((<> ",") . ppParameter) params
        ]
      , nest 4 $ "):" <+> ppReturnType returnType
      ]
  where
    typeParamsDoc = ppTypeParams typeParams
    onelineDoc = cat
      [ typeParamsDoc
      , parens (hsep $ punctuate comma (map ppParameter params))
      ]
    paramsText = renderStrict $ layoutSmart opts onelineDoc
    fitsOnOneLine = not containsNewline
    containsNewline = Text.any (== '\n') paramsText

ppTypeParams :: [TypeParameter] -> Doc ()
ppTypeParams typeParams | null typeParams = ""
ppTypeParams typeParams = cat
  [ nest 4 $ cat
    [ "<", sep $ punctuate "," (map ppTypeParam typeParams)]
  , ">"
  ]

ppTypeParam :: TypeParameter -> Doc ()
ppTypeParam (TypeParameter name variance reify constraints) =
  hcat $ execWriter $ do
    when (reify==SoftReified) $ tell ["<<__Soft>> reify "]
    when (reify==Reified) $ tell ["reify "]
    when (variance==Covariant) $ tell ["+"]
    when (variance==Contravariant) $ tell ["-"]
    tell [ppName name]
    forM_ constraints $ \(Constraint kind ty) -> do
      when (kind==Equal) $ tell [" = "]
      when (kind==As) $ tell [" as "]
      when (kind==Super) $ tell [" super "]
      tell [ppType ty]

ppType :: HackType -> Doc ()
ppType (HackType t) = pretty t

ppReturnType :: ReturnType -> Doc ()
ppReturnType (ReturnType t) = ppType $ HackType t

ppParameter :: Parameter -> Doc ()
ppParameter (Parameter name typeName inout defaultValue) =
  nest 4 $ sep $ execWriter $ do
    whenJust inout $ tell . ppInout
    tell [ppType typeName]
    tell [ppName name]
    whenJust defaultValue $ tell . ppDefaultValue

ppDefaultValue :: DefaultValue -> [Doc ()]
ppDefaultValue (DefaultValue defaultValue) =
  ["=" <+> squotes (pretty defaultValue)]

ppInout :: Inout -> [Doc ()]
ppInout Inout = ["inout"]

ppConstraintTypes :: HackType -> HackType -> Doc ()
ppConstraintTypes ty1 ty2 = ppType ty1 <+> "as" <+> ppType ty2

ppMethodModifiers :: Container -> MethodMod -> Doc ()
ppMethodModifiers container (MethodMod abstract final visibility static async) =
  fillSep $ execWriter $ do
    when
      (  abstract == Abstract
      && container /= InterfaceContainer
      ) $ tell ["abstract"]
    when (final==Final) $ tell ["final"]
    tell $ pure $ case visibility of
      Public -> "public"
      Protected -> "protected"
      Private -> "private"
      Internal -> "internal"
    when (static==Static) $ tell ["static"]
    when (async==Async) $ tell ["async"]
    tell ["function"]

ppPropertyModifiers :: Container -> PropertyMod -> Doc ()
ppPropertyModifiers container (PropertyMod abstract final visibility static) =
  fillSep $ execWriter $ do
    when
      (  abstract == Abstract
      && container /= InterfaceContainer
      ) $ tell ["abstract"]
    when (final==Final) $ tell ["final"]
    tell $ pure $ case visibility of
      Public -> "public"
      Protected -> "protected"
      Private -> "private"
      Internal -> "internal"
    when (static==Static) $ tell ["static"]

decl :: Hack.Declaration -> Glean.MaybeTRepoHaxl u w Decl
decl (Hack.Declaration_classConst Hack.ClassConstDeclaration{..}) = do
  Hack.ClassConstDeclaration_key{..} <- liftMaybe classConstDeclaration_key
  name <- liftMaybe $ Hack.name_key classConstDeclaration_key_name
  pure $ ClassConst $ Name name
decl (Hack.Declaration_container container) = containerDecl container
decl (Hack.Declaration_enumerator Hack.Enumerator{..}) = do
  Hack.Enumerator_key{..} <- liftMaybe enumerator_key
  Hack.EnumDeclaration{..} <- pure enumerator_key_enumeration
  Hack.EnumDeclaration_key{..} <- liftMaybe enumDeclaration_key
  enum <- qName enumDeclaration_key_name
  name <- liftMaybe $ Hack.name_key enumerator_key_name
  pure $ Enumerator (QualName enum) $ Name name
decl (Hack.Declaration_function_ decl@Hack.FunctionDeclaration{..}) = do
  Hack.FunctionDeclaration_key{..} <- liftMaybe functionDeclaration_key
  Hack.FunctionDefinition{..} <- maybeT $ fetchDataRecursive $
    angleFunctionDefinition (Angle.factId (Glean.getId decl))
  name <- qName functionDeclaration_key_name
  def <- liftMaybe functionDefinition_key
  let typeParams = Hack.functionDefinition_key_typeParams def
  let sign = Hack.functionDefinition_key_signature def
  pure $ Function (modifiersForFunction def) (QualName name)
    (toSignature typeParams sign)
decl (Hack.Declaration_module Hack.ModuleDeclaration{..}) = do
  Hack.ModuleDeclaration_key{..} <- liftMaybe moduleDeclaration_key
  name <- liftMaybe $ Hack.name_key moduleDeclaration_key_name
  pure $ Module $ Name name
decl (Hack.Declaration_globalConst Hack.GlobalConstDeclaration{..}) = do
  Hack.GlobalConstDeclaration_key{..} <- liftMaybe globalConstDeclaration_key
  name <- qName globalConstDeclaration_key_name
  pure $ GlobalConst $ QualName name
decl (Hack.Declaration_namespace_ Hack.NamespaceDeclaration{..}) = do
  Hack.NamespaceDeclaration_key{..} <- liftMaybe namespaceDeclaration_key
  name <- namespaceQName $ Just namespaceDeclaration_key_name
  pure $ Namespace $ Qual name
decl (Hack.Declaration_method decl@Hack.MethodDeclaration{..}) = do
  Hack.MethodDeclaration_key{..} <- liftMaybe methodDeclaration_key
  Hack.MethodDefinition{..} <- maybeT $ fetchDataRecursive $
    angleMethodDefinition (Angle.factId (Glean.getId decl))
  name <- liftMaybe $ Hack.name_key methodDeclaration_key_name
  def <- liftMaybe methodDefinition_key
  let typeParams = Hack.methodDefinition_key_typeParams def
  let sig = Hack.methodDefinition_key_signature def
  let container = containerKind methodDeclaration_key_container
  pure $ Method (modifiersForMethod def) container (Name name)
    (toSignature typeParams sig)
decl (Hack.Declaration_property_ prop@Hack.PropertyDeclaration{..}) = do
  Hack.PropertyDeclaration_key{..} <- liftMaybe propertyDeclaration_key
  Hack.PropertyDefinition{..} <- maybeT $ fetchDataRecursive $
    predicate @Hack.PropertyDefinition $
      rec $
        field @"declaration"
          (Angle.asPredicate $ Angle.factId $ Glean.getId prop)
      end
  def <- liftMaybe propertyDefinition_key
  let type_ = Hack.propertyDefinition_key_type def
  name <- liftMaybe $ Hack.name_key propertyDeclaration_key_name
  let container = containerKind propertyDeclaration_key_container
  pure $ Property (modifiersForProperty def) container (Name name)
   (toType type_)
decl (Hack.Declaration_typeConst decl@Hack.TypeConstDeclaration{..}) = do
  Hack.TypeConstDeclaration_key{..} <- liftMaybe typeConstDeclaration_key
  hackTypeConstKind <- maybeT $ fetchData $
    angleTypeConstDefinition (Angle.factId (Glean.getId decl))
  let typeConstKind = case hackTypeConstKind of
        Hack.TypeConstKind_Abstract -> IsAbstract
        Hack.TypeConstKind_Concrete -> Concrete
        Hack.TypeConstKind_PartiallyAbstract -> PartiallyAbstract
        Hack.TypeConstKind__UNKNOWN{} -> error "unexpected typeconst kind"
  name <- liftMaybe $ Hack.name_key typeConstDeclaration_key_name
  pure $ TypeConst (Name name) typeConstKind
decl (Hack.Declaration_typedef_ decl@Hack.TypedefDeclaration{..}) = do
  Hack.TypedefDeclaration_key{..} <- liftMaybe typedefDeclaration_key
  typedefTypeParams <- maybeT $ fetchDataRecursive $
    angleTypedefDefinition (Angle.factId (Glean.getId decl))
  name <- qName typedefDeclaration_key_name
  let typeParams = map toTypeParameter typedefTypeParams
  pure $ Typedef (QualName name) typeParams
decl Hack.Declaration_EMPTY = MaybeT (return Nothing)

containerDecl :: Hack.ContainerDeclaration -> Glean.MaybeTRepoHaxl u w Decl
containerDecl (Hack.ContainerDeclaration_enum_
      decl@Hack.EnumDeclaration{..}) = do
    Hack.EnumDeclaration_key{..} <- liftMaybe enumDeclaration_key
    (enumBase,enumConstraint,isClass) <- maybeT $ fetchDataRecursive $
      angleEnumDefinition (Angle.factId (Glean.getId decl))
    let enumKind = if isClass then IsClass else Regular
    let constraint =  case enumConstraint of
          Nothing -> EnumBase (toType1 enumBase)
          Just ty -> Constrained (toType1 enumBase) (toType1 ty)
    name <- qName enumDeclaration_key_name
    pure $ Enum (QualName name) enumKind constraint
containerDecl
  (Hack.ContainerDeclaration_trait decl@Hack.TraitDeclaration{..}) = do
    Hack.TraitDeclaration_key{..} <- liftMaybe traitDeclaration_key
    traitTypeParams <- maybeT $ fetchDataRecursive $
      angleTraitDefinition (Angle.factId (Glean.getId decl))
    name <- qName traitDeclaration_key_name
    let typeParams = map toTypeParameter traitTypeParams
    pure $ Trait (QualName name) typeParams
containerDecl
  (Hack.ContainerDeclaration_class_ decl@Hack.ClassDeclaration{..}) = do
    Hack.ClassDeclaration_key{..} <- liftMaybe classDeclaration_key
    (isAbstract, isFinal, classTypeParams) <- maybeT $ fetchDataRecursive $
      angleClassDefinition (Angle.factId (Glean.getId decl))
    name <- qName classDeclaration_key_name
    let typeParams = map toTypeParameter classTypeParams
    pure $ Class (modifiersForClass isAbstract isFinal) (QualName name)
      typeParams
containerDecl
  (Hack.ContainerDeclaration_interface_ decl@Hack.InterfaceDeclaration{..}) = do
    Hack.InterfaceDeclaration_key{..} <- liftMaybe interfaceDeclaration_key
    interTypeParams <- maybeT $ fetchDataRecursive $
      angleInterfaceDefinition (Angle.factId (Glean.getId decl))
    name <- qName interfaceDeclaration_key_name
    let typeParams = map toTypeParameter interTypeParams
    pure $ Interface (QualName name) typeParams
containerDecl Hack.ContainerDeclaration_EMPTY = MaybeT (return Nothing)

containerKind
  :: Hack.ContainerDeclaration -> Container
containerKind Hack.ContainerDeclaration_enum_ {} = EnumContainer
containerKind Hack.ContainerDeclaration_trait {} = TraitContainer
containerKind Hack.ContainerDeclaration_class_ {} = ClassContainer
containerKind Hack.ContainerDeclaration_interface_ {} = InterfaceContainer
containerKind Hack.ContainerDeclaration_EMPTY = ClassContainer

qName :: Hack.QName -> Glean.MaybeTRepoHaxl u w ([Text], Text)
qName Hack.QName{..} = do
 Hack.QName_key{..} <- liftMaybe qName_key
 namespace <- namespaceQName qName_key_namespace_
 name <- liftMaybe $ Hack.name_key qName_key_name
 return (namespace, name)

namespaceQName :: Maybe Hack.NamespaceQName -> Glean.MaybeTRepoHaxl u w [Text]
namespaceQName Nothing = return []
namespaceQName (Just name) = do
  let Hack.NamespaceDeclaration_key{..} = Hack.NamespaceDeclaration_key name
  alias <- lift $ fetchDataRecursive $ predicate @Hack.GlobalNamespaceAlias $
      rec $
        field @"to"
          (Angle.asPredicate $ Angle.factId
          $ Glean.getId namespaceDeclaration_key_name)
      end
  case alias of
    Just Hack.GlobalNamespaceAlias
      { globalNamespaceAlias_key = Just Hack.GlobalNamespaceAlias_key
          { globalNamespaceAlias_key_from=from
          }
      } -> do
        name <- liftMaybe $ Hack.name_key from
        return [name]
    _ -> return $ fromMaybe [] $ namespaceQNameInner (Just name) []
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

modifiersForClass :: Bool -> Bool -> ClassMod
modifiersForClass isAbstract isFinal =
  ClassMod
    (if isAbstract then Abstract else NotAbstract)
    (if isFinal then Final else NotFinal)

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

modifiersForProperty :: Hack.PropertyDefinition_key -> PropertyMod
modifiersForProperty Hack.PropertyDefinition_key {..} =
  PropertyMod
  (if propertyDefinition_key_isAbstract then Abstract else NotAbstract)
  (if propertyDefinition_key_isFinal then Final else NotFinal)
  (case propertyDefinition_key_visibility of
    Hack.Visibility_Public -> Public
    Hack.Visibility_Protected -> Protected
    Hack.Visibility_Private -> Private
    Hack.Visibility_Internal -> Internal
    Hack.Visibility__UNKNOWN{} -> error "unexpected visibility"
  )
  (if propertyDefinition_key_isStatic then Static else NotStatic)

toSignature :: [Hack.TypeParameter] -> Hack.Signature -> Signature
toSignature typeParams Hack.Signature {..} =
  Signature
  (ReturnType $ case signature_key of
    Nothing -> unknownType
    Just (Hack.Signature_key mtype _ _) -> unHackType $ toType mtype
  )
  (map toTypeParameter typeParams)
  (case signature_key of
    Nothing -> []
    Just (Hack.Signature_key _ params _) -> map toParameter params
  )

toType :: Maybe Hack.Type -> HackType
toType Nothing = HackType unknownType
toType (Just (Hack.Type _ mkey)) = HackType $ fromMaybe unknownType mkey

toType1 :: Hack.Type -> HackType
toType1 (Hack.Type _ mkey) = HackType $ fromMaybe unknownType mkey

toTypeParameter :: Hack.TypeParameter -> TypeParameter
toTypeParameter
  (Hack.TypeParameter name variance reifyKind constraints _attributes) =
    TypeParameter
    (toName name)
    (toVariance variance)
    (toReifyKind reifyKind)
    (map toConstraint constraints)

toVariance :: Hack.Variance -> Variance
toVariance Hack.Variance_Invariant = Invariant
toVariance Hack.Variance_Contravariant = Contravariant
toVariance Hack.Variance_Covariant = Covariant
toVariance (Hack.Variance__UNKNOWN _) = Invariant

toReifyKind :: Hack.ReifyKind -> Reify
toReifyKind Hack.ReifyKind_Reified = Reified
toReifyKind Hack.ReifyKind_SoftReified = SoftReified
toReifyKind Hack.ReifyKind_Erased = Erased
toReifyKind (Hack.ReifyKind__UNKNOWN _) = Erased

toConstraint :: Hack.Constraint -> Constraint
toConstraint (Hack.Constraint kind ty) =
  Constraint (toConstraintKind kind) (toType $ Just ty)

toConstraintKind :: Hack.ConstraintKind -> ConstraintKind
toConstraintKind Hack.ConstraintKind_Equal = Equal
toConstraintKind Hack.ConstraintKind_As = As
toConstraintKind Hack.ConstraintKind_Super = Super
toConstraintKind (Hack.ConstraintKind__UNKNOWN _) = Equal

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

angleClassDefinition
  :: Angle Hack.ClassDeclaration -> Angle (Bool, Bool, [Hack.TypeParameter])
angleClassDefinition decl = vars $ \isAbstract isFinal typeParams ->
  tuple (isAbstract, isFinal, typeParams) `where_` [
    wild .= predicate @Hack.ClassDefinition (
      rec $
        field @"declaration" (Angle.asPredicate decl) $
        field @"isAbstract" isAbstract $
        field @"isFinal" isFinal $
        field @"typeParams" typeParams
      end)
  ]

angleTraitDefinition
  :: Angle Hack.TraitDeclaration -> Angle [Hack.TypeParameter]
angleTraitDefinition decl = var $ \typeParams ->
  typeParams `where_` [
    wild .= predicate @Hack.TraitDefinition (
      rec $
        field @"declaration" (Angle.asPredicate decl) $
        field @"typeParams" typeParams
      end)
  ]

angleInterfaceDefinition
  :: Angle Hack.InterfaceDeclaration -> Angle [Hack.TypeParameter]
angleInterfaceDefinition decl = var $ \typeParams ->
  typeParams `where_` [
    wild .= predicate @Hack.InterfaceDefinition (
      rec $
        field @"declaration" (Angle.asPredicate decl) $
        field @"typeParams" typeParams
      end)
  ]

angleTypedefDefinition
  :: Angle Hack.TypedefDeclaration -> Angle [Hack.TypeParameter]
angleTypedefDefinition decl = var $ \typeParams ->
  typeParams `where_` [
    wild .= predicate @Hack.TypedefDefinition (
      rec $
        field @"declaration" (Angle.asPredicate decl) $
        field @"typeParams" typeParams
      end)
  ]

angleTypeConstDefinition
  :: Angle Hack.TypeConstDeclaration -> Angle Hack.TypeConstKind
angleTypeConstDefinition decl = var $ \typeConstKind ->
  typeConstKind `where_` [
    wild .= predicate @Hack.TypeConstDefinition (
      rec $
        field @"declaration" (Angle.asPredicate decl) $
        field @"kind" typeConstKind
      end)
  ]

-- hack enums: need to distinguish `enum T : Z` , or `enum class T : X as Y`
angleEnumDefinition
  :: Angle Hack.EnumDeclaration -> Angle (Hack.Type, Maybe Hack.Type, Bool)
angleEnumDefinition decl = vars $ \(baseType :: Angle Hack.Type)
    (asType :: Angle (Maybe Hack.Type)) (isEnumClass :: Angle Bool) ->
  tuple (baseType, asType, isEnumClass) `where_` [
    wild .= predicate @Hack.EnumDefinition (
      rec $
        field @"declaration" (Angle.asPredicate decl) $
        field @"enumBase" (Angle.asPredicate baseType) $
        field @"enumConstraint" asType $
        field @"isEnumClass" isEnumClass
      end)
  ]

angleMethodDefinition
  :: Angle Hack.MethodDeclaration -> Angle Hack.MethodDefinition
angleMethodDefinition decl = predicate @Hack.MethodDefinition $
  rec $
    field @"declaration" (Angle.asPredicate decl)
  end

angleFunctionDefinition
  :: Angle Hack.FunctionDeclaration -> Angle Hack.FunctionDefinition
angleFunctionDefinition decl = predicate @Hack.FunctionDefinition $
  rec $
    field @"declaration" (Angle.asPredicate decl)
  end
