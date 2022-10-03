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
data Variance = Contravariant | Covariant | Invariant deriving (Eq)
data Reify = Erased | Reified | SoftReified deriving (Eq)
data ConstraintKind = As | Equal | Super deriving (Eq)
data Constraint = Constraint ConstraintKind HackType
data TypeParameter = TypeParameter Name Variance Reify [Constraint]
data Signature = Signature ReturnType [TypeParameter] [Parameter]

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
  | Module Name

prettyDecl :: Decl -> Text
prettyDecl (ClassConst name) =
  "const" <+> ppName name
prettyDecl (Module name) =
  "module" <+> ppName name
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
  ppFunctionModifiers modifiers <+> "function" <+> ppQualName name <>
  ppSignature signature
prettyDecl (GlobalConst name) =
  "const" <+> ppQualName name
prettyDecl (Namespace name) =
  "namespace" <+> ppQual name
prettyDecl (Method modifiers _container name signature) =
  ppMethodModifiers modifiers <+> ppName name <>
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
ppSignature (Signature returnType typeParams params) =
  ppTypeParams typeParams
   <> "(" <> Text.intercalate ", " (map ppParameter params) <> "):"
   <+> ppReturnType returnType

ppTypeParams :: [TypeParameter] -> Text
ppTypeParams typeParams | null typeParams = ""
ppTypeParams typeParams =
  "<" <> Text.intercalate ", " (map ppTypeParam typeParams) <> ">"

ppTypeParam :: TypeParameter -> Text
ppTypeParam (TypeParameter name variance reify constraints) =
  Text.concat $ execWriter $ do
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
  enum <- qName enumDeclaration_key_name
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
  let typeParams = Hack.methodDefinition_key_typeParams def
  let sign = Hack.methodDefinition_key_signature def
  container <- containerQualName methodDeclaration_key_container
  pure $ Method (modifiersForMethod def) container (Name name)
    (toSignature typeParams sign)
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
  name <- qName typedefDeclaration_key_name
  pure $ Typedef $ QualName name
decl Hack.Declaration_EMPTY = MaybeT (return Nothing)

containerDecl :: Hack.ContainerDeclaration -> Glean.MaybeTRepoHaxl u w Decl
containerDecl
  (Hack.ContainerDeclaration_enum_ Hack.EnumDeclaration{..}) = do
    Hack.EnumDeclaration_key{..} <- liftMaybe enumDeclaration_key
    name <- qName enumDeclaration_key_name
    pure $ Enum $ QualName name
containerDecl
  (Hack.ContainerDeclaration_trait Hack.TraitDeclaration{..}) = do
    Hack.TraitDeclaration_key{..} <- liftMaybe traitDeclaration_key
    name <- qName traitDeclaration_key_name
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
    name <- qName classDeclaration_key_name
    pure $ Class (modifiersForClass def) $ QualName name
containerDecl
  (Hack.ContainerDeclaration_interface_ Hack.InterfaceDeclaration{..}) = do
    Hack.InterfaceDeclaration_key{..} <- liftMaybe interfaceDeclaration_key
    name <- qName interfaceDeclaration_key_name
    pure $ Interface $ QualName name
containerDecl Hack.ContainerDeclaration_EMPTY = MaybeT (return Nothing)

containerQualName
  :: Hack.ContainerDeclaration -> Glean.MaybeTRepoHaxl u w QualName
containerQualName
  (Hack.ContainerDeclaration_enum_ Hack.EnumDeclaration{..}) = do
    Hack.EnumDeclaration_key{..} <- liftMaybe enumDeclaration_key
    name <- qName enumDeclaration_key_name
    pure $ QualName name
containerQualName
  (Hack.ContainerDeclaration_trait Hack.TraitDeclaration{..}) = do
    Hack.TraitDeclaration_key{..} <- liftMaybe traitDeclaration_key
    name <- qName traitDeclaration_key_name
    pure $ QualName name
containerQualName
  (Hack.ContainerDeclaration_class_ Hack.ClassDeclaration{..}) = do
    Hack.ClassDeclaration_key{..} <- liftMaybe classDeclaration_key
    name <- qName classDeclaration_key_name
    pure $ QualName name
containerQualName
  (Hack.ContainerDeclaration_interface_ Hack.InterfaceDeclaration{..}) = do
    Hack.InterfaceDeclaration_key{..} <- liftMaybe interfaceDeclaration_key
    name <- qName interfaceDeclaration_key_name
    pure $ QualName name
containerQualName Hack.ContainerDeclaration_EMPTY = liftMaybe Nothing

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
  alias <- lift $ Glean.getFirstResult $
    Glean.recursive $
    query @Hack.GlobalNamespaceAlias $
    predicate @Hack.GlobalNamespaceAlias $
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
