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
  , ByteSpan(..)
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
  , ReadOnly(..)
  , Signature (..)
  , HackType(..)
  , ReturnType(..)
  , ModuleInternal(..)
  , DefaultValue(..)
  , Inout(..)
  , Variadic(..)
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
  , Transparency(..)
  ) where

import Data.List as List ( foldl' )
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import qualified Data.Text as Text
import Control.Monad.Extra ( when, whenJust )
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Writer.Strict ( execWriter, tell )
import qualified Data.Map.Strict as Map

import Glean.Glass.SymbolId ( toSymbolId )
import Glean.Glass.Types ( SymbolId(..), RepoName(..) )
import Glean.Glass.Base ( GleanPath(..) )
import Glean.Glass.Path ( fromGleanPath )
import Glean.Util.ToAngle ( ToAngle(toAngle) )
import Glean.Glass.Utils
import qualified Glean.Glass.SymbolId.Hack as Hack

import qualified Glean
import Glean.Angle as Angle
import qualified Glean.Haxl.Repos as Glean
import qualified Glean.Schema.Hack.Types as Hack
import qualified Glean.Schema.Src.Types as Src
import Glean.Schema.CodeHack.Types as Hack
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Codemarkup.Types as Code

-- Pretty-printer annotations for Doc or SimpleDocStream
-- Used to collect xrefs bytespan in pretty-printed signatures
data Ann
  = None
  | BareDecl !Hack.Declaration {- todo: !GleanPath -}
  | SymId !SymbolId

prettyHackSignature
  :: LayoutOptions
  -> RepoName
  -> SymbolId
  -> Hack.Entity
  -> Glean.RepoHaxl u w (Maybe (SimpleDocStream (Maybe SymbolId)))
prettyHackSignature opts repo sym (Hack.Entity_decl d) = runMaybeT $ do
  docStream <- layoutSmart opts . prettyDecl opts sym <$> decl d
  let docStreamSymbol = sequence $ reAnnotateS (declToSymbolId repo) docStream
  maybeT $ Just <$> docStreamSymbol
prettyHackSignature _ _ _ Hack.Entity_EMPTY = return Nothing

-- Turn declaration to symbol ids
-- This requires a query to Code.EntityLocation to gather
-- the path to an entity, needed for constructing a SymbolId
declToSymbolId :: RepoName -> Ann -> Glean.RepoHaxl u w (Maybe SymbolId)
declToSymbolId _repo None = return Nothing
declToSymbolId _repo (SymId sym) = return (Just sym)
declToSymbolId repo (BareDecl decl) = runMaybeT $ do
  Code.EntityLocation{..} <- maybeT $ fetchDataRecursive $
    angleEntityLocation entityAngle
  Code.EntityLocation_key{..} <- liftMaybe entityLocation_key
  let Code.Location{..} = entityLocation_key_location
  path <- MaybeT (Just . GleanPath <$> Glean.keyOf location_file)
  MaybeT $ Just <$> toSymbolId (fromGleanPath repo path) entity
  where
    entity = Code.Entity_hack (Hack.Entity_decl decl)
    entityAngle = alt @"hack" (alt @"decl" (toAngle decl))

newtype Name = Name Text
newtype Qual = Qual [Text]
newtype QualName = QualName ([Text], Text)

-- Hack modifier orderings are not as strict as this but since we want
-- to print them in some consistent ordering we enforce the most popular
data FunctionMod = FunctionMod !Async !ModuleInternal
data ClassMod    = ClassMod Abstract Final !ModuleInternal
data MethodMod   =
  MethodMod Abstract Final Visibility Static Async ReadOnly
data PropertyMod   = PropertyMod Abstract Final Visibility Static

data Abstract = Abstract | NotAbstract deriving Eq
data Final = Final | NotFinal deriving Eq
data Visibility = Public | Protected | Private | Internal
data Static = Static | NotStatic deriving Eq
data Async = Async | NotAsync deriving Eq
data ModuleInternal = IsInternal | NotInternal deriving Eq
data ReadOnly = IsReadOnly | NotReadOnly deriving Eq

data ByteSpan = ByteSpan
  { start :: {-# UNPACK #-}!Int
  , length :: {-# UNPACK #-}!Int
  }
type XRefs = [(Hack.Declaration, ByteSpan)]
newtype HackType = HackType { unHackType :: Text }
newtype ReturnType = ReturnType { unReturnType :: Text }
newtype DefaultValue = DefaultValue Text
data Inout = Inout
data Variadic = Variadic
data Parameter
  = Parameter Name HackType (Maybe Inout)
      (Maybe Variadic) (Maybe DefaultValue) ReadOnly XRefs
data Variance = Contravariant | Covariant | Invariant
data Reify = Erased | Reified | SoftReified
data ConstraintKind = As | Equal | Super
data Constraint = Constraint ConstraintKind HackType
data TypeParameter = TypeParameter Name Variance Reify [Constraint] [UserAttr]
data UserAttr = UserAttr Name [Text]
newtype Context = Context { _unContext :: Text }
data Signature = Signature ReadOnly ReturnType [TypeParameter] [Parameter]
  (Maybe [Context]) XRefs
data Container
  = ClassContainer | InterfaceContainer | TraitContainer | EnumContainer
  deriving Eq
data EnumKind = IsClass | Regular
data EnumConstraint = EnumBase HackType | Constrained HackType HackType
data TypeConstKind = IsAbstract | Concrete | PartiallyAbstract
data Transparency = Newtype_ | Type_

data Decl
  = ClassConst Name
  | Enum QualName EnumKind EnumConstraint ModuleInternal
  | Trait QualName [TypeParameter] ModuleInternal
  | Class ClassMod QualName [TypeParameter]
  | Interface QualName [TypeParameter] ModuleInternal
  | Enumerator QualName Name
  | Function FunctionMod QualName Signature
  | GlobalConst QualName
  | Namespace Qual
  | Method MethodMod Container Name Signature
  | Property PropertyMod Container Name HackType
  | TypeConst Name TypeConstKind
  | Typedef QualName [TypeParameter] Transparency ModuleInternal
  | Module Name

prettyDecl :: LayoutOptions -> SymbolId -> Decl -> Doc Ann
prettyDecl _ sym (ClassConst name) =
  "const" <+> annotate (SymId sym) (ppName name)
prettyDecl _ sym (Module name) =
  "module" <+> annotate (SymId sym) (ppName name)

prettyDecl _ sym (Enum name enumKind (EnumBase ty1) internal) =
  ppModuleInternal internal <>
    "enum" <> ppEnumClass enumKind <+>
      annotate (SymId sym) (ppQualName name) <+> ":" <+> ppType ty1
prettyDecl _ sym (Enum name enumKind (Constrained ty1 ty2) internal) =
  ppModuleInternal internal <>
    "enum" <> ppEnumClass enumKind <+>
      annotate (SymId sym) (ppQualName name) <+>
       ":" <+> ppConstraintTypes ty1 ty2

prettyDecl _ sym (Class modifiers name typeParams) =
  ppClassModifiers modifiers <> "class" <+>
    annotate (SymId sym) (ppQualName name) <> ppTypeParams typeParams
prettyDecl _ sym (Interface name typeParams internal) =
  ppModuleInternal internal <> "interface" <+>
    annotate (SymId sym) (ppQualName name) <> ppTypeParams typeParams
prettyDecl _ sym (Trait name typeParams internal) =
  ppModuleInternal internal <> "trait" <+>
    annotate (SymId sym) (ppQualName name) <> ppTypeParams typeParams

prettyDecl _ _sym (Enumerator enum name) =
  ppQualName enum <> "::" <> ppName name

prettyDecl opts sym (Function modifiers name sig) =
  ppSignature opts (ppFunctionModifiers modifiers <+>
    annotate (SymId sym) (ppQualName name)) sig
prettyDecl opts sym (Method modifiers container name sig) =
  ppSignature opts (ppMethodModifiers container modifiers <+>
    annotate (SymId sym) (ppName name)) sig

prettyDecl _ _sym (GlobalConst name) =
  "const" <+> ppQualName name
prettyDecl _ sym (Namespace name) =
  "namespace" <+> annotate (SymId sym) (ppQual name)

prettyDecl _ _ (Property modifiers container name mhacktype) =
  ppPropertyModifiers container modifiers <+> ppType mhacktype <+> ppName name

prettyDecl _ sym (TypeConst name IsAbstract) =
  "abstract" <+> "const" <+> "type" <+> annotate (SymId sym) (ppName name)
prettyDecl _ sym (TypeConst name _) =
  "const" <+> "type" <+> annotate (SymId sym) (ppName name)

prettyDecl _ _sym (Typedef name typeParams Type_ internal) =
  ppModuleInternal internal <>
    "type" <+> ppQualName name <> ppTypeParams typeParams
prettyDecl _ _sym (Typedef name typeParams Newtype_ internal) =
  ppModuleInternal internal <>
    "newtype" <+> ppQualName name <> ppTypeParams typeParams

ppEnumClass :: EnumKind -> Doc Ann
ppEnumClass Regular = emptyDoc
ppEnumClass IsClass = space <> "class"

ppName :: Name -> Doc Ann
ppName (Name n) = pretty n

-- | We never qualify names in generated signatures now
ppQualName :: QualName -> Doc Ann
ppQualName (QualName ([], name)) = pretty name
ppQualName (QualName (_namespace, name)) = pretty name

ppQual :: Qual -> Doc Ann
ppQual (Qual namespace) = concatWith (surround "\\") (pretty <$> namespace)

ppFunctionModifiers :: FunctionMod -> Doc Ann
ppFunctionModifiers (FunctionMod async internal) =
  hcat [ ppModuleInternal internal, ppAsync async, "function" ]

ppClassModifiers :: ClassMod -> Doc Ann
ppClassModifiers (ClassMod abstract final internal) =
  hcat [ ppModuleInternal internal, ppAbstract abstract, ppFinal final ]

ppModuleInternal :: ModuleInternal -> Doc Ann
ppModuleInternal IsInternal = "internal" <> space
ppModuleInternal NotInternal = mempty

ppAsync :: Async -> Doc Ann
ppAsync Async = "async" <> space
ppAsync NotAsync = mempty

ppAbstract :: Abstract -> Doc Ann
ppAbstract Abstract = "abstract" <> space
ppAbstract NotAbstract = mempty

ppFinal :: Final -> Doc Ann
ppFinal Final = "final" <> space
ppFinal NotFinal = mempty

ppReadOnly :: Doc Ann
ppReadOnly = "readonly"

ppSignature :: LayoutOptions -> Doc Ann -> Signature -> Doc Ann
ppSignature
  opts head (Signature readOnly returnType typeParams params ctxs xrefs) =
    if fitsOnOneLine then onelineSig else multilineSig
  where
    onelineTypeParams = if null typeParams then emptyDoc else cat
      [ "<"
      , sep $ punctuate "," (map ppTypeParam typeParams)
      , ">"
      ]
    onelineArgs = if null params then "()" else
      parens (hsep $ punctuate comma (map ppParameter params))
    onelineSig = nest 4 $ head <> fillCat
      [ onelineTypeParams
      , onelineArgs
      , ppContexts ctxs
      , ":" <> readOnlyKwd <+> ppReturnType returnType xrefs
      ]
    multilineTypeParams = if null typeParams then emptyDoc else vcat
        [ nest 4 $ vcat
          [ "<"
          , vcat $ map ((<> ",") . ppTypeParam) typeParams
          ]
        , ">"
        ]
    multilineArgs = vcat
      [ nest 4 (vcat $
        "("
        : map ((<> ",") . ppParameter) params
        )
      , ")"
      ]
    multilineSig = head
      <> multilineTypeParams
      <> multilineArgs
      <> nest 4 (
        ppContexts ctxs
        <> ":"
        <> readOnlyKwd
        <+> ppReturnType returnType xrefs
      )
    paramsText = renderStrict $ layoutSmart opts onelineSig
    fitsOnOneLine = not containsNewline
    containsNewline = Text.any (== '\n') paramsText
    readOnlyKwd = case readOnly of
      IsReadOnly -> space <> ppReadOnly
      NotReadOnly -> ""

ppTypeParams :: [TypeParameter] -> Doc Ann
ppTypeParams typeParams | null typeParams = emptyDoc
ppTypeParams typeParams = cat
  [ nest 4 $ cat
    [ "<", sep $ punctuate "," (map ppTypeParam typeParams)]
  , ">"
  ]

--
-- Lots of interesting syntax for type parameters
-- https://docs.hhvm.com/hack/generics/introduction
--
ppTypeParam :: TypeParameter -> Doc Ann
ppTypeParam (TypeParameter name variance reify constraints userAttrs) = hcat
  [ ppAttrs userAttrs -- attributes
  , ppReified reify -- reify keyword
  , ppVariance variance -- then the variance markers
  , ppName name -- then the type param name
  , hsep (map ppConstraint constraints) -- and any constraints
  ]

-- User attributes on type parameters, including the reification attributes
ppAttrs :: [UserAttr] -> Doc Ann
ppAttrs attrs = case attrs of
  [] -> emptyDoc
  _ -> hcat ["<<", hsep (punctuate comma (map ppAttr attrs)), ">> "]

-- https://docs.hhvm.com/hack/attributes/introduction
ppAttr :: UserAttr -> Doc Ann
-- e.g. __Memoize
ppAttr (UserAttr name []) = ppName name
-- __Deprecated("foo")
-- <<Contributors("John Doe", keyset["ORM Team", "Core Library Team"])>>
ppAttr (UserAttr name args)
  = ppName name <>
      parens (hsep (punctuate comma (map pretty args)))

-- Reified generics
-- https://docs.hhvm.com/hack/reified-generics/reified-generics
ppReified :: Reify -> Doc Ann
ppReified SoftReified = "reify "
ppReified Reified = "reify "
ppReified _ = emptyDoc

--
-- Variance markers on type parameters
-- See https://docs.hhvm.com/hack/generics/variance
--
ppVariance :: Variance -> Doc Ann
ppVariance Covariant = "+"
ppVariance Contravariant = "-"
ppVariance _ = emptyDoc

--
-- Generic type parameter constraint syntax
-- See https://docs.hhvm.com/hack/generics/type-constraints
--
ppConstraint :: Constraint -> Doc Ann
ppConstraint (Constraint kind ty) = hcat
  [ case kind of
      Equal -> " = "
      As -> " as "
      Super -> " super "
  , ppType ty
  ]

ppTypeXRefs :: HackType -> XRefs -> Doc Ann
ppTypeXRefs (HackType t) xrefs =
  let spans = fmap (\(ann, ByteSpan{..}) -> (ann, start, length)) xrefs
      fragments = splitString t spans in
  mconcat $ (\(frag, ann) -> annotate (toAnn ann) $ pretty frag) <$> fragments
  where
    toAnn Nothing = None
    toAnn (Just decl) = BareDecl decl

ppType :: HackType -> Doc Ann
ppType (HackType t) = pretty t

ppReturnType :: ReturnType -> XRefs -> Doc Ann
ppReturnType (ReturnType t) xrefs = ppTypeXRefs (HackType t) xrefs

ppParameter :: Parameter -> Doc Ann
ppParameter (Parameter name typeName inout variadic defaultValue readOnly xrefs)
  = nest 4 $ sep $ execWriter $ do
    when (readOnly == IsReadOnly) $ tell [ppReadOnly]
    whenJust inout $ tell . ppInout
    tell [ppTypeXRefs typeName xrefs]
    case variadic of
      Just Variadic -> tell [hcat ["...", ppName name]]
      Nothing ->  tell [ppName name]
    whenJust defaultValue $ tell . pure . ppDefaultValue typeName

-- Contexts can be parameterised, empty, missing. or a simple list
-- https://docs.hhvm.com/hack/contexts-and-capabilities/introduction
ppContexts :: Maybe [Context] -> Doc Ann
ppContexts Nothing = emptyDoc
ppContexts (Just []) = brackets emptyDoc
ppContexts (Just ctxs) = brackets $ hsep (punctuate comma (map ppContext ctxs))

ppContext :: Context -> Doc Ann
ppContext (Context ctx) = pretty ctx

ppDefaultValue :: HackType -> DefaultValue -> Doc Ann
ppDefaultValue typeName (DefaultValue v) = "=" <+>
  case typeName of -- work around for type-sensitive quoting
    HackType ty
      | ty `elem` ["string", "?string"], v /= "null" -> squotes (pretty v)
    _ -> pretty v

ppInout :: Inout -> [Doc Ann]
ppInout Inout = ["inout"]

ppConstraintTypes :: HackType -> HackType -> Doc Ann
ppConstraintTypes ty1 ty2 = ppType ty1 <+> "as" <+> ppType ty2

ppMethodModifiers :: Container -> MethodMod -> Doc Ann
ppMethodModifiers container
  (MethodMod abstract final visibility static async readonlyThis) =
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
    when (readonlyThis==IsReadOnly) $ tell ["readonly"]
    when (async==Async) $ tell ["async"]
    tell ["function"]

ppPropertyModifiers :: Container -> PropertyMod -> Doc Ann
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

--
-- Process the raw declaration result from Glean to build up a cleaner
-- `Decl` description of the type signature, possibly pulling in some related
-- information as we go
--
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
    (toSignature typeParams sign (Hack.functionDefinition_key_readonlyRet def))
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
    (toSignature typeParams sig (Hack.methodDefinition_key_readonlyRet def))
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
  (typedefTypeParams, isTransparent, moduleMembership) <- maybeT $
    fetchDataRecursive $
      angleTypedefDefinition (Angle.factId (Glean.getId decl))
  name <- qName typedefDeclaration_key_name
  let typeParams = map toTypeParameter typedefTypeParams
      isNewtype = if isTransparent then Type_ else Newtype_
  pure $ Typedef (QualName name) typeParams isNewtype
    (toModuleVisibility moduleMembership)
decl Hack.Declaration_EMPTY = MaybeT (return Nothing)

containerDecl :: Hack.ContainerDeclaration -> Glean.MaybeTRepoHaxl u w Decl
containerDecl (Hack.ContainerDeclaration_enum_
      decl@Hack.EnumDeclaration{..}) = do
    Hack.EnumDeclaration_key{..} <- liftMaybe enumDeclaration_key
    (enumBase,enumConstraint,isClass,moduleInfo) <- maybeT $
      fetchDataRecursive $ angleEnumDefinition (Angle.factId (Glean.getId decl))
    let enumKind = if isClass then IsClass else Regular
    let constraint =  case enumConstraint of
          Nothing -> EnumBase (toType1 enumBase)
          Just ty -> Constrained (toType1 enumBase) (toType1 ty)
    name <- qName enumDeclaration_key_name
    pure $ Enum (QualName name) enumKind constraint
      (toModuleVisibility moduleInfo)
containerDecl
  (Hack.ContainerDeclaration_trait decl@Hack.TraitDeclaration{..}) = do
    Hack.TraitDeclaration_key{..} <- liftMaybe traitDeclaration_key
    (traitTypeParams, moduleInfo) <- maybeT $ fetchDataRecursive $
      angleTraitDefinition (Angle.factId (Glean.getId decl))
    name <- qName traitDeclaration_key_name
    let typeParams = map toTypeParameter traitTypeParams
    pure $ Trait (QualName name) typeParams (toModuleVisibility moduleInfo)
containerDecl
  (Hack.ContainerDeclaration_class_ decl@Hack.ClassDeclaration{..}) = do
    Hack.ClassDeclaration_key{..} <- liftMaybe classDeclaration_key
    (isAbstract, isFinal, classTypeParams, moduleInfo) <- maybeT $
      fetchDataRecursive $ angleClassDefinition
        (Angle.factId (Glean.getId decl))
    name <- qName classDeclaration_key_name
    let typeParams = map toTypeParameter classTypeParams
    pure $ Class (modifiersForClass isAbstract isFinal
             (toModuleVisibility moduleInfo))
       (QualName name) typeParams
containerDecl
  (Hack.ContainerDeclaration_interface_ decl@Hack.InterfaceDeclaration{..}) = do
    Hack.InterfaceDeclaration_key{..} <- liftMaybe interfaceDeclaration_key
    (interTypeParams, moduleInfo) <- maybeT $ fetchDataRecursive $
      angleInterfaceDefinition (Angle.factId (Glean.getId decl))
    name <- qName interfaceDeclaration_key_name
    let typeParams = map toTypeParameter interTypeParams
    pure $ Interface (QualName name) typeParams (toModuleVisibility moduleInfo)
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
namespaceQName nsqname = go nsqname []
  where
    go Nothing acc = return acc
    go (Just nsqname@Hack.NamespaceQName{..}) acc = do
      mshortname <- lift $ shortenGlobalAlias nsqname
      case mshortname of -- if this one is global alias, we're done.
        Just name -> return (name : acc)
        Nothing -> do -- else collect name and try parent
          Hack.NamespaceQName_key{..} <- liftMaybe namespaceQName_key
          name <- liftMaybe $ Hack.name_key namespaceQName_key_name
          go namespaceQName_key_parent (name : acc)

-- in a (recursive) hack.NamespaceQName, occurences of global namespace
-- aliases should be shortened wherever they occur.
shortenGlobalAlias :: Hack.NamespaceQName -> Glean.RepoHaxl u w (Maybe Text)
shortenGlobalAlias nsqname = Map.lookup (Glean.getId nsqname) <$>
  Hack.hackGlobalNamespaceAliases

modifiersForFunction :: Hack.FunctionDefinition_key -> FunctionMod
modifiersForFunction Hack.FunctionDefinition_key{..} =
  FunctionMod asyncMod visibility
  where
    asyncMod = if functionDefinition_key_isAsync then Async else NotAsync
    visibility = toModuleVisibility functionDefinition_key_module_

toModuleVisibility :: Maybe Hack.ModuleMembership -> ModuleInternal
toModuleVisibility (Just (Hack.ModuleMembership _ True)) = IsInternal
toModuleVisibility _ = NotInternal

modifiersForClass :: Bool -> Bool -> ModuleInternal -> ClassMod
modifiersForClass isAbstract isFinal internal = ClassMod
  (if isAbstract then Abstract else NotAbstract)
  (if isFinal then Final else NotFinal)
  internal

modifiersForMethod :: Hack.MethodDefinition_key -> MethodMod
modifiersForMethod Hack.MethodDefinition_key {..} =
  MethodMod
  (if methodDefinition_key_isAbstract then Abstract else NotAbstract)
  (if methodDefinition_key_isFinal then Final else NotFinal)
  (fromHackVisibility methodDefinition_key_visibility)
  (if methodDefinition_key_isStatic then Static else NotStatic)
  (if methodDefinition_key_isAsync then Async else NotAsync)
  (if methodDefinition_key_isReadonlyThis == Just True
     then IsReadOnly else NotReadOnly)

modifiersForProperty :: Hack.PropertyDefinition_key -> PropertyMod
modifiersForProperty Hack.PropertyDefinition_key {..} =
  PropertyMod
  (if propertyDefinition_key_isAbstract then Abstract else NotAbstract)
  (if propertyDefinition_key_isFinal then Final else NotFinal)
  (fromHackVisibility propertyDefinition_key_visibility)
  (if propertyDefinition_key_isStatic then Static else NotStatic)

fromHackVisibility :: Hack.Visibility  -> Visibility
fromHackVisibility v = case v of
  Hack.Visibility_Public -> Public
  Hack.Visibility_Protected -> Protected
  Hack.Visibility_Private -> Private
  Hack.Visibility_Internal -> Internal
  Hack.Visibility__UNKNOWN{} -> error "unexpected visibility"

-- Glass-side implementation of fbcode/glean/rts/prim.cpp:relSpansToAbs
relSpansToAbs :: [Src.RelByteSpan] -> [ByteSpan]
relSpansToAbs byteSpans = snd $ List.foldl' f (0, []) byteSpans
  where
    f (!start, acc) (Src.RelByteSpan offset length) =
      let off = fromIntegral (Glean.fromNat offset)
          len = fromIntegral (Glean.fromNat length)
          start' = start + off
      in (start', ByteSpan start' len : acc)

-- Extracts type and xrefs from a TypeInfo
-- XRefs are converted from relative spans (Glean representation)
-- to absolute (Glass representation)
-- If TypeInfo doesn't exist, returned type is overridden by provided
-- default.
toTypeAndXRefs :: Maybe Hack.Type -> Maybe Hack.TypeInfo -> (HackType, XRefs)
toTypeAndXRefs type_ typeInfo = case typeInfo of
  (Just (Hack.TypeInfo _ (Just (Hack.TypeInfo_key displayType hackXRefs)))) ->
    let f (Hack.XRef declaration ranges) = case declaration of
          Hack.XRefTarget_declaration decl ->
            Just ((\x -> (decl, x)) <$> relSpansToAbs ranges)
          _ -> Nothing
        xrefs = concat (mapMaybe f hackXRefs)
    in (toType $ Just displayType, xrefs)

  _ -> (toType type_, [])

toSignature ::
  [Hack.TypeParameter] -> Hack.Signature -> Maybe Hack.ReadonlyKind -> Signature
toSignature typeParams Hack.Signature{..} mbReadOnly = case signature_key of
  Nothing -> Signature NotReadOnly (ReturnType unknownType) [] [] Nothing []
  Just (Hack.Signature_key retType params mctxs retTypeInfo) ->
   let (type_, xrefs) = toTypeAndXRefs retType retTypeInfo
       readOnly = case mbReadOnly of
         Just _ -> IsReadOnly
         Nothing -> NotReadOnly
   in Signature
        readOnly
        (ReturnType (unHackType type_))
        (map toTypeParameter typeParams)
        (map toParameter params)
        (map toContext <$> mctxs)
        -- Maybe [] is used to distinguish default context from literal "[]"
        xrefs

toType :: Maybe Hack.Type -> HackType
toType Nothing = HackType unknownType
toType (Just (Hack.Type _ mkey)) = HackType $ fromMaybe unknownType mkey

toType1 :: Hack.Type -> HackType
toType1 (Hack.Type _ mkey) = HackType $ fromMaybe unknownType mkey

toTypeParameter :: Hack.TypeParameter -> TypeParameter
toTypeParameter
  (Hack.TypeParameter name variance reifyKind constraints attributes) =
    TypeParameter
    (toName name)
    (toVariance variance)
    (toReifyKind reifyKind)
    (map toConstraint constraints)
    (mapMaybe toAttribute attributes)

toAttribute :: Hack.UserAttribute -> Maybe UserAttr
toAttribute (Hack.UserAttribute _ Nothing) = Nothing
toAttribute (Hack.UserAttribute _ (Just
    (Hack.UserAttribute_key name params _))) =
  Just $ UserAttr (toName name) params

toContext :: Hack.Context_ -> Context
toContext (Hack.Context_ _ Nothing) = Context unknownType
toContext (Hack.Context_ _ (Just ctx)) = Context ctx'
  where
    -- There are only a few dozen contexts in use.
    -- Gronky auto-imported handling hack to get a short name
    --
    -- > [\HH\Contexts\leak_safe] -> [leak_safe]
    --
    -- If we switch to proper declarations and auto-import tables we can avoid
    -- the string handling here.
    ctx' | Just tidy <- Text.stripPrefix "\\HH\\Contexts\\" ctx = tidy
         | otherwise = ctx

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
toParameter
  (Hack.Parameter name mtype inout variadic mdefaultValue _ typeInfo readOnly) =
  let (type_, xrefs) = toTypeAndXRefs mtype typeInfo
  in Parameter
      (toName name)
      (HackType (unHackType type_))
      (if inout then Just Inout else Nothing)
      (if variadic then Just Variadic else Nothing)
      (DefaultValue <$> mdefaultValue)
      (maybe NotReadOnly (const IsReadOnly) readOnly)
      xrefs

toName :: Hack.Name -> Name
toName (Hack.Name _ mkey) = Name $ fromMaybe "(anonymous)" mkey

unknownType :: Text
unknownType = "<unknown-type>"

angleClassDefinition
  :: Angle Hack.ClassDeclaration
  -> Angle (Bool, Bool, [Hack.TypeParameter], Maybe Hack.ModuleMembership )
angleClassDefinition decl = vars $ \isAbstract isFinal typeParams moduleInfo ->
  tuple (isAbstract, isFinal, typeParams, moduleInfo) `where_` [
    wild .= predicate @Hack.ClassDefinition (
      rec $
        field @"declaration" (Angle.asPredicate decl) $
        field @"isAbstract" isAbstract $
        field @"isFinal" isFinal $
        field @"typeParams" typeParams $
        field @"module_" moduleInfo
      end)
  ]

angleTraitDefinition
  :: Angle Hack.TraitDeclaration
  -> Angle ([Hack.TypeParameter], Maybe Hack.ModuleMembership)
angleTraitDefinition decl = vars $ \typeParams modInfo ->
  tuple (typeParams, modInfo) `where_` [
    wild .= predicate @Hack.TraitDefinition (
      rec $
        field @"declaration" (Angle.asPredicate decl) $
        field @"typeParams" typeParams $
        field @"module_" modInfo
      end)
  ]

angleInterfaceDefinition
  :: Angle Hack.InterfaceDeclaration
  -> Angle ([Hack.TypeParameter], Maybe Hack.ModuleMembership)
angleInterfaceDefinition decl = vars $ \typeParams modInfo ->
  tuple (typeParams, modInfo) `where_` [
    wild .= predicate @Hack.InterfaceDefinition (
      rec $
        field @"declaration" (Angle.asPredicate decl) $
        field @"typeParams" typeParams $
        field @"module_" modInfo
      end)
  ]

angleTypedefDefinition
  :: Angle Hack.TypedefDeclaration
  -> Angle ([Hack.TypeParameter], Bool, Maybe Hack.ModuleMembership)
angleTypedefDefinition decl = vars $ \typeParams transparency moduleInfo ->
  tuple (typeParams, transparency, moduleInfo) `where_` [
    wild .= predicate @Hack.TypedefDefinition (
      rec $
        field @"declaration" (Angle.asPredicate decl) $
        field @"typeParams" typeParams $
        field @"isTransparent" transparency $
        field @"module_" moduleInfo
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
  :: Angle Hack.EnumDeclaration
  -> Angle (Hack.Type, Maybe Hack.Type, Bool, Maybe Hack.ModuleMembership)
angleEnumDefinition decl = vars $ \(baseType :: Angle Hack.Type)
    asType isEnumClass modInfo ->
  tuple (baseType, asType, isEnumClass, modInfo ) `where_` [
    wild .= predicate @Hack.EnumDefinition (
      rec $
        field @"declaration" (Angle.asPredicate decl) $
        field @"enumBase" (Angle.asPredicate baseType) $
        field @"enumConstraint" asType $
        field @"isEnumClass" isEnumClass $
        field @"module_" modInfo
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

angleEntityLocation
  :: Angle Code.Entity -> Angle Code.EntityLocation
angleEntityLocation ent =
  predicate @Code.EntityLocation $
    rec $
      field @"entity" ent
    end
