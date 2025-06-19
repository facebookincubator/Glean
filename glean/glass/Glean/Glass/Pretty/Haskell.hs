{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications, ApplicativeDo, CPP #-}

module Glean.Glass.Pretty.Haskell
  (
    prettyHaskellSignature
  ) where

import Compat.Prettyprinter

#if MIN_VERSION_ghc(9,2,0)
import Data.Char
import Data.List (foldl')
import Data.Text ( Text )
import qualified Data.Text.Encoding as Text

import qualified GHC
import qualified GHC.Utils.Outputable as GHC (
  renderWithContext, defaultSDocContext, defaultUserStyle, SDocContext(..))
import qualified GHC.Iface.Type as GHC (
  pprIfaceType, IfaceType(..), IfaceType(..), IfaceTyLit(..),
  IfaceTyCon(..), IfaceTyConInfo(..), IfExtName, IfaceTyConSort(..),
  IfaceAppArgs(..), many_ty, IfaceBndr(..))
#if !MIN_VERSION_ghc(9,6,0)
import qualified GHC.Iface.Type as GHC (AnonArgFlag(..))
#endif
import qualified GHC.Types.Basic as GHC (TupleSort(..))
#if !MIN_VERSION_ghc(9,6,0)
import qualified GHC.Types.Basic as GHC (PromotionFlag(..))
#endif
import qualified GHC.Types.Unique as GHC (getUnique)
import qualified GHC.Types.Name as GHC (mkExternalName)
import qualified GHC.Types.Name.Cache as GHC
import qualified GHC.Types.Name.Occurrence as GHC (
  mkOccNameFS, varName, dataName, tvName, tcName)
#if MIN_VERSION_ghc(9,6,0)
import qualified GHC.Types.Var as GHC (
  Specificity(..), ForAllTyFlag(..), FunTyFlag(..), VarBndr(..),
  visArgTypeLike, invisArgTypeLike)
#else
import qualified GHC.Types.Var as GHC (
  Specificity(..), ArgFlag(..), VarBndr(..))
#endif
import qualified GHC.Unit.Module.Env as GHC (emptyModuleEnv)
#if !MIN_VERSION_ghc(9,6,0)
import qualified GHC.Unit.Module.Name as GHC (mkModuleNameFS)
#endif
import qualified GHC.Unit.Types as GHC (fsToUnit)
import qualified GHC.Data.FastString as GHC (FastString, mkFastStringByteString)
import qualified GHC.Builtin.Utils as GHC (knownKeyNames)

import qualified Glean
import Glean.Angle as Angle
import qualified Glean.Schema.Hs.Types as Hs
import Glean.Glass.Utils ( fetchDataRecursive )
#endif

import qualified Glean.Haxl.Repos as Glean
import qualified Glean.Schema.CodeHs.Types as Hs
import Glean.Glass.Types

prettyHaskellSignature
  :: LayoutOptions
  -> Hs.Entity
  -> Glean.RepoHaxl u w (Maybe (SimpleDocStream (Maybe SymbolId)))

#if !MIN_VERSION_ghc(9,2,0)
prettyHaskellSignature _opts _entity = return Nothing
#else
prettyHaskellSignature opts entity = case entity of
    Hs.Entity_name name -> do
      mDecl <- fetchDataRecursive $ angleHaskellNameDeclaration $
        factId (Glean.getId name)
      case mDecl of
        Nothing -> return Nothing
        Just decl -> do
          mTy <- typeOfDeclaration decl
          case mTy of
            Nothing -> return Nothing
            Just ty -> do
              ifaceTy <- toIfaceType ty
              return $ Just $ ppr $
                GHC.renderWithContext ctx $
                GHC.pprIfaceType ifaceTy
    _ -> return Nothing
  where
    ppr = reAnnotateS (const Nothing) . layoutSmart opts . pretty
    baseCtx = GHC.defaultSDocContext { GHC.sdocStyle = GHC.defaultUserStyle }
    ctx = case layoutPageWidth opts of
      AvailablePerLine w _ -> baseCtx { GHC.sdocLineLength = w }
      Unbounded -> baseCtx

toIfaceType :: Hs.Type -> Glean.RepoHaxl u w GHC.IfaceType
toIfaceType ty = Glean.keyOf ty >>= \case
  Hs.Type_key_tyvar var -> return $ GHC.IfaceTyVar (textToFS var)
  Hs.Type_key_app (Hs.Type_app_ ty args) ->
    GHC.IfaceAppTy
      <$> toIfaceType ty
      <*> toIfaceTypeArgs args
  Hs.Type_key_tyconapp (Hs.Type_tyconapp_ tycon args) ->
    GHC.IfaceTyConApp
      <$> toIfaceTyCon tycon
      <*> toIfaceTypeArgs args
  Hs.Type_key_forall (Hs.Type_forall_ name kind flag inner) -> do
    let af = toIfaceArgFlag flag
    k <- toIfaceType kind
    GHC.IfaceForAllTy (GHC.Bndr (GHC.IfaceTvBndr (textToFS name, k)) af)
      <$> toIfaceType inner
  Hs.Type_key_fun (Hs.Type_fun_ mult arg res) -> do
    GHC.IfaceFunTy visArg
      <$> toIfaceType mult
      <*> toIfaceType arg
      <*> toIfaceType res
  Hs.Type_key_qual (Hs.Type_qual_ pred res) ->
    GHC.IfaceFunTy invisArg
      <$> pure GHC.many_ty
      <*> toIfaceType pred
      <*> toIfaceType res
  Hs.Type_key_lit lit -> GHC.IfaceLitTy <$> toIfaceLitType lit
  Hs.Type_key_cast ty -> toIfaceType ty
  Hs.Type_key_coercion{} -> return $ GHC.IfaceTyVar "<coercion type>"
  _ -> return $ GHC.IfaceTyVar "<unknown type>"
  where

#if MIN_VERSION_ghc(9,6,0)
visArg :: GHC.FunTyFlag
visArg = GHC.visArgTypeLike
#else
visArg :: GHC.AnonArgFlag
visArg = GHC.VisArg
#endif

#if MIN_VERSION_ghc(9,6,0)
invisArg :: GHC.FunTyFlag
invisArg = GHC.invisArgTypeLike
#else
invisArg :: GHC.AnonArgFlag
invisArg = GHC.InvisArg
#endif

toIfaceTyCon :: Hs.TyCon -> Glean.RepoHaxl u w GHC.IfaceTyCon
toIfaceTyCon tycon = do
  Hs.TyCon_key name sort prom <- Glean.keyOf tycon
  let p = if prom then GHC.IsPromoted else GHC.NotPromoted
  GHC.IfaceTyCon
    <$> toIfaceExtName name
    <*> pure (GHC.IfaceTyConInfo p (toIfaceTyConSort sort))

toIfaceExtName :: Hs.Name -> Glean.RepoHaxl u w GHC.IfExtName
toIfaceExtName n = do
  Hs.Name_key occ mod _sort <- Glean.keyOf n
  Hs.OccName_key str ns <- Glean.keyOf occ
  Hs.Module_key modname unit <- Glean.keyOf mod
  modtext <- Glean.keyOf modname
  unittext <- Glean.keyOf unit
  let
    ghcNamespace = case ns of
      Hs.Namespace_var_ -> GHC.varName
      Hs.Namespace_datacon -> GHC.dataName
      Hs.Namespace_tyvar -> GHC.tvName
      Hs.Namespace_tycon -> GHC.tcName
      _ -> GHC.tcName
    id = Glean.fromFid (Glean.idOf (Glean.getId n))
    ghcUnique = GHC.getUnique (fromIntegral id :: Int)
    ghcOcc = GHC.mkOccNameFS ghcNamespace (textToFS str)
    ghcModName = GHC.mkModuleNameFS (textToFS modtext)
    ghcUnit = GHC.fsToUnit (textToFS unittext)
    ghcMod = GHC.mkModule ghcUnit ghcModName
  case GHC.lookupOrigNameCache nameCache ghcMod ghcOcc of
    Just name -> return name
    Nothing -> return $ GHC.mkExternalName ghcUnique ghcMod ghcOcc GHC.noSrcSpan

-- For known-key Names we have to ensure that we use the correct Name,
-- otherwise pretty-printing won't work correctly. This cache maps
-- known qualified names to their GHC.Name. It must be at the top
-- level, so it is built only once and cached.
nameCache :: GHC.OrigNameCache
#if MIN_VERSION_ghc(9,4,0)
nameCache = foldl' GHC.extendOrigNameCache' GHC.emptyModuleEnv GHC.knownKeyNames
#else
nameCache = foldl' GHC.extendOrigNameCache GHC.emptyModuleEnv GHC.knownKeyNames
#endif

toIfaceTyConSort :: Hs.TyConSort -> GHC.IfaceTyConSort
toIfaceTyConSort sort = case sort of
  Hs.TyConSort_normal{} -> GHC.IfaceNormalTyCon
  Hs.TyConSort_tuple (Hs.TyConSort_tuple_ arity sort) ->
    GHC.IfaceTupleTyCon (fromIntegral (Glean.fromNat arity)) $
      case sort of
        Hs.TupleSort_boxed -> GHC.BoxedTuple
        Hs.TupleSort_unboxed -> GHC.UnboxedTuple
        Hs.TupleSort_constraint -> GHC.ConstraintTuple
        Hs.TupleSort__UNKNOWN{} -> GHC.BoxedTuple
  Hs.TyConSort_sum (Hs.TyConSort_sum_ arity) ->
    GHC.IfaceSumTyCon (fromIntegral (Glean.fromNat arity))
  Hs.TyConSort_equality{} -> GHC.IfaceEqualityTyCon
  Hs.TyConSort_EMPTY -> GHC.IfaceNormalTyCon

toIfaceTypeArgs :: [Hs.TypeArg] -> Glean.RepoHaxl u w GHC.IfaceAppArgs
toIfaceTypeArgs [] = return GHC.IA_Nil
toIfaceTypeArgs (Hs.TypeArg vis ty : xs) =
  GHC.IA_Arg
    <$> toIfaceType ty
    <*> pure (if vis then GHC.Required else GHC.Specified)
    <*> toIfaceTypeArgs xs

toIfaceArgFlag :: Hs.ArgFlag -> ForAllTyFlag
toIfaceArgFlag (Hs.ArgFlag_invisible spec) =
  GHC.Invisible $ case spec of
    Hs.Specificity_inferred -> GHC.InferredSpec
    Hs.Specificity_specified -> GHC.SpecifiedSpec
    Hs.Specificity__UNKNOWN{} -> GHC.SpecifiedSpec
toIfaceArgFlag Hs.ArgFlag_requird{} = GHC.Required
toIfaceArgFlag Hs.ArgFlag_EMPTY{} = GHC.Required

#if MIN_VERSION_ghc(9,6,0)
type ForAllTyFlag = GHC.ForAllTyFlag
#else
type ForAllTyFlag = GHC.ArgFlag
#endif

toIfaceLitType :: Hs.LitType -> Glean.RepoHaxl u w GHC.IfaceTyLit
toIfaceLitType l = Glean.keyOf l >>= \case
  Hs.LitType_key_num n ->
    return $ GHC.IfaceNumTyLit (fromIntegral (Glean.fromNat n))
  Hs.LitType_key_str txt ->
    return $ GHC.IfaceStrTyLit (textToFS txt)
  Hs.LitType_key_chr c ->
    return $ GHC.IfaceCharTyLit (chr (fromIntegral (Glean.fromNat c)))
  Hs.LitType_key_EMPTY{} ->
    return $ GHC.IfaceNumTyLit 0

angleHaskellNameDeclaration :: Angle Hs.Name -> Angle Hs.Declaration
angleHaskellNameDeclaration name = var $ \decl ->
  decl `where_` [
    wild .= predicate @Hs.DeclarationOfName (
      rec $
        field @"name" (asPredicate name) $
        field @"decl" decl
      end
    )
  ]

typeOfDeclaration :: Hs.Declaration -> Glean.RepoHaxl u w (Maybe Hs.Type)
typeOfDeclaration (Hs.Declaration_val val) = do
  k <- Glean.keyOf val
  return (Hs.valBind_key_ty k)
typeOfDeclaration (Hs.Declaration_patBind val) = do
  k <- Glean.keyOf val
  return (Hs.patBind_key_ty k)
typeOfDeclaration _other =
  return Nothing

textToFS :: Text -> GHC.FastString
textToFS txt = GHC.mkFastStringByteString (Text.encodeUtf8 txt)
#endif
