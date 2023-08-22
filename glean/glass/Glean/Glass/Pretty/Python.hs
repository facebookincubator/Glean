{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications, ApplicativeDo #-}

module Glean.Glass.Pretty.Python
  (
    prettyPythonSignature
  ) where

import Data.Maybe ( mapMaybe, isNothing )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Text ( Text, takeWhileEnd )
import Data.Text.Prettyprint.Doc
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad ( forM )
import Util.List ( uniq )

import Glean.Angle as Angle
import Glean.Glass.Path ( fromGleanPath )
import Glean.Glass.Base ( GleanPath(GleanPath) )
import Glean.Glass.Types ( SymbolId(..), RepoName(..) )
import Glean.Glass.Utils
import Glean.Glass.SymbolId ( toSymbolId )
import Glean.Util.ToAngle ( ToAngle(toAngle) )
import qualified Glean
import qualified Glean.Haxl.Repos as Glean

import Glean.Schema.CodePython.Types as Python ( Entity(..) )
import qualified Glean.Schema.Python.Types as Python
import qualified Glean.Schema.Src.Types as Src
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.Codemarkup.Types as Code

-- Type of Python entity signatures to capture the the subset we generate
-- We use this to separate the processing of the Glean type from the text
-- we wish to generate

-- Python symbol kinds
data Definition
  = Function {
      funModifier :: !AsyncModifier,
      funName :: !Name,
      funParams :: [Parameter], -- regular parameters
      funPosOnlyParams :: [Parameter], -- posonly
      funKWOnlyParams :: [Parameter], -- kw only
      funStarArg :: Maybe Parameter, -- star args
      funStarKWArg :: Maybe Parameter, --  star star kwargs
      funRetType :: !AType
    }
  | Module !Name
  | Variable !Name !AType
  | Class {
      _clsName :: !Name,
      _baseNames :: [Name]
    }

data AType
  = NoType
  | AType !PyType XRefs

data Parameter = Parameter !Name (Maybe ExprText) !(Maybe PyType) XRefs

-- modifiers
data AsyncModifier = Async | NotAsync
 deriving (Eq, Show)

newtype ExprText = ExprText Text

-- names
newtype Name = Name Text

newtype PyType = PyType Text

type XRef = (Python.Declaration, Src.ByteSpan, GleanPath)
type XRefs = [XRef]

data Ann
  = None
  | BareDecl !Python.Declaration !GleanPath
  | SymId !SymbolId

prettyPythonSignature
  :: LayoutOptions
  -> RepoName
  -> SymbolId
  -> Python.Entity
  -> Glean.RepoHaxl u w (Maybe (SimpleDocStream (Maybe SymbolId)))
prettyPythonSignature opts repo sym (Python.Entity_decl decl) = runMaybeT $ do
    pyDef <- maybeT $ fetchDataRecursive (angleDeclToDef (toAngle decl))
    def <- maybeT $ fmap (pprDefinition sym) <$> fromAngleDefinition pyDef
    maybeT $ Just <$> sequence (annotateDocs def)
  where
    annotateDocs doc = reAnnotateS (declToSymbolId repo) (layoutSmart opts doc)
prettyPythonSignature _ _ _ Python.Entity_EMPTY = return Nothing

declToSymbolId :: RepoName -> Ann -> Glean.RepoHaxl u w (Maybe SymbolId)
declToSymbolId _repo None = return Nothing
declToSymbolId _repo (SymId symId) = return (Just symId)
declToSymbolId repo (BareDecl decl filepath) = Just <$>
    toSymbolId (fromGleanPath repo filepath) entity
  where
    entity = Code.Entity_python (Python.Entity_decl decl)

fromAngleDefinition
  :: Python.Definition -> Glean.RepoHaxl u w (Maybe Definition)
fromAngleDefinition def = case def of
  Python.Definition_func fn -> Just <$>
    (fromFunctionDefinition =<< Glean.keyOf fn)
  Python.Definition_module m -> Just <$>
    (fromModuleDefinition =<< Glean.keyOf m)
  Python.Definition_cls c -> Just <$>
    (fromClassDefinition =<< Glean.keyOf c)
  Python.Definition_variable v -> Just <$>
    (fromVariableDefinition =<< Glean.keyOf v)
  _ -> pure Nothing

fromClassDefinition
  :: Python.ClassDefinition_key -> Glean.RepoHaxl u w Definition
fromClassDefinition def = do
  Python.ClassDeclaration_key name _bases <- Glean.keyOf decl
  -- get class xrefs from bases
  baseNames <- case mBases of
    Nothing -> pure []
    Just decls ->
      mapM (fmap Python.classDeclaration_key_name <$> Glean.keyOf) decls

  nameStr <- trimModule <$> Glean.keyOf name
  baseStrs <- mapM (fmap trimModule <$> Glean.keyOf) baseNames
  return $ Class nameStr baseStrs
  where
    Python.ClassDefinition_key {
      classDefinition_key_bases = mBases,
      classDefinition_key_declaration = decl
    } = def

fromVariableDefinition
  :: Python.VariableDefinition_key -> Glean.RepoHaxl u w Definition
fromVariableDefinition def = do
  Python.VariableDeclaration_key name <- Glean.keyOf decl
  let mVarTy = case mTypeInfo of
        Nothing -> Nothing
        Just tyInfo ->
          let tyNames = map Python.xRefViaName_target
                (Python.typeInfo_xrefs tyInfo)
          in Just (tyNames, tyInfo)
  declMap <- fetchDeclWithNames $ maybe [] fst mVarTy
  varType <- case mVarTy of
    Nothing -> pure NoType
    Just (_, retTyInfo) -> do
      retTyText <- fromTypeInfo retTyInfo
      pure $ AType retTyText (mkXRefs declMap retTyInfo)
  varName <- trimModule <$> Glean.keyOf name
  return $ Variable varName varType
  where
    Python.VariableDefinition_key {
      variableDefinition_key_typeInfo = mTypeInfo,
      variableDefinition_key_declaration = decl
    } = def

fromModuleDefinition
  :: Python.ModuleDefinition_key -> Glean.RepoHaxl u w Definition
fromModuleDefinition (Python.ModuleDefinition_key mod) = do
  Python.Module_key name <- Glean.keyOf mod
  modName <- Glean.keyOf name
  return $ Module (Name modName)

fromFunctionDefinition
  :: Python.FunctionDefinition_key -> Glean.RepoHaxl u w Definition
fromFunctionDefinition def = do
  Python.FunctionDeclaration_key name <- Glean.keyOf decl

  let returnTy = case mReturnTy of
        Nothing -> Nothing
        Just tyInfo ->
          let retTypeDeclNames = map Python.xRefViaName_target
                (Python.typeInfo_xrefs tyInfo)
          in Just (retTypeDeclNames, tyInfo)

  -- resolve all names to their xref decls in one shot
  declMap <- fetchDeclWithNames $ concat
    [ map Python.xRefViaName_target (Python.typeInfo_xrefs tyInfo)
    | Just tyInfo <- map Python.parameter_typeInfo pyParams
    ] ++ maybe [] fst returnTy
      ++ maybe []
            (map Python.xRefViaName_target . Python.typeInfo_xrefs)
            (Python.parameter_typeInfo =<< mStarKWArg)
      ++ maybe []
            (map Python.xRefViaName_target . Python.typeInfo_xrefs)
            (Python.parameter_typeInfo =<< mStarArg)
      ++ maybe [] (\ps -> concat
          [ map Python.xRefViaName_target (Python.typeInfo_xrefs tyInfo)
          | Just tyInfo <- map Python.parameter_typeInfo ps
          ]) mKWOnlyParams
      ++ maybe [] (\ps -> concat
          [ map Python.xRefViaName_target (Python.typeInfo_xrefs tyInfo)
          | Just tyInfo <- map Python.parameter_typeInfo ps
          ]) mPosOnlyParams

  let regParamsAndXRefs =
        [ case parameter_typeInfo of
            Nothing -> (param, [])
            Just tyInfo -> (param, mkXRefs declMap tyInfo)
        | param@Python.Parameter{..} <- pyParams
        ]
  let kwOnlyParamsAndXRefs = case mKWOnlyParams of
        Nothing -> []
        Just ps ->
          [ case parameter_typeInfo of
              Nothing -> (param, [])
              Just tyInfo -> (param, mkXRefs declMap tyInfo)
          | param@Python.Parameter{..} <- ps
          ]
  let posOnlyParamsAndXRefs = case mPosOnlyParams of
        Nothing -> []
        Just ps ->
          [ case parameter_typeInfo of
              Nothing -> (param, [])
              Just tyInfo -> (param, mkXRefs declMap tyInfo)
          | param@Python.Parameter{..} <- ps
          ]
  let starKWArgsAndXRefs = case mStarKWArg of
        Nothing -> Nothing
        Just param@Python.Parameter{..} -> Just $ case parameter_typeInfo of
            Nothing -> (param, []) -- has no type info
            Just tyInfo -> (param, mkXRefs declMap tyInfo)
  let starArgAndXRefs = case mStarArg of
        Nothing -> Nothing
        Just param@Python.Parameter{..} -> Just $ case parameter_typeInfo of
            Nothing -> (param, []) -- has no type info
            Just tyInfo -> (param, mkXRefs declMap tyInfo)

  funRetType <- case returnTy of
    Nothing -> pure NoType
    Just (_, retTyInfo) -> do
      retTyText <- fromTypeInfo retTyInfo
      pure $ AType retTyText (mkXRefs declMap retTyInfo)

  funName <- trimModule <$> Glean.keyOf name
  funParams <- mapM fromParameter regParamsAndXRefs
  funPosOnlyParams <- mapM fromParameter posOnlyParamsAndXRefs
  funKWOnlyParams <- mapM fromParameter kwOnlyParamsAndXRefs
  funStarKWArg <- mapM fromParameter starKWArgsAndXRefs
  funStarArg <- mapM fromParameter starArgAndXRefs

  return $ Function { funModifier = if async then Async else NotAsync, .. }
  where
    Python.FunctionDefinition_key {
      functionDefinition_key_declaration = decl,
      functionDefinition_key_is_async = async,
      functionDefinition_key_params = pyParams,
      functionDefinition_key_kwonly_params = mKWOnlyParams,
      functionDefinition_key_posonly_params = mPosOnlyParams,
      functionDefinition_key_star_arg = mStarArg,
      functionDefinition_key_star_kwarg = mStarKWArg,
      functionDefinition_key_returnsInfo = mReturnTy
    } = def

mkXRefs
  :: Map.Map Python.Name (Python.Declaration, GleanPath)
  -> Python.TypeInfo
  -> XRefs
mkXRefs declMap Python.TypeInfo{..} =
  mapMaybe (\Python.XRefViaName{..} ->
    case Map.lookup xRefViaName_target declMap of
      Nothing -> Nothing
      Just (decl, path) -> Just (decl, xRefViaName_source, path)
  ) typeInfo_xrefs

fromTypeInfo  :: Python.TypeInfo -> Glean.RepoHaxl u w PyType
fromTypeInfo Python.TypeInfo{..} = PyType <$> Glean.keyOf
  typeInfo_displayType

fromParameter :: (Python.Parameter, XRefs) -> Glean.RepoHaxl u w Parameter
fromParameter (Python.Parameter{..}, xrefs) = do
  nameStr <- Glean.keyOf parameter_name
  tyInfo <- case parameter_typeInfo of
    Nothing -> return Nothing
    Just ty -> Just <$> fromTypeInfo ty
  return $ Parameter (Name nameStr) (ExprText <$> parameter_value) tyInfo xrefs

pprDefinition :: SymbolId -> Definition -> Doc Ann
pprDefinition self (Module name) =
  "module" <+> annotate (SymId self) (pprName name)
pprDefinition self (Class name _) =
  "class" <+> annotate (SymId self) (pprName name)

pprDefinition self (Variable name mTyInfo) =
  annotate (SymId self) (pprName name) <> case mTyInfo of
    NoType -> emptyDoc
    AType ty xrefs -> colon <+> pprTypeXRefs ty xrefs

-- empty param case
pprDefinition self (Function async name [] [] [] Nothing Nothing returnTy) =
  hcat [
   pprAsync async, "def" <+>
      annotate (SymId self) (pprName name), "()",
   pprReturnType returnTy
  ]
-- full param list
pprDefinition self (Function async name params posOnlyParams
       kwOnlyParams starArg starKWArg returnTy) =
  vcat [
    nest 4 (vsep (
      hcat [pprAsync async, "def" <> space,
        annotate (SymId self) (pprName name) <> lparen] :
      punctuate comma ( -- ordering is quite semantically sensitive
        concat [
          pprPosOnlyParams posOnlyParams,
          map pprParam params,
          maybe [] (pure . pprStarArg) starArg,
          pprKWOnlyParams (isNothing starArg) kwOnlyParams,
          maybe [] (pure . pprStarKWArg) starKWArg
        ]))),
    rparen <> pprReturnType returnTy
    ]

pprAsync :: AsyncModifier -> Doc Ann
pprAsync Async = "async" <> space
pprAsync _ = emptyDoc

pprReturnType :: AType -> Doc Ann
pprReturnType NoType = emptyDoc
pprReturnType (AType ty xrefs) = space <> "->" <+> pprTypeXRefs ty xrefs

pprKWOnlyParams :: Bool -> [Parameter] -> [Doc Ann]
pprKWOnlyParams _ [] = []
pprKWOnlyParams True xs  = "*" : map pprParam xs
pprKWOnlyParams False xs =       map pprParam xs

pprPosOnlyParams :: [Parameter] -> [Doc Ann]
pprPosOnlyParams [] = []
pprPosOnlyParams xs = map pprParam xs ++ ["/"]

pprParam :: Parameter -> Doc Ann
pprParam (Parameter name mDefValue mty xrefs) = hcat
  [ pprName name
  , case mty of
    Nothing -> emptyDoc
    Just ty -> colon <+> pprTypeXRefs ty xrefs
  , case mDefValue of
    Nothing -> emptyDoc
    Just (ExprText val) -> space <> equals <+> pretty val
  ]

pprStarArg :: Parameter -> Doc Ann
pprStarArg param = "*" <> pprParam param

pprStarKWArg :: Parameter -> Doc Ann
pprStarKWArg param = "**" <> pprParam param

pprName :: Name -> Doc Ann
pprName (Name name) = pretty name

pprTypeXRefs :: PyType -> XRefs -> Doc Ann
pprTypeXRefs (PyType ty) xrefs =
    mconcat $ (\(frag, ann) -> annotate (toAnn ann) $ pretty frag) <$>
      splitString ty spans
  where
    toAnn Nothing = None
    toAnn (Just (decl, path)) = BareDecl decl path

    spans = map (\(decl, Src.ByteSpan{..}, filepath) ->
               ((decl,filepath), fromIntegral (Glean.fromNat byteSpan_start)
                   , fromIntegral (Glean.fromNat byteSpan_length))) xrefs

--
-- | Convert a list of python.Names into their corresponding decl/location pairs
--
fetchDeclWithNames
  :: [Python.Name]
  -> Glean.RepoHaxl u w (Map Python.Name (Python.Declaration, GleanPath))
fetchDeclWithNames [] = pure mempty
fetchDeclWithNames names = do
  (result,_truncated) <- searchRecursiveWithLimit maxXRefs
    (angleDeclsByNames ids)
  Map.fromList <$> forM result (\(decl,srcFile) -> do
    Python.DeclarationWithName_key{..} <- Glean.keyOf decl
    filepath <- GleanPath <$> Glean.keyOf srcFile
    let decl = (declarationWithName_key_declaration, filepath)
        name = declarationWithName_key_name
    return (name, decl)
   )
  where
    maxXRefs = Just (length ids)

    ids :: [Glean.IdOf Python.Name]
    ids = uniq (map Glean.getId names)

angleDeclToDef :: Angle Python.Declaration -> Angle Python.Definition
angleDeclToDef decl = var $ \(def :: Angle Python.Definition) ->
  def `where_` [
    wild .= predicate @Python.DeclarationDefinition (
      rec $
        field @"declaration" decl $
        field @"definition" def
      end
    )
  ]

-- Bulk convert each name to its definition entity and file location
-- to build a symbol id later
angleDeclsByNames
  :: [Glean.IdOf Python.Name] -> Angle (Python.DeclarationWithName, Src.File)
angleDeclsByNames names = vars $ \(decl :: Angle Python.Declaration)
    (file :: Angle Src.File) (p :: Angle Python.DeclarationWithName) ->
  tuple (p, file) `where_` [
    p .= predicate @Python.DeclarationWithName (
      rec $
        field @"name" (asPredicate (elementsOf (factIdsArray names))) $
        field @"declaration" decl
      end),
    wild .= predicate @Code.EntityLocation (
      rec $
        field @"entity" (alt @"python" (alt @"decl" decl)) $
        field @"location" (rec $ field @"file" (asPredicate file) $ end)
      end)
    ]

-- | we could use the sname here to lookup the associated decl fact
-- as an xref in the type signature (c.f how Hack does this)
trimModule :: Text -> Name
trimModule qname = Name (takeWhileEnd (/= '.') qname)
