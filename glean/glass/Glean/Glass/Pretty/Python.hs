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

import Data.Maybe ( mapMaybe )
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Text ( Text, takeWhileEnd )
import Data.Text.Prettyprint.Doc
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad ( forM )
import Util.List ( uniq )

import Glean.Angle as Angle
import Glean.Glass.Base ( GleanPath(..) )
import Glean.Glass.Path ( fromGleanPath )
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
  = Function !AsyncModifier !Name [Parameter] !ReturnType
 -- Class
 -- Variable
 -- Import
 -- Module

data ReturnType
  = NoReturnType
  | ReturnType !PyType XRefs

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

type Ann = Maybe (Python.Declaration, GleanPath)

prettyPythonSignature
  :: LayoutOptions
  -> RepoName
  -> Python.Entity
  -> Glean.RepoHaxl u w (Maybe (SimpleDocStream (Maybe SymbolId)))
prettyPythonSignature opts repo (Python.Entity_decl decl) = runMaybeT $ do
    pyDef <- maybeT $ fetchDataRecursive (angleDeclToDef (toAngle decl))
    def <- maybeT $ fmap pprDefinition <$> fromAngleDefinition pyDef
    maybeT $ Just <$> sequence (annotateDocs def)
  where
    annotateDocs doc = reAnnotateS (declToSymbolId repo) (layoutSmart opts doc)
prettyPythonSignature _ _ Python.Entity_EMPTY = return Nothing

declToSymbolId :: RepoName -> Ann -> Glean.RepoHaxl u w (Maybe SymbolId)
declToSymbolId _repo Nothing = return Nothing
declToSymbolId repo (Just (decl, filepath)) = Just <$>
    toSymbolId (fromGleanPath repo filepath) entity
  where
    entity = Code.Entity_python (Python.Entity_decl decl)

fromAngleDefinition
  :: Python.Definition -> Glean.RepoHaxl u w (Maybe Definition)
fromAngleDefinition def = case def of
  Python.Definition_func fn -> Just <$>
    (fromFunctionDefinition =<< Glean.keyOf fn)
  _ -> pure Nothing

fromFunctionDefinition
  :: Python.FunctionDefinition_key -> Glean.RepoHaxl u w Definition
fromFunctionDefinition def = do
  Python.FunctionDeclaration_key name <- Glean.keyOf decl
  returnTy <- case mReturnTy of
    Nothing -> pure Nothing
    Just tyInfo -> do
      let retTypeDeclNames = map Python.xRefViaName_target
            (Python.typeInfo_xrefs tyInfo)
      return $ Just (retTypeDeclNames, tyInfo)

  -- resolve all names to their xref decls in one shot
  declMap <- fetchDeclWithNames $
      concat
        [ map Python.xRefViaName_target (Python.typeInfo_xrefs tyInfo)
        | Just tyInfo <- map Python.parameter_typeInfo pyParams
        ] ++ maybe [] fst returnTy

  let paramsAndXRefs =
        [ case parameter_typeInfo of
            Nothing -> (param, [])
            Just tyInfo -> (param, mkXRefs declMap tyInfo)
        | param@Python.Parameter{..} <- pyParams
        ]

  returnTyXRefs <- case returnTy of
    Nothing -> pure NoReturnType
    Just (_, retTyInfo) -> do
      retTyText <- fromTypeInfo retTyInfo
      pure $ ReturnType retTyText (mkXRefs declMap retTyInfo)

  nameStr <- trimModule <$> Glean.keyOf name

  params <- mapM fromParameter paramsAndXRefs
  return $ Function
    (if async then Async else NotAsync)
    nameStr
    params
    returnTyXRefs
  where
    Python.FunctionDefinition_key {
      functionDefinition_key_declaration = decl,
      functionDefinition_key_is_async = async,
      functionDefinition_key_params = pyParams,
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

pprDefinition :: Definition -> Doc Ann
-- empty param case
pprDefinition (Function async name [] returnTy) =
  hcat [
   pprAsync async, "def" <+> pprName name, "()",
   pprReturnType returnTy
  ]
-- full param list
pprDefinition (Function async name params returnTy) =
  vcat [
    nest 4 (vsep (
      hcat [pprAsync async, "def" <> space, pprName name <> lparen] :
      punctuate comma (map pprParam params)
    )),
    rparen <> pprReturnType returnTy
    ]

pprAsync :: AsyncModifier -> Doc Ann
pprAsync Async = "async" <> space
pprAsync _ = emptyDoc

pprReturnType :: ReturnType -> Doc Ann
pprReturnType NoReturnType = emptyDoc
pprReturnType (ReturnType ty xrefs) = space <> "->" <+> pprTypeXRefs ty xrefs

pprParam :: Parameter -> Doc Ann
pprParam (Parameter name _mDefValue mty xrefs) = hcat
  [ pprName name
  , case mty of
    Nothing -> emptyDoc
    Just ty -> colon <+> pprTypeXRefs ty xrefs
  ]

pprName :: Name -> Doc Ann
pprName (Name name) = pretty name

pprTypeXRefs :: PyType -> XRefs -> Doc Ann
pprTypeXRefs (PyType ty) xrefs =
    mconcat $ (\(frag, ann) -> annotate ann $ pretty frag) <$>
      splitString ty spans
  where
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
  result <- searchRecursiveWithLimit maxXRefs (angleDeclsByNames ids)
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
        field @"name" (elementsOf (array (map (asPredicate . factId) names))) $
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
