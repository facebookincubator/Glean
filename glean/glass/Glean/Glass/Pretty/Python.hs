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

import Data.Text ( Text, takeWhileEnd )
import Data.Text.Prettyprint.Doc

import qualified Glean
import Glean.Angle as Angle
import qualified Glean.Haxl.Repos as Glean
import Glean.Schema.CodePython.Types as Python ( Entity(..) )
import qualified Glean.Schema.Python.Types as Python
import Glean.Util.ToAngle ( ToAngle(toAngle) )
import Glean.Glass.Types ( SymbolId )
import Glean.Glass.Utils ( fetchDataRecursive )

-- Type of Python entity signatures to capture the the subset we generate
-- We use this to separate the processing of the Glean type from the text
-- we wish to generate

-- Python symbol kinds
data Definition
  = Function !AsyncModifier !Name [Parameter] !(Maybe PyType)
 -- Class
 -- Variable
 -- Import
 -- Module

data Parameter = Parameter !Name (Maybe ExprText) !(Maybe PyType)

-- modifiers
data AsyncModifier = Async | NotAsync deriving (Eq)

newtype ExprText = ExprText Text

-- names
newtype Name = Name Text

newtype PyType = PyType Text

prettyPythonSignature
  :: LayoutOptions
  -> Python.Entity
  -> Glean.RepoHaxl u w (Maybe (SimpleDocStream (Maybe SymbolId)))
prettyPythonSignature opts (Python.Entity_decl decl) = do
  mQuery <- fetchDataRecursive (angleDeclToDef (toAngle decl))
  case mQuery of
    Nothing -> return Nothing
    Just pyDef -> do
      mDef <- fromAngleDefinition pyDef
      return $ case mDef of
        Nothing -> Nothing
        Just def -> Just (doLayout (pprDefinition def))
  where
    doLayout doc = reAnnotateS (const Nothing) (layoutSmart opts doc)

prettyPythonSignature _ Python.Entity_EMPTY = return Nothing

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
    Just tyInfo -> Just <$> fromTypeInfo tyInfo
  nameStr <- Glean.keyOf name
  params <- mapM fromParameter pyParams
  return $ Function
    (if async then Async else NotAsync)
    (trimModule nameStr)
    params
    returnTy
  where
    Python.FunctionDefinition_key {
      functionDefinition_key_declaration = decl,
      functionDefinition_key_is_async = async,
      functionDefinition_key_params = pyParams,
      functionDefinition_key_returnsInfo = mReturnTy
    } = def

fromTypeInfo  :: Python.TypeInfo -> Glean.RepoHaxl u w PyType
fromTypeInfo Python.TypeInfo{..} = PyType <$> Glean.keyOf
  typeInfo_displayType

fromParameter :: Python.Parameter -> Glean.RepoHaxl u w Parameter
fromParameter Python.Parameter{..} = do
  nameStr <- Glean.keyOf parameter_name
  tyInfo <- case parameter_typeInfo of
    Nothing -> return Nothing
    Just ty -> Just <$> fromTypeInfo ty
  return $ Parameter (Name nameStr) (ExprText <$> parameter_value) tyInfo

pprDefinition :: Definition -> Doc ()
-- empty param case
pprDefinition (Function async name [] returnTy) =
  hcat [
   pprAsync async, "def" <+> pprName name, "()",
   case returnTy of
      Nothing -> emptyDoc
      Just ty -> space <> "->" <+> pprType ty
  ]
-- full param list
pprDefinition (Function async name params returnTy) =
  vcat [
    nest 4 (vsep (
      hcat [pprAsync async, "def" <> space, pprName name <> lparen] :
      punctuate comma (map pprParam params)
    )),
    rparen <+> case returnTy of
      Nothing -> emptyDoc
      Just ty -> "->" <+> pprType ty
    ]

pprAsync :: AsyncModifier -> Doc ()
pprAsync Async = "async" <> space
pprAsync _ = emptyDoc

pprParam :: Parameter -> Doc ()
pprParam (Parameter name _mDefValue mty) = hcat
  [ pprName name
  , case mty of
    Nothing -> emptyDoc
    Just ty -> colon <+> pprType ty
  ]

pprName :: Name -> Doc ()
pprName (Name name) = pretty name

pprType :: PyType -> Doc ()
pprType (PyType ty) = pretty ty

angleDeclToDef:: Angle Python.Declaration -> Angle Python.Definition
angleDeclToDef decl = var $ \(def :: Angle Python.Definition) ->
  def `where_` [
    wild .= predicate @Python.DeclarationDefinition (
      rec $
        field @"declaration" decl $
        field @"definition" def
      end
    )
  ]

-- | we could use the sname here to lookup the associated decl fact
-- as an xref in the type signature (c.f how Hack does this)
trimModule :: Text -> Name
trimModule qname = Name (takeWhileEnd (/= '.') qname)
