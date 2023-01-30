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

import Data.Text ( Text, takeWhileEnd, intercalate )
import Data.Text.Prettyprint.Doc

import qualified Glean
import Glean.Angle as Angle
import qualified Glean.Haxl.Repos as Glean
import Glean.Schema.CodePython.Types as Python ( Entity(..) )
import qualified Glean.Schema.Python.Types as Python
import Glean.Util.ToAngle ( ToAngle(toAngle) )
import Glean.Glass.Types
import Glean.Glass.Utils (fetchDataRecursive )

prettyPythonSignature
  :: LayoutOptions
  -> Python.Entity
  -> Glean.RepoHaxl u w (Maybe (SimpleDocStream (Maybe SymbolId)))
prettyPythonSignature opts entity = case entity of
    Python.Entity_decl decl  -> do
      mDef <- fetchDataRecursive (anglePythonDeclDef (toAngle decl))
      case mDef of
        Nothing -> return Nothing
        Just def -> ppr =<< prettyDef def
    Python.Entity_EMPTY -> return Nothing
  where
    ppr mText = do
      let docStream = layoutSmart opts . pretty <$> mText
      return $ reAnnotateS (const Nothing) <$> docStream


anglePythonDeclDef :: Angle Python.Declaration -> Angle Python.Definition
anglePythonDeclDef decl = var $ \(def :: Angle Python.Definition) ->
  def `where_` [
    wild .= predicate @Python.DeclarationDefinition (
      rec $
        field @"declaration" decl $
        field @"definition" def
      end
    )
  ]

prettyDef :: Python.Definition -> Glean.RepoHaxl u w (Maybe Text)
prettyDef def = do
  case def of
    Python.Definition_func x -> do
      Python.FunctionDefinition_key {
        functionDefinition_key_declaration = decl,
        functionDefinition_key_is_async = async,
        functionDefinition_key_decorators = decors
      }  <- Glean.keyOf x
      Python.FunctionDeclaration_key name  <- Glean.keyOf decl
      Just <$>
        pure (fmtDecorators decors) <>
        (if async then "async " else "") <>
        "def " <> (trimModule <$> Glean.keyOf name) <>
        "(...) -> ...:"
    Python.Definition_EMPTY -> return Nothing
    _ -> return Nothing

trimModule :: Text -> Text
trimModule qname = takeWhileEnd (/='.') qname

fmtDecorators :: Maybe [Python.Decorator] -> Text
fmtDecorators mDecors =
  case mDecors of
    Nothing -> ""
    Just decors -> intercalate "\n" decors <> "\n"
