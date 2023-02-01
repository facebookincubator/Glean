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
import Glean.Glass.Utils (fetchDataRecursive )

prettyPythonSignature
  :: LayoutOptions
  -> Python.Entity
  -> Glean.RepoHaxl u w (Maybe (SimpleDocStream (Maybe SymbolId)))
prettyPythonSignature opts (Python.Entity_decl decl) = do
  mDef <- fetchDataRecursive (anglePythonDeclDef (toAngle decl))
  case mDef of
    Nothing -> return Nothing
    Just def -> ppr =<< prettyDef def
  where
    ppr mText = do
      let docStream = layoutSmart opts . pretty <$> mText
      return $ reAnnotateS (const Nothing) <$> docStream

prettyPythonSignature _ Python.Entity_EMPTY = return Nothing

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
prettyDef (Python.Definition_func def) = do
  Python.FunctionDefinition_key {
    functionDefinition_key_declaration = decl,
    functionDefinition_key_is_async = async
   }  <- Glean.keyOf def
  Python.FunctionDeclaration_key name  <- Glean.keyOf decl
  Just <$>
    (if async then "async " else "") <>
    "def " <> (trimModule <$> Glean.keyOf name) <>
    "(...) -> ...:"

prettyDef Python.Definition_cls{} = return Nothing
prettyDef Python.Definition_variable{} = return Nothing
prettyDef Python.Definition_module{} = return Nothing
prettyDef Python.Definition_EMPTY = return Nothing

-- | we could use the sname here to lookup the associated decl fact
-- as an xref in the type signature (c.f how Hack does this)
trimModule :: Text -> Text
trimModule qname = takeWhileEnd (/= '.') qname
