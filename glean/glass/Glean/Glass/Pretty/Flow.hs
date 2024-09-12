{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications, ApplicativeDo #-}

module Glean.Glass.Pretty.Flow
  (
    prettyFlowSignature
  ) where

import Data.Text ( Text )
import Compat.Prettyprinter

import qualified Glean
import Glean.Angle as Angle
import qualified Glean.Haxl.Repos as Glean
import Glean.Schema.CodeFlow.Types as Flow ( Entity(..) )
import qualified Glean.Schema.Flow.Types as Flow
import Glean.Util.ToAngle ( ToAngle(toAngle) )
import Glean.Glass.Types
import Glean.Glass.Utils ( fetchData )

prettyFlowSignature
  :: LayoutOptions
  -> Flow.Entity
  -> Glean.RepoHaxl u w (Maybe (SimpleDocStream (Maybe SymbolId)))
prettyFlowSignature opts entity = case entity of
    Flow.Entity_decl decl  -> do
      mSig <- fetchData (angleFlowTypeSignature (toAngle decl))
      case mSig of
        Nothing -> return Nothing
        Just sig -> ppr =<< declSignature decl sig
    Flow.Entity_module_ mod -> ppr =<<
      (moduleSignature =<< Glean.keyOf mod)
    Flow.Entity_EMPTY -> return Nothing
  where
    ppr mText = do
      let docStream = layoutSmart opts . pretty <$> mText
      return $ reAnnotateS (const Nothing) <$> docStream

moduleSignature :: Flow.Module_key -> Glean.RepoHaxl u w (Maybe Text)
moduleSignature m = case m of
  Flow.Module_key_file file -> Just <$> Glean.keyOf file
  Flow.Module_key_lib text -> pure (Just ("module " <> text))
  Flow.Module_key_string_ text -> pure (Just ("module " <> text))
  Flow.Module_key_builtin _ -> pure Nothing
  Flow.Module_key_noSource _ -> pure Nothing
  Flow.Module_key_EMPTY -> pure Nothing

declSignature :: Flow.SomeDeclaration -> Text -> Glean.RepoHaxl u w (Maybe Text)
declSignature decl sig = do
  mName <- case decl of
    Flow.SomeDeclaration_localDecl x -> do
      Flow.Declaration_key name _ <- Glean.keyOf x
      Just <$> Glean.keyOf name
    Flow.SomeDeclaration_memberDecl x -> do
      Flow.MemberDeclaration_key name _ <- Glean.keyOf x
      Just <$> Glean.keyOf name
    Flow.SomeDeclaration_typeDecl x -> do
      Flow.TypeDeclaration_key name _ <- Glean.keyOf x
      Just <$> Glean.keyOf name
    Flow.SomeDeclaration_EMPTY -> return Nothing
  return $ (\name -> name <> ": " <> sig ) <$> mName

angleFlowTypeSignature :: Angle Flow.SomeDeclaration -> Angle Text
angleFlowTypeSignature decl = var $ \(sig :: Angle Text) ->
  sig `where_` [
    wild .= predicate @Flow.DeclarationSignature (
      rec $
        field @"decl" decl $
        field @"signature" sig
      end
    )
  ]
