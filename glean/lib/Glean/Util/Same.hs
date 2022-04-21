{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Glean.Util.Same
  ( queryDeclFamily
  , DeclToDef
  ) where

import Glean
import Glean.Angle hiding (query)
import qualified Glean.Angle as Angle
import qualified Glean.Schema.Cxx1.Types as Cxx
import Glean.Util.ToAngle

-- | Use 'Cxx.DeclToFamily' to find the same facts (shallow query)
--
-- Always returns non-empty list, in same order as in 'Cxx.DeclFamily' fact
queryDeclFamily :: Cxx.Declaration -> Haxl w [Cxx.Declaration]
queryDeclFamily decl = do
  fams <- search_ $ Angle.query $
    predicate @Cxx.DeclToFamily $
      rec $ field @"decl" (toAngle decl) end
  case fams of
    [] -> return [decl]
    (Cxx.DeclToFamily _ (Just key):_) ->
      getKey (Cxx.declToFamily_key_family key)
    _ -> error ("queryDeclFamily: " ++ show decl)

-- | Map to Cxx1 definition type, or @p@ if it does not exist. Injective
type family DeclToDef p = d | d -> p

-- main DeclToDef types
type instance DeclToDef Cxx.RecordDeclaration = Cxx.RecordDefinition
type instance DeclToDef Cxx.EnumDeclaration = Cxx.EnumDefinition
type instance DeclToDef Cxx.FunctionDeclaration = Cxx.FunctionDefinition
type instance DeclToDef Cxx.ObjcContainerDeclaration =
  Cxx.ObjcContainerDefinition
type instance DeclToDef Cxx.ObjcMethodDeclaration = Cxx.ObjcMethodDefinition
type instance DeclToDef Cxx.NamespaceDeclaration = Cxx.NamespaceDefinition
-- some variables are global
type instance DeclToDef Cxx.VariableDeclaration = Cxx.VariableDeclaration
-- questionable DeclToDef type
type instance DeclToDef Cxx.ObjcPropertyDeclaration =
  Cxx.ObjcPropertyImplementation
-- identity DeclToDef x = x, to make extra branches compile
type instance DeclToDef Cxx.UsingDeclaration = Cxx.UsingDeclaration
type instance DeclToDef Cxx.UsingDirective = Cxx.UsingDirective
type instance DeclToDef Cxx.TypeAliasDeclaration =
  Cxx.TypeAliasDeclaration
