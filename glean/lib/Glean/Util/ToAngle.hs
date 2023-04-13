{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
module Glean.Util.ToAngle
  ( ToAngle(..)
  ) where

import Glean
import Glean.Angle

import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.Erlang.Types as Erlang
import qualified Glean.Schema.Flow.Types as Flow
import qualified Glean.Schema.Hack.Types as Hack
import qualified Glean.Schema.Lsif.Types as Lsif
import qualified Glean.Schema.Python.Types as Py
import qualified Glean.Schema.Thrift.Types as Thrift

import qualified Glean.Schema.CodeCxx.Types as Cxx
import qualified Glean.Schema.CodeBuck.Types as Buck
import qualified Glean.Schema.CodeFlow.Types as Flow
import qualified Glean.Schema.CodeHs.Types as Hs

-- | Convert a value to a query for that value. Useful when we want to
-- use a result we got back from a query in another query.
--
-- Note that the query will be shallow and will use fact IDs instead
-- of matching by structure, so the resulting query only works on the
-- same DB that the value was obtained from.

class ToAngle a where
  toAngle :: a -> Angle a

-- | Generically get an Angle key query
mkKey :: Glean.Predicate p => p -> Angle (Glean.KeyType p)
mkKey x = asPredicate (factId (Glean.getId x))

-- Cxx

instance ToAngle Cxx.Entity where
  toAngle e = case e of
    Cxx.Entity_decl x -> alt @"decl" (toAngle x)
    Cxx.Entity_defn x -> alt @"defn" (toAngle x)
    Cxx.Entity_enumerator x -> alt @"enumerator" (mkKey x)
    Cxx.Entity_EMPTY -> error "unknown Entity"

instance ToAngle Cxx.Declaration where
  toAngle e = case e of
    Cxx.Declaration_namespace_ x -> alt @"namespace_" (mkKey x)
    Cxx.Declaration_usingDeclaration x -> alt @"usingDeclaration" (mkKey x)
    Cxx.Declaration_usingDirective x -> alt @"usingDirective" (mkKey x)
    Cxx.Declaration_record_ x -> alt @"record_" (mkKey x)
    Cxx.Declaration_enum_ x -> alt @"enum_" (mkKey x)
    Cxx.Declaration_function_ x -> alt @"function_" (mkKey x)
    Cxx.Declaration_variable x -> alt @"variable" (mkKey x)
    Cxx.Declaration_objcContainer x -> alt @"objcContainer" (mkKey x)
    Cxx.Declaration_objcMethod x -> alt @"objcMethod" (mkKey x)
    Cxx.Declaration_objcProperty x -> alt @"objcProperty" (mkKey x)
    Cxx.Declaration_typeAlias x -> alt @"typeAlias" (mkKey x)
    Cxx.Declaration_EMPTY -> error "unknown Declaration"

instance ToAngle Cxx.Definition where
  toAngle e = case e of
    Cxx.Definition_record_ x -> alt @"record_" (mkKey x)
    Cxx.Definition_function_ x -> alt @"function_" (mkKey x)
    Cxx.Definition_enum_ x -> alt @"enum_" (mkKey x)
    Cxx.Definition_objcMethod x -> alt @"objcMethod" (mkKey x)
    Cxx.Definition_objcContainer x -> alt @"objcContainer" (mkKey x)
    Cxx.Definition_variable x -> alt @"variable" (mkKey x)
    Cxx.Definition_namespace_ x -> alt @"namespace_" (mkKey x)
    Cxx.Definition_EMPTY -> error "unknown Definition"

-- Erlang

instance ToAngle Erlang.Declaration where
  toAngle d = case d of
    Erlang.Declaration_func x -> alt @"func" (mkKey x)
    Erlang.Declaration_EMPTY -> error "unknown Declaration"

-- Buck

instance ToAngle Buck.Entity where
  toAngle e = case e of
    Buck.Entity_locator x -> alt @"locator" (mkKey x)
    Buck.Entity_file x -> alt @"file" (mkKey x)
    Buck.Entity_definition x -> alt @"definition" (mkKey x)
    Buck.Entity_EMPTY -> error "unknown entity"

-- Flow

instance ToAngle Flow.Entity where
  toAngle e = case e of
    Flow.Entity_decl x -> alt @"decl" (toAngle x)
    Flow.Entity_module_ x -> alt @"module_" (mkKey x)
    Flow.Entity_EMPTY -> error "unknown Entity"

instance ToAngle Flow.SomeDeclaration where
  toAngle e = case e of
    Flow.SomeDeclaration_localDecl x -> alt @"localDecl" (mkKey x)
    Flow.SomeDeclaration_memberDecl x -> alt @"memberDecl" (mkKey x)
    Flow.SomeDeclaration_typeDecl x -> alt @"typeDecl" (mkKey x)
    Flow.SomeDeclaration_EMPTY -> error "unknown SomeDeclaration"

-- Hack

instance ToAngle Hack.Declaration where
  toAngle e = case e of
    Hack.Declaration_classConst x -> alt @"classConst" (mkKey x)
    Hack.Declaration_container x -> alt @"container" (toAngle x)
    Hack.Declaration_enumerator x -> alt @"enumerator" (mkKey x)
    Hack.Declaration_function_ x -> alt @"function_" (mkKey x)
    Hack.Declaration_globalConst x -> alt @"globalConst" (mkKey x)
    Hack.Declaration_method x -> alt @"method" (mkKey x)
    Hack.Declaration_module x -> alt @"module" (mkKey x)
    Hack.Declaration_namespace_ x -> alt @"namespace_" (mkKey x)
    Hack.Declaration_property_ x -> alt @"property_" (mkKey x)
    Hack.Declaration_typeConst x -> alt @"typeConst" (mkKey x)
    Hack.Declaration_typedef_ x -> alt @"typedef_" (mkKey x)
    Hack.Declaration_EMPTY -> error "unknown Declaration"

instance ToAngle Hack.ContainerDeclaration where
  toAngle e = case e of
    Hack.ContainerDeclaration_class_ x -> alt @"class_" (mkKey x)
    Hack.ContainerDeclaration_enum_ x -> alt @"enum_" (mkKey x)
    Hack.ContainerDeclaration_interface_ x -> alt @"interface_" (mkKey x)
    Hack.ContainerDeclaration_trait x -> alt @"trait" (mkKey x)
    Hack.ContainerDeclaration_EMPTY -> error "unknown ContainerDeclaration"

-- Haskell

instance ToAngle Hs.Entity where
  toAngle e = case e of
    Hs.Entity_class_ x -> alt @"class_" (mkKey x)
    Hs.Entity_definition x -> alt @"definition" (mkKey x)
    Hs.Entity_function_ x -> alt @"function_" (mkKey x)
    Hs.Entity_EMPTY -> error "unknown Entity"

-- Python

instance ToAngle Py.Declaration where
  toAngle (Py.Declaration_cls x) = alt @"cls" (mkKey x)
  toAngle (Py.Declaration_func x) = alt @"func" (mkKey x)
  toAngle (Py.Declaration_module x) = alt @"module" (mkKey x)
  toAngle (Py.Declaration_variable x) = alt @"variable" (mkKey x)
  toAngle (Py.Declaration_imp x) = alt @"imp" (mkKey x)
  toAngle Py.Declaration_EMPTY = error "unknown Declaration"

-- Thrift

instance ToAngle Thrift.XRefTarget where
  toAngle (Thrift.XRefTarget_include_ x) = alt @"include_" (mkKey x)
  toAngle (Thrift.XRefTarget_named x) = alt @"named" (mkKey x)
  toAngle (Thrift.XRefTarget_exception_ x) = alt @"exception_" (mkKey x)
  toAngle (Thrift.XRefTarget_service_ x) = alt @"service_" (mkKey x)
  toAngle (Thrift.XRefTarget_constant x) = alt @"constant" (mkKey x)
  toAngle (Thrift.XRefTarget_enumValue x) = alt @"enumValue" (mkKey x)
  toAngle (Thrift.XRefTarget_function_ x) = alt @"function_" (mkKey x)
  toAngle Thrift.XRefTarget_EMPTY = error "unknown Entity"

-- LSIF languages

instance ToAngle Lsif.SomeEntity where
  toAngle e = case e of
    Lsif.SomeEntity_defn x -> alt @"defn" (mkKey x)
    Lsif.SomeEntity_decl x -> alt @"decl" (mkKey x)
    Lsif.SomeEntity_EMPTY -> error "unknown Lsif.SomeEntity"
