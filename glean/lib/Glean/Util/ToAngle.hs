{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
module Glean.Util.ToAngle
  ( ToAngle(..)
  , ToAngleFull(..)
  , Normalize(..)
  , Prune(..)
  ) where

import Glean
import Glean.Angle

import qualified Glean.Schema.Src.Types as Src
import qualified Glean.Schema.Csharp.Types as CSharp
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.Erlang.Types as Erlang
import qualified Glean.Schema.Fbthrift.Types as Fbthrift
import qualified Glean.Schema.Flow.Types as Flow
import qualified Glean.Schema.Graphql.Types as GraphQL
import qualified Glean.Schema.Hack.Types as Hack
import qualified Glean.Schema.JavaAlpha.Types as Java
import qualified Glean.Schema.KotlinAlpha.Types as Kotlin
import qualified Glean.Schema.Lsif.Types as Lsif
import qualified Glean.Schema.Python.Types as Py
import qualified Glean.Schema.Scip.Types as Scip

import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.CodeHack.Types as Hack
import qualified Glean.Schema.CodePython.Types as Py
import qualified Glean.Schema.CodeCxx.Types as Cxx
import qualified Glean.Schema.CodeGraphql.Types as GraphQL
import qualified Glean.Schema.CodePp.Types as Pp
import qualified Glean.Schema.CodeBuck.Types as Buck
import qualified Glean.Schema.CodeFlow.Types as Flow
import qualified Glean.Schema.CodeFbthrift.Types as Fbthrift
import qualified Glean.Schema.CodeHs.Types as Hs

-- | Convert a value to a query for that value. Useful when we want to
-- use a result we got back from a query in another query.
--
-- toAngle returns a shallow query and will use fact IDs instead
-- of matching by structure, so the resulting query only works on the
-- same DB that the value was obtained from.
--
-- toAngleFull returns deep queries, provided the parameter is fully
-- resolved. Useful for querying a different db from the one the value
-- was obtained.
--
-- normalize returns a canonical representation of Glean fact where
-- identifiers are replaced with 0. Useful for comparing facts generated
-- from different dbs, as long as the facts are fully computed (all keys
-- are present).

class ToAngle a where
  toAngle :: a -> Angle a

class ToAngleFull a where
  toAngleFull :: a -> Angle a

class Normalize a where
  normalize :: a -> a

-- | Prune keys, leaving only fact IDs. Useful for hashing values cheaply.
class Prune a where
  prune :: a -> a

instance Prune Code.Entity where
  prune e = case e of
    Code.Entity_hack (Hack.Entity_decl x) ->
      Code.Entity_hack (Hack.Entity_decl (prune x))
    Code.Entity_python (Py.Entity_decl x) ->
      Code.Entity_python (Py.Entity_decl (prune x))
    Code.Entity_cxx x ->
      Code.Entity_cxx (prune x)
    x -> x

-- | Generically get an Angle key query
mkKey :: Glean.Predicate p => p -> Angle (Glean.KeyType p)
mkKey x = asPredicate (factId (Glean.getId x))

-- C pre-processor

instance ToAngle Pp.Entity where
  toAngle e = case e of
    Pp.Entity_define x -> alt @"define" (mkKey x)
    Pp.Entity_undef x -> alt @"undef" (mkKey x)
    Pp.Entity_include_ x -> alt @"include_" (mkKey x)
    Pp.Entity_EMPTY -> error "unknown code.pp.Entity"

-- Cxx

instance ToAngle Cxx.Entity where
  toAngle e = case e of
    Cxx.Entity_decl x -> alt @"decl" (toAngle x)
    Cxx.Entity_defn x -> alt @"defn" (toAngle x)
    Cxx.Entity_enumerator x -> alt @"enumerator" (mkKey x)
    Cxx.Entity_objcSelectorSlot x -> alt @"objcSelectorSlot" (toAngle x)
    Cxx.Entity_EMPTY -> error "unknown Entity"

instance Prune Cxx.Entity where
  prune e = case e of
    Cxx.Entity_decl x ->
      Cxx.Entity_decl (prune x)
    Cxx.Entity_defn x ->
      Cxx.Entity_defn (prune x)
    Cxx.Entity_enumerator x ->
      Cxx.Entity_enumerator (mkFact (getId x) Nothing Nothing)
    Cxx.Entity_objcSelectorSlot x ->
      Cxx.Entity_objcSelectorSlot x
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
    Cxx.Declaration_namespaceAlias x -> alt @"namespaceAlias" (mkKey x)
    Cxx.Declaration_EMPTY -> error "unknown Declaration"

instance Prune Cxx.Declaration where
  prune e = case e of
    Cxx.Declaration_namespace_ x ->
      Cxx.Declaration_namespace_ (mkFact (getId x) Nothing Nothing)
    Cxx.Declaration_usingDeclaration x ->
      Cxx.Declaration_usingDeclaration (mkFact (getId x) Nothing Nothing)
    Cxx.Declaration_usingDirective x ->
      Cxx.Declaration_usingDirective (mkFact (getId x) Nothing Nothing)
    Cxx.Declaration_record_ x ->
      Cxx.Declaration_record_ (mkFact (getId x) Nothing Nothing)
    Cxx.Declaration_enum_ x ->
      Cxx.Declaration_enum_ (mkFact (getId x) Nothing Nothing)
    Cxx.Declaration_function_ x ->
      Cxx.Declaration_function_ (mkFact (getId x) Nothing Nothing)
    Cxx.Declaration_variable x ->
      Cxx.Declaration_variable (mkFact (getId x) Nothing Nothing)
    Cxx.Declaration_objcContainer x ->
      Cxx.Declaration_objcContainer (mkFact (getId x) Nothing Nothing)
    Cxx.Declaration_objcMethod x ->
      Cxx.Declaration_objcMethod (mkFact (getId x) Nothing Nothing)
    Cxx.Declaration_objcProperty x ->
      Cxx.Declaration_objcProperty (mkFact (getId x) Nothing Nothing)
    Cxx.Declaration_typeAlias x ->
      Cxx.Declaration_typeAlias (mkFact (getId x) Nothing Nothing)
    Cxx.Declaration_namespaceAlias x ->
      Cxx.Declaration_namespaceAlias (mkFact (getId x) Nothing Nothing)
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

instance Prune Cxx.Definition where
  prune e = case e of
    Cxx.Definition_record_ x ->
      Cxx.Definition_record_ (mkFact (getId x) Nothing Nothing)
    Cxx.Definition_function_ x ->
      Cxx.Definition_function_ (mkFact (getId x) Nothing Nothing)
    Cxx.Definition_enum_ x ->
      Cxx.Definition_enum_ (mkFact (getId x) Nothing Nothing)
    Cxx.Definition_objcMethod x ->
      Cxx.Definition_objcMethod (mkFact (getId x) Nothing Nothing)
    Cxx.Definition_objcContainer x ->
      Cxx.Definition_objcContainer (mkFact (getId x) Nothing Nothing)
    Cxx.Definition_variable x ->
      Cxx.Definition_variable (mkFact (getId x) Nothing Nothing)
    Cxx.Definition_namespace_ x ->
      Cxx.Definition_namespace_ (mkFact (getId x) Nothing Nothing)
    Cxx.Definition_EMPTY -> error "unknown Definition"

instance ToAngle Cxx.ObjcMethodEntity where
  toAngle e = case e of
    Cxx.ObjcMethodEntity_decl x -> alt @"decl" (mkKey x)
    Cxx.ObjcMethodEntity_defn x -> alt @"defn" (mkKey x)
    Cxx.ObjcMethodEntity_EMPTY -> error "unknown ObjcMethodEntity"

instance ToAngle Cxx.ObjcSelectorSlotEntity where
  toAngle (Cxx.ObjcSelectorSlotEntity method idx) =
    rec $
      field @"objcMethod" (toAngle method) $
      field @"index" (nat $ fromNat idx)
    end

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

-- GraphQL

instance ToAngle GraphQL.Entity where
  toAngle e = case e of
    GraphQL.Entity_decl x -> alt @"decl" (toAngle x)
    GraphQL.Entity_EMPTY -> error "unknown Entity"

instance ToAngle GraphQL.Declaration where
  toAngle e = case e of
    GraphQL.Declaration_operation_ x -> alt @"operation_" (mkKey x)
    GraphQL.Declaration_fragment_ x -> alt @"fragment_" (mkKey x)
    GraphQL.Declaration_field_ x -> alt @"field_" (mkKey x)
    GraphQL.Declaration_enum_ x -> alt @"enum_" (mkKey x)
    GraphQL.Declaration_directive_ x -> alt @"directive_" (mkKey x)
    GraphQL.Declaration_EMPTY -> error "unknown GraphQL.Declaration"

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

instance Prune Hack.Declaration where
  prune d = case d of
    Hack.Declaration_classConst x ->
      Hack.Declaration_classConst (mkFact (getId x) Nothing Nothing)
    Hack.Declaration_container x ->
      Hack.Declaration_container (prune x)
    Hack.Declaration_enumerator x ->
      Hack.Declaration_enumerator (mkFact (getId x) Nothing Nothing)
    Hack.Declaration_function_ x ->
      Hack.Declaration_function_ (mkFact (getId x) Nothing Nothing)
    Hack.Declaration_globalConst x ->
      Hack.Declaration_globalConst (mkFact (getId x) Nothing Nothing)
    Hack.Declaration_method x ->
      Hack.Declaration_method (mkFact (getId x) Nothing Nothing)
    Hack.Declaration_module x ->
      Hack.Declaration_module (mkFact (getId x) Nothing Nothing)
    Hack.Declaration_namespace_ x ->
      Hack.Declaration_namespace_ (mkFact (getId x) Nothing Nothing)
    Hack.Declaration_property_ x ->
      Hack.Declaration_property_ (mkFact (getId x) Nothing Nothing)
    Hack.Declaration_typeConst x ->
      Hack.Declaration_typeConst (mkFact (getId x) Nothing Nothing)
    Hack.Declaration_typedef_ x ->
      Hack.Declaration_typedef_ (mkFact (getId x) Nothing Nothing)
    Hack.Declaration_EMPTY -> error "unknown Declaration"

instance ToAngle Hack.ContainerDeclaration where
  toAngle e = case e of
    Hack.ContainerDeclaration_class_ x -> alt @"class_" (mkKey x)
    Hack.ContainerDeclaration_enum_ x -> alt @"enum_" (mkKey x)
    Hack.ContainerDeclaration_interface_ x -> alt @"interface_" (mkKey x)
    Hack.ContainerDeclaration_trait x -> alt @"trait" (mkKey x)
    Hack.ContainerDeclaration_EMPTY -> error "unknown ContainerDeclaration"

instance Prune Hack.ContainerDeclaration where
  prune e = case e of
    Hack.ContainerDeclaration_class_ x ->
      Hack.ContainerDeclaration_class_ (mkFact (getId x) Nothing Nothing)
    Hack.ContainerDeclaration_enum_ x ->
      Hack.ContainerDeclaration_enum_ (mkFact (getId x) Nothing Nothing)
    Hack.ContainerDeclaration_interface_ x ->
      Hack.ContainerDeclaration_interface_ (mkFact (getId x) Nothing Nothing)
    Hack.ContainerDeclaration_trait x ->
      Hack.ContainerDeclaration_trait (mkFact (getId x) Nothing Nothing)
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

instance Prune Py.Declaration where
  prune (Py.Declaration_cls x) =
    Py.Declaration_cls (mkFact (getId x) Nothing Nothing)
  prune (Py.Declaration_func x) =
    Py.Declaration_func (mkFact (getId x) Nothing Nothing)
  prune (Py.Declaration_module x) =
    Py.Declaration_module (mkFact (getId x) Nothing Nothing)
  prune (Py.Declaration_variable x) =
    Py.Declaration_variable (mkFact (getId x) Nothing Nothing)
  prune (Py.Declaration_imp x) =
    Py.Declaration_imp (mkFact (getId x) Nothing Nothing)
  prune Py.Declaration_EMPTY = error "unknown Declaration"

-- Src

instance Normalize Src.File where
  normalize (Src.File _ (Just k)) = Src.File 0 (Just k)
  normalize  _ = error "Not fully resolved"

instance ToAngleFull Src.File where
  toAngleFull (Src.File _ (Just k)) = predicate $ string k
  toAngleFull  _ = error "Not fully resolved"

-- Fbthrift

instance Normalize Fbthrift.File where
  normalize (Fbthrift.File _ (Just k)) = Fbthrift.File 0 (Just (normalize k))
  normalize  _ = error "Not fully resolved"

instance Normalize Fbthrift.QualName_key where
  normalize
    (Fbthrift.QualName_key (Fbthrift.File _ (Just file))
    (Fbthrift.Identifier _ (Just identifier))) =
    Fbthrift.QualName_key (Fbthrift.File 0 (Just (normalize file)))
    (Fbthrift.Identifier 0 (Just identifier))
  normalize  _ = error "Not fully resolved"

instance Normalize Fbthrift.ExceptionName_key where
  normalize (Fbthrift.ExceptionName_key (Fbthrift.QualName _ (Just qualname)))
    =
    Fbthrift.ExceptionName_key (Fbthrift.QualName 0 (Just (normalize qualname)))
  normalize  _ = error "Not fully resolved"

instance Normalize Fbthrift.ServiceName_key where
  normalize (Fbthrift.ServiceName_key (Fbthrift.QualName _ (Just qualname)))
    =
    Fbthrift.ServiceName_key (Fbthrift.QualName 0 (Just (normalize qualname)))
  normalize  _ = error "Not fully resolved"

instance Normalize Fbthrift.NamedDecl_key where
  normalize
    (Fbthrift.NamedDecl_key
      (Fbthrift.NamedType (Fbthrift.QualName _ (Just qualname)) kind)) =
    Fbthrift.NamedDecl_key
      (Fbthrift.NamedType
        (Fbthrift.QualName 0 (Just (normalize qualname))) kind)
  normalize  _ = error "Not fully resolved"

instance Normalize Fbthrift.FunctionName_key where
  normalize
    (Fbthrift.FunctionName_key (Fbthrift.ServiceName _ (Just service_))
      (Fbthrift.Identifier _ (Just identifier))) =
    Fbthrift.FunctionName_key (Fbthrift.ServiceName 0
      (Just (normalize service_))) (Fbthrift.Identifier 0 (Just identifier))
  normalize  _ = error "Not fully resolved"

instance Normalize Fbthrift.XRefTarget where
  normalize
    (Fbthrift.XRefTarget_function_
      (Fbthrift.FunctionName _ (Just x))) =
    Fbthrift.XRefTarget_function_ (Fbthrift.FunctionName 0 (Just (normalize x)))
  normalize (Fbthrift.XRefTarget_service_
      (Fbthrift.ServiceName _ (Just x))) =
    Fbthrift.XRefTarget_service_ (Fbthrift.ServiceName 0 (Just (normalize x)))
  normalize (Fbthrift.XRefTarget_exception_
      (Fbthrift.ExceptionName _ (Just x))) =
    Fbthrift.XRefTarget_exception_
      (Fbthrift.ExceptionName 0 (Just (normalize x)))
  normalize (Fbthrift.XRefTarget_named
      (Fbthrift.NamedDecl _ (Just x))) =
    Fbthrift.XRefTarget_named (Fbthrift.NamedDecl 0 (Just (normalize x)))
  normalize _ = error "unknown Entity"

instance ToAngleFull Fbthrift.File where
  toAngleFull (Fbthrift.File _ (Just k)) = predicate $ toAngleFull k
  toAngleFull  _ = error "Not Fully resolved"

instance ToAngleFull Fbthrift.QualName_key where
  toAngleFull
    (Fbthrift.QualName_key (Fbthrift.File _ (Just file))
    (Fbthrift.Identifier _ (Just identifier))) = rec $
        field @"file" (sig (toAngleFull file)) $
        field @"name" (string identifier)
    end
  toAngleFull  _ = error "Not Fully resolved"

instance ToAngleFull Fbthrift.ServiceName_key where
  toAngleFull (Fbthrift.ServiceName_key (Fbthrift.QualName _ (Just qualname)))
    = rec $
        field @"name" (toAngleFull qualname)
    end
  toAngleFull  _ = error "Not Fully resolved"

instance ToAngleFull Fbthrift.NamedDecl_key where
  toAngleFull
    (Fbthrift.NamedDecl_key
      (Fbthrift.NamedType (Fbthrift.QualName _ (Just qualname)) kind)) =
      let namedType :: Angle Fbthrift.NamedType = rec $
            field @"name" (toAngleFull qualname) $
            field @"kind" (enum kind)
            end
      in
      rec $
        field @"name" namedType
    end
  toAngleFull  _ = error "Not fully resolved"

instance ToAngleFull Fbthrift.ExceptionName_key where
  toAngleFull (Fbthrift.ExceptionName_key (Fbthrift.QualName _ (Just qualname)))
    = rec $
        field @"name" (toAngleFull qualname)
    end
  toAngleFull  _ = error "Not Fully resolved"

instance ToAngleFull Fbthrift.FunctionName_key where
  toAngleFull
    (Fbthrift.FunctionName_key (Fbthrift.ServiceName _ (Just service_))
    (Fbthrift.Identifier _ (Just identifier))) = rec $
      field @"service_" (toAngleFull service_) $
      field @"name" (string identifier)
    end
  toAngleFull  _ = error "Not Fully resolved"

instance ToAngleFull Fbthrift.XRefTarget where
  toAngleFull
    (Fbthrift.XRefTarget_function_ (Fbthrift.FunctionName _ (Just x))) =
    alt @"function_" (toAngleFull x)
  toAngleFull
    (Fbthrift.XRefTarget_service_ (Fbthrift.ServiceName _ (Just x))) =
    alt @"service_" (toAngleFull x)
  toAngleFull
    (Fbthrift.XRefTarget_exception_ (Fbthrift.ExceptionName _ (Just x))) =
    alt @"exception_" (toAngleFull x)
  toAngleFull
    (Fbthrift.XRefTarget_named (Fbthrift.NamedDecl _ (Just x))) =
    alt @"named" (toAngleFull x)
  toAngleFull _ = error "unknown Entity"

instance ToAngle Fbthrift.XRefTarget where
  toAngle (Fbthrift.XRefTarget_include_ x) = alt @"include_" (mkKey x)
  toAngle (Fbthrift.XRefTarget_named x) = alt @"named" (mkKey x)
  toAngle (Fbthrift.XRefTarget_exception_ x) = alt @"exception_" (mkKey x)
  toAngle (Fbthrift.XRefTarget_service_ x) = alt @"service_" (mkKey x)
  toAngle (Fbthrift.XRefTarget_constant x) = alt @"constant" (mkKey x)
  toAngle (Fbthrift.XRefTarget_enumValue x) = alt @"enumValue" (mkKey x)
  toAngle (Fbthrift.XRefTarget_function_ x) = alt @"function_" (mkKey x)
  toAngle (Fbthrift.XRefTarget_field x) = alt @"field" (mkKey x)
  toAngle Fbthrift.XRefTarget_EMPTY = error "unknown Entity"

-- Java

instance ToAngle Java.Declaration where
  toAngle (Java.Declaration_class_ x) = alt @"class_" (mkKey x)
  toAngle (Java.Declaration_interface_ x) = alt @"interface_" (mkKey x)
  toAngle (Java.Declaration_enum_ x) = alt @"enum_" (mkKey x)
  toAngle (Java.Declaration_method x) = alt @"method" (mkKey x)
  toAngle (Java.Declaration_ctor x) = alt @"ctor" (mkKey x)
  toAngle (Java.Declaration_field x) = alt @"field" (mkKey x)
  toAngle (Java.Declaration_param x) = alt @"param" (mkKey x)
  toAngle (Java.Declaration_local x) = alt @"local" (mkKey x)
  toAngle Java.Declaration_EMPTY = error "unknown Declaration"

-- Kotlin

instance ToAngle Kotlin.Declaration where
  toAngle (Kotlin.Declaration_class_ x) = alt @"class_" (mkKey x)
  toAngle (Kotlin.Declaration_method x) = alt @"method" (mkKey x)
  toAngle (Kotlin.Declaration_variable x) = alt @"variable" (mkKey x)
  toAngle Kotlin.Declaration_EMPTY = error "unknown Declaration"

-- C#

instance ToAngle CSharp.Definition where
  toAngle (CSharp.Definition_type atype) = alt @"type" (toAngle atype)
  toAngle (CSharp.Definition_method x) = alt @"method" (mkKey x)
  toAngle (CSharp.Definition_field x) = alt @"field" (mkKey x)
  toAngle (CSharp.Definition_parameter x) = alt @"parameter" (mkKey x)
  toAngle (CSharp.Definition_typeParameter x) = alt @"typeParameter" (mkKey x)
  toAngle (CSharp.Definition_local x) = alt @"local" (mkKey x)
  toAngle (CSharp.Definition_property x) = alt @"property" (mkKey x)
  toAngle CSharp.Definition_EMPTY = error "unknown Definition"

instance ToAngle CSharp.AType where
  toAngle (CSharp.AType_arrayType x) = alt @"arrayType" (mkKey x)
  toAngle (CSharp.AType_namedType x) = alt @"namedType" (toAngle x)
  toAngle (CSharp.AType_functionPointerType x) =
    alt @"functionPointerType" (mkKey x)
  toAngle (CSharp.AType_pointerType x) = alt @"pointerType" (mkKey x)
  toAngle (CSharp.AType_typeParameter x) = alt @"typeParameter" (mkKey x)
  toAngle CSharp.AType_EMPTY = error "unknown AType"

instance ToAngle CSharp.NamedType where
  toAngle (CSharp.NamedType_class_ x) = alt @"class_" (mkKey x)
  toAngle (CSharp.NamedType_interface_ x) = alt @"interface_" (mkKey x)
  toAngle (CSharp.NamedType_record_ x) = alt @"record_" (mkKey x)
  toAngle (CSharp.NamedType_struct_ x) = alt @"struct_" (mkKey x)
  toAngle CSharp.NamedType_EMPTY = error "unknown NamedType"

-- LSIF and SCIP languages

instance ToAngle Lsif.SomeEntity where
  toAngle e = case e of
    Lsif.SomeEntity_defn x -> alt @"defn" (mkKey x)
    Lsif.SomeEntity_decl x -> alt @"decl" (mkKey x)
    Lsif.SomeEntity_EMPTY -> error "unknown Lsif.SomeEntity"

instance ToAngle Scip.SomeEntity where
  toAngle (Scip.SomeEntity defn) = rec $ field @"defn" (mkKey defn) end
    -- note: singleton type, not a sum.

-- Codemarkup

instance ToAngleFull Code.Entity where
    toAngleFull entity = case entity of
      Code.Entity_fbthrift (Fbthrift.Entity_decl x) ->
        alt @"fbthrift" (alt @"decl" (toAngleFull x))
      _ -> error "Only thrift entities are expected"

instance Normalize Code.Entity where
    normalize entity = case entity of
      Code.Entity_fbthrift (Fbthrift.Entity_decl x) ->
        Code.Entity_fbthrift (Fbthrift.Entity_decl (normalize x))
      _ -> error "Only thrift entities are expected"
