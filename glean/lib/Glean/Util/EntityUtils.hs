{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}


-- | Support the original @SymId@ based on @QueryCodeEntity@
module Glean.Util.EntityUtils
  ( entityToQuery, trimCodeEntity
  ) where

import Data.Default

import qualified Glean (toQuery)
import Glean.Schema.Code.Types as Code
import Glean.Schema.CodeCxx.Types as Cxx
import Glean.Schema.CodeHack.Types as Hack
import Glean.Schema.CodeFlow.Types as Flow
import Glean.Schema.CodeHs.Types as Hs
import Glean.Schema.CodeJava.Types as Java
import Glean.Schema.CodePp.Types as Pp
import Glean.Schema.CodePython.Types as Python
import Glean.Schema.Hack.Types as Hack
import Glean.Schema.Flow.Types as Flow
import Glean.Schema.Python.Types as Python
import Glean.Schema.Query.Code.Types as Query.Code
import Glean.Schema.Query.CodeCxx.Types as Query.Code.Cxx
import Glean.Schema.Query.CodeJava.Types as Query.Code.Java
import Glean.Schema.Query.CodePp.Types as Query.Code.Pp
import Glean.Schema.Query.Cxx1.Types as Query.Cxx
import Glean.Schema.Query.Java.Types as Query.Java
import Glean.Schema.Query.Pp1.Types as Query.Pp
import Glean.Schema.Query.Src.Types as Query.Src
import Glean.Typed
import Glean.Util.Declarations (applyDeclaration)

-- | Trim the predicate in 'Code.Entity' to only hold a fact id, and not
-- a key or value.  Useful when making a symbol id.
trimCodeEntity :: Code.Entity -> Code.Entity
trimCodeEntity = \case
    Code.Entity_cxx x -> Code.Entity_cxx $ trimCxx x
    Code.Entity_pp x -> Code.Entity_pp $ trimPp x
    Code.Entity_java x -> Code.Entity_java $ trimJava x
    Code.Entity_hs x -> Code.Entity_hs $ trimHs x
    Code.Entity_python x -> Code.Entity_python $ trimPython x
    Code.Entity_hack x -> Code.Entity_hack $ trimHack x
    Code.Entity_flow x -> Code.Entity_flow $ trimFlow x
    _ -> error "trimCodeEntity: unimplemented language case"
  where
    trim :: (Predicate child, SumBranches child parent) => child -> parent
    trim = injectBranch . justId . getId

    trimPp = \case
      Pp.Entity_define x -> trim x
      Pp.Entity_undef x -> trim x
      Pp.Entity_include_ x -> trim x
      Pp.Entity_EMPTY -> error "trimPp: unexpected"

    trimCxx = \case
      Cxx.Entity_decl x -> Cxx.Entity_decl $
        applyDeclaration trim x
      Cxx.Entity_defn x -> Cxx.Entity_defn (trimCxxDefn x)
      Cxx.Entity_enumerator x -> trim x
      Cxx.Entity_EMPTY -> error "trimCxx: unexpected"

    trimCxxDefn = \case
      Cxx.Definition_record_ x -> trim x
      Cxx.Definition_function_ x -> trim x
      Cxx.Definition_enum_ x -> trim x
      Cxx.Definition_objcMethod x -> trim x
      Cxx.Definition_objcContainer x -> trim x
      Cxx.Definition_variable x -> trim x
      Cxx.Definition_namespace_ x -> trim x
      Cxx.Definition_EMPTY -> error "trimCxxDefn: unexpected"

    trimJava = \case
      Java.Entity_class_ x -> trim x
      Java.Entity_EMPTY -> error "trimJava: unexpected"

    trimHs = \case
      Hs.Entity_definition x -> trim x
      Hs.Entity_function_ x -> trim x
      Hs.Entity_class_ x -> trim x
      Hs.Entity_EMPTY -> error "trimHs: unexpected"

    trimPython = \case
      Python.Entity_decl x -> Python.Entity_decl $ case x of
        Python.Declaration_cls y -> trim y
        Python.Declaration_func y -> trim y
        Python.Declaration_variable y -> trim y
        Python.Declaration_imp y -> trim y
        Python.Declaration_module y -> trim y
        Python.Declaration_EMPTY -> error "trimPython: unexpected"
      Python.Entity_EMPTY -> error "trimPython: unexpected"

    trimHack = \case
      Hack.Entity_decl x -> Hack.Entity_decl $ case x of
        Hack.Declaration_classConst y -> trim y
        Hack.Declaration_container y -> Hack.Declaration_container $ case y of
          Hack.ContainerDeclaration_class_ z -> trim z
          Hack.ContainerDeclaration_enum_ z -> trim z
          Hack.ContainerDeclaration_interface_ z -> trim z
          Hack.ContainerDeclaration_trait z -> trim z
          Hack.ContainerDeclaration_EMPTY -> error "trimHack: unexpected"
        Hack.Declaration_enumerator y -> trim y
        Hack.Declaration_function_ y -> trim y
        Hack.Declaration_globalConst y -> trim y
        Hack.Declaration_namespace_ y -> trim y
        Hack.Declaration_method y -> trim y
        Hack.Declaration_property_ y -> trim y
        Hack.Declaration_typeConst y -> trim y
        Hack.Declaration_typedef_ y -> trim y
        Hack.Declaration_EMPTY -> error "trimHack: unexpected"
      Hack.Entity_EMPTY -> error "trimHack: unexpected"

    trimFlow = \case
      Flow.Entity_module_ m -> Flow.Entity_module_ m
      Flow.Entity_decl x -> Flow.Entity_decl $ case x of
         Flow.SomeDeclaration_localDecl d -> trim d
         Flow.SomeDeclaration_memberDecl d -> trim d
         Flow.SomeDeclaration_typeDecl d -> trim d
         Flow.SomeDeclaration_EMPTY -> error "trimFlow: unexpected"
      Flow.Entity_EMPTY -> error "trimFlow: unexpected"

-- -----------------------------------------------------------------------------
-- Entity to query

-- The purpose of this code is to take an Entity that we found via
-- search, and reduce it to a query that will hopefully find the
-- "same" entity when performed in the context of a different repo.
--
-- We can choose exactly how sensitive to be on a case-by-case
-- basis. For example, for Hack we just use the fully-qualified name.
--
-- There's way too much boilerplate here, but there's no easy way to
-- make it simpler without pulling in some heavy machinery.  We're
-- already using codegen to get from the original data to the query
-- types, i.e. toQuery.
--
-- TODO:
-- * support Cxx namespaces
-- * python

-- | Convert a concrete 'Code.Entity' into a fairly-precise search which
-- will return the 'Code.Entity' and hopefully few or none other results.
--
-- This is useful in making a symbol identifier that works accross repo
-- hashes.
entityToQuery :: Code.Entity -> Query.Code.Entity
entityToQuery ent = prune (Glean.toQuery ent)

class Prune query where
  prune :: query -> query

instance Prune Query.Code.Entity where
  prune d@Query.Code.Entity{..} = d
    { entity_cxx = fmap prune entity_cxx
    , entity_pp = fmap prune entity_pp
    , entity_java = fmap prune entity_java
      -- Haskell: FunctionDefinition doesn't have a fully-qualified name,
      -- so let's just keep everything including the source location.
      -- Python: TODO
      -- Hack: Nothing to do, declarations are already just
      -- fully-qualified names and don't have any extraneous stuff
    , Query.Code.entity_any = False
    }

instance Prune Query.Code.Pp.Entity where
  prune Query.Code.Pp.Entity{..} = Query.Code.Pp.Entity
    { entity_define = fmap prune entity_define
    , entity_undef = Nothing
    , entity_include_ = Nothing
    , entity_any = False
    }

instance Prune Query.Code.Cxx.Entity where
  prune Query.Code.Cxx.Entity{..} = Query.Code.Cxx.Entity
    { entity_decl = fmap prune entity_decl
    , entity_defn = fmap prune entity_defn
    , entity_enumerator = fmap prune entity_enumerator
    , entity_any = False
    }

instance Prune Query.Cxx.Declaration where
  prune Query.Cxx.Declaration{..} = Query.Cxx.Declaration
    { declaration_namespace_ = fmap prune declaration_namespace_
    , declaration_usingDeclaration = Nothing
    , declaration_usingDirective = Nothing
    , declaration_record_ = fmap prune declaration_record_
    , declaration_enum_ = fmap prune declaration_enum_
    , declaration_function_ = fmap prune declaration_function_
    , declaration_variable = fmap prune declaration_variable
    , declaration_objcContainer = fmap prune declaration_objcContainer
    , declaration_objcMethod = fmap prune declaration_objcMethod
    , declaration_objcProperty = fmap prune declaration_objcProperty
    , declaration_typeAlias = fmap prune declaration_typeAlias
    , declaration_any = False
    }

instance Prune Query.Code.Java.Entity where
  prune d@Query.Code.Java.Entity{..} = d
    { Query.Code.Java.entity_class_ = fmap prune entity_class_ }

instance Prune Query.Java.ClassDeclaration where
  prune (Query.Java.ClassDeclaration_with_key k) =
    Query.Java.ClassDeclaration_with_key (prune k)
  prune other = other

instance Prune Query.Java.ClassDeclaration_key where
  prune Query.Java.ClassDeclaration_key{..} = def
    { Query.Java.classDeclaration_key_name = classDeclaration_key_name }

instance Prune Query.Pp.Define where
  prune (Query.Pp.Define_with_key k) = Query.Pp.Define_with_key (prune k)
  prune other = other

instance Prune Query.Pp.Define_key where
  -- keep the macro name and the filename
  prune d@Query.Pp.Define_key{..}
    | Just src <- define_key_source = d
    { Query.Pp.define_key_source = Just def
      { Query.Src.range_file = Query.Src.range_file src } }
  prune other = other

instance Prune Query.Cxx.NamespaceDeclaration where
  prune (Query.Cxx.NamespaceDeclaration_with_key k) =
    Query.Cxx.NamespaceDeclaration_with_key (prune k)
  prune other = other

instance Prune Query.Cxx.NamespaceDeclaration_key where
  prune Query.Cxx.NamespaceDeclaration_key{..} = def
    { Query.Cxx.namespaceDeclaration_key_name = namespaceDeclaration_key_name }

instance Prune Query.Cxx.FunctionDeclaration where
  prune (Query.Cxx.FunctionDeclaration_with_key k) =
    Query.Cxx.FunctionDeclaration_with_key (prune k)
  prune other = other

instance Prune Query.Cxx.FunctionDeclaration_key where
  prune Query.Cxx.FunctionDeclaration_key{..} = def
    { Query.Cxx.functionDeclaration_key_name = functionDeclaration_key_name }

instance Prune Query.Cxx.RecordDeclaration where
  prune (Query.Cxx.RecordDeclaration_with_key k) =
    Query.Cxx.RecordDeclaration_with_key (prune k)
  prune other = other

instance Prune Query.Cxx.RecordDeclaration_key where
  prune Query.Cxx.RecordDeclaration_key{..} = def
    { Query.Cxx.recordDeclaration_key_name = recordDeclaration_key_name }

instance Prune Query.Cxx.EnumDeclaration where
  prune (Query.Cxx.EnumDeclaration_with_key k) =
    Query.Cxx.EnumDeclaration_with_key (prune k)
  prune other = other

instance Prune Query.Cxx.EnumDeclaration_key where
  prune Query.Cxx.EnumDeclaration_key{..} = def
    { Query.Cxx.enumDeclaration_key_name = enumDeclaration_key_name }

instance Prune Query.Cxx.VariableDeclaration where
  prune (Query.Cxx.VariableDeclaration_with_key k) =
    Query.Cxx.VariableDeclaration_with_key (prune k)
  prune other = other

instance Prune Query.Cxx.VariableDeclaration_key where
  prune Query.Cxx.VariableDeclaration_key{..} = def
    { Query.Cxx.variableDeclaration_key_name = variableDeclaration_key_name }

instance Prune Query.Cxx.ObjcContainerDeclaration where
  prune (Query.Cxx.ObjcContainerDeclaration_with_key k) =
    Query.Cxx.ObjcContainerDeclaration_with_key (prune k)
  prune other = other

instance Prune Query.Cxx.ObjcContainerDeclaration_key where
  prune Query.Cxx.ObjcContainerDeclaration_key{..} = def
    { Query.Cxx.objcContainerDeclaration_key_id =
      objcContainerDeclaration_key_id }

instance Prune Query.Cxx.ObjcMethodDeclaration where
  prune (Query.Cxx.ObjcMethodDeclaration_with_key k) =
    Query.Cxx.ObjcMethodDeclaration_with_key (prune k)
  prune other = other

instance Prune Query.Cxx.ObjcMethodDeclaration_key where
  -- container + selector
  prune Query.Cxx.ObjcMethodDeclaration_key{..} = def
    { Query.Cxx.objcMethodDeclaration_key_selector =
        objcMethodDeclaration_key_selector
    , Query.Cxx.objcMethodDeclaration_key_container =
        objcMethodDeclaration_key_container }

instance Prune Query.Cxx.ObjcPropertyDeclaration where
  prune (Query.Cxx.ObjcPropertyDeclaration_with_key k) =
    Query.Cxx.ObjcPropertyDeclaration_with_key (prune k)
  prune other = other

instance Prune Query.Cxx.ObjcPropertyDeclaration_key where
  -- container + name
  prune Query.Cxx.ObjcPropertyDeclaration_key{..} = def
    { Query.Cxx.objcPropertyDeclaration_key_name =
      objcPropertyDeclaration_key_name
    , Query.Cxx.objcPropertyDeclaration_key_container =
      objcPropertyDeclaration_key_container
    }

instance Prune Query.Cxx.TypeAliasDeclaration where
  prune (Query.Cxx.TypeAliasDeclaration_with_key k) =
    Query.Cxx.TypeAliasDeclaration_with_key (prune k)
  prune other = other

instance Prune Query.Cxx.TypeAliasDeclaration_key where
  prune Query.Cxx.TypeAliasDeclaration_key{..} = def
    { Query.Cxx.typeAliasDeclaration_key_name = typeAliasDeclaration_key_name }

instance Prune Query.Code.Cxx.Definition where
  prune Query.Code.Cxx.Definition{..} = Query.Code.Cxx.Definition
    { definition_record_ = fmap prune definition_record_
    , definition_function_ = fmap prune definition_function_
    , definition_enum_ = fmap prune definition_enum_
    , definition_objcMethod = fmap prune definition_objcMethod
    , definition_objcContainer = fmap prune definition_objcContainer
    , definition_variable = fmap prune definition_variable
    , definition_namespace_ = fmap prune definition_namespace_
    , definition_any = False
    }

instance Prune Query.Cxx.FunctionDefinition where
  prune (Query.Cxx.FunctionDefinition_with_key k) =
    Query.Cxx.FunctionDefinition_with_key (prune k)
  prune other = other

instance Prune Query.Cxx.FunctionDefinition_key where
  prune Query.Cxx.FunctionDefinition_key{..} = def
    { Query.Cxx.functionDefinition_key_declaration =
      fmap prune functionDefinition_key_declaration }

instance Prune Query.Cxx.RecordDefinition where
  prune (Query.Cxx.RecordDefinition_with_key k) =
    Query.Cxx.RecordDefinition_with_key (prune k)
  prune other = other

instance Prune Query.Cxx.RecordDefinition_key where
  prune Query.Cxx.RecordDefinition_key{..} = def
    { Query.Cxx.recordDefinition_key_declaration =
      fmap prune recordDefinition_key_declaration }

instance Prune Query.Cxx.EnumDefinition where
  prune (Query.Cxx.EnumDefinition_with_key k) =
    Query.Cxx.EnumDefinition_with_key (prune k)
  prune other = other

instance Prune Query.Cxx.EnumDefinition_key where
  prune Query.Cxx.EnumDefinition_key{..} = def
    { Query.Cxx.enumDefinition_key_declaration =
      fmap prune enumDefinition_key_declaration }

instance Prune Query.Cxx.ObjcMethodDefinition where
  prune (Query.Cxx.ObjcMethodDefinition_with_key k) =
    Query.Cxx.ObjcMethodDefinition_with_key (prune k)
  prune other = other

instance Prune Query.Cxx.ObjcContainerDefinition where
  prune (Query.Cxx.ObjcContainerDefinition_with_key k) =
    Query.Cxx.ObjcContainerDefinition_with_key (prune k)
  prune other = other

instance Prune Query.Cxx.ObjcContainerDefinition_key where
  prune Query.Cxx.ObjcContainerDefinition_key{..} = def
    { Query.Cxx.objcContainerDefinition_key_declaration =
      fmap prune objcContainerDefinition_key_declaration }

instance Prune Query.Cxx.NamespaceDefinition where
  prune (Query.Cxx.NamespaceDefinition_with_key k) =
    Query.Cxx.NamespaceDefinition_with_key (prune k)
  prune other = other

instance Prune Query.Cxx.NamespaceDefinition_key where
  prune Query.Cxx.NamespaceDefinition_key{..} = def
    { Query.Cxx.namespaceDefinition_key_declaration =
      fmap prune namespaceDefinition_key_declaration }

instance Prune Query.Cxx.Enumerator where
  prune (Query.Cxx.Enumerator_with_key k) =
    Query.Cxx.Enumerator_with_key (prune k)
  prune other = other

instance Prune Query.Cxx.Enumerator_key where
  -- name + enumeration
  prune Query.Cxx.Enumerator_key{..} = def
    { Query.Cxx.enumerator_key_name = enumerator_key_name
    , Query.Cxx.enumerator_key_enumeration =
      fmap prune enumerator_key_enumeration
    }
