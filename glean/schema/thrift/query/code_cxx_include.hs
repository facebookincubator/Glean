-- @generated
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, DataKinds #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
import qualified Data.ByteString
import qualified Data.Default
import qualified Data.Text

import qualified Glean.Types as Glean
import qualified Glean.Typed as Glean
import qualified Glean.Query.Angle as Angle

import qualified Glean.Schema.Builtin.Types
import qualified Glean.Schema.Query.Builtin.Types

import qualified Glean.Schema.Cxx1.Types
import qualified Glean.Schema.Query.Cxx1.Types

import qualified Glean.Schema.CodeCxx.Types


type instance Glean.QueryResult Glean.Schema.Query.CodeCxx.Types.DeclToDef_key = Glean.Schema.CodeCxx.Types.DeclToDef_key
type instance Glean.QueryOf Glean.Schema.CodeCxx.Types.DeclToDef_key = Glean.Schema.Query.CodeCxx.Types.DeclToDef_key

instance Glean.ToQuery Glean.Schema.CodeCxx.Types.DeclToDef_key where
  toQuery (Glean.Schema.CodeCxx.Types.DeclToDef_key x1 x2) = Glean.Schema.Query.CodeCxx.Types.DeclToDef_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.CodeCxx.Types.DeclToDef where
  toQueryId = Glean.Schema.Query.CodeCxx.Types.DeclToDef_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.CodeCxx.Types.DeclToDef_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.CodeCxx.Types.DeclToDef = Glean.Schema.CodeCxx.Types.DeclToDef
type instance Glean.QueryOf Glean.Schema.CodeCxx.Types.DeclToDef = Glean.Schema.Query.CodeCxx.Types.DeclToDef

instance Glean.ToQuery Glean.Schema.CodeCxx.Types.DeclToDef

type instance Glean.QueryResult Glean.Schema.Query.CodeCxx.Types.Entity = Glean.Schema.CodeCxx.Types.Entity
type instance Glean.QueryOf Glean.Schema.CodeCxx.Types.Entity = Glean.Schema.Query.CodeCxx.Types.Entity

instance Glean.ToQuery Glean.Schema.CodeCxx.Types.Entity where
  toQuery (Glean.Schema.CodeCxx.Types.Entity_decl x) = Data.Default.def { Glean.Schema.Query.CodeCxx.Types.entity_decl = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.CodeCxx.Types.Entity_defn x) = Data.Default.def { Glean.Schema.Query.CodeCxx.Types.entity_defn = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.CodeCxx.Types.Entity_enumerator x) = Data.Default.def { Glean.Schema.Query.CodeCxx.Types.entity_enumerator = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.Declaration Glean.Schema.Query.CodeCxx.Types.Entity where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.CodeCxx.Types.entity_decl = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.CodeCxx.Types.Definition Glean.Schema.Query.CodeCxx.Types.Entity where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.CodeCxx.Types.entity_defn = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.Enumerator Glean.Schema.Query.CodeCxx.Types.Entity where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.CodeCxx.Types.entity_enumerator = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.CodeCxx.Types.Definition = Glean.Schema.CodeCxx.Types.Definition
type instance Glean.QueryOf Glean.Schema.CodeCxx.Types.Definition = Glean.Schema.Query.CodeCxx.Types.Definition

instance Glean.ToQuery Glean.Schema.CodeCxx.Types.Definition where
  toQuery (Glean.Schema.CodeCxx.Types.Definition_record_ x) = Data.Default.def { Glean.Schema.Query.CodeCxx.Types.definition_record_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.CodeCxx.Types.Definition_function_ x) = Data.Default.def { Glean.Schema.Query.CodeCxx.Types.definition_function_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.CodeCxx.Types.Definition_enum_ x) = Data.Default.def { Glean.Schema.Query.CodeCxx.Types.definition_enum_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.CodeCxx.Types.Definition_objcMethod x) = Data.Default.def { Glean.Schema.Query.CodeCxx.Types.definition_objcMethod = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.CodeCxx.Types.Definition_objcContainer x) = Data.Default.def { Glean.Schema.Query.CodeCxx.Types.definition_objcContainer = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.CodeCxx.Types.Definition_variable x) = Data.Default.def { Glean.Schema.Query.CodeCxx.Types.definition_variable = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.CodeCxx.Types.Definition_namespace_ x) = Data.Default.def { Glean.Schema.Query.CodeCxx.Types.definition_namespace_ = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.RecordDefinition Glean.Schema.Query.CodeCxx.Types.Definition where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.CodeCxx.Types.definition_record_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.FunctionDefinition Glean.Schema.Query.CodeCxx.Types.Definition where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.CodeCxx.Types.definition_function_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.EnumDefinition Glean.Schema.Query.CodeCxx.Types.Definition where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.CodeCxx.Types.definition_enum_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.ObjcMethodDefinition Glean.Schema.Query.CodeCxx.Types.Definition where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.CodeCxx.Types.definition_objcMethod = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.ObjcContainerDefinition Glean.Schema.Query.CodeCxx.Types.Definition where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.CodeCxx.Types.definition_objcContainer = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.VariableDeclaration Glean.Schema.Query.CodeCxx.Types.Definition where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.CodeCxx.Types.definition_variable = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.NamespaceDefinition Glean.Schema.Query.CodeCxx.Types.Definition where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.CodeCxx.Types.definition_namespace_ = Prelude.Just q }
