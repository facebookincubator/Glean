-- @generated
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
import qualified Data.ByteString
import qualified Data.Default
import qualified Data.Text

import qualified Glean.Types as Glean
import qualified Glean.Typed as Glean
import qualified Glean.Query.Angle as Angle

import qualified Glean.Schema.Builtin.Types
import qualified Glean.Schema.Query.Builtin.Types

import qualified Glean.Schema.Src.Types
import qualified Glean.Schema.Query.Src.Types

import qualified Glean.Schema.Hs.Types


type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.Class_key = Glean.Schema.Hs.Types.Class_key
type instance Glean.QueryOf Glean.Schema.Hs.Types.Class_key = Glean.Schema.Query.Hs.Types.Class_key

instance Glean.ToQuery Glean.Schema.Hs.Types.Class_key where
  toQuery (Glean.Schema.Hs.Types.Class_key x1 x2) = Glean.Schema.Query.Hs.Types.Class_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Hs.Types.Class where
  toQueryId = Glean.Schema.Query.Hs.Types.Class_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hs.Types.Class_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.Class = Glean.Schema.Hs.Types.Class
type instance Glean.QueryOf Glean.Schema.Hs.Types.Class = Glean.Schema.Query.Hs.Types.Class

instance Glean.ToQuery Glean.Schema.Hs.Types.Class

instance Glean.PredicateQuery Glean.Schema.Hs.Types.FunctionName where
  toQueryId = Glean.Schema.Query.Hs.Types.FunctionName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hs.Types.FunctionName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.FunctionName = Glean.Schema.Hs.Types.FunctionName
type instance Glean.QueryOf Glean.Schema.Hs.Types.FunctionName = Glean.Schema.Query.Hs.Types.FunctionName

instance Glean.ToQuery Glean.Schema.Hs.Types.FunctionName

instance Glean.PredicateQuery Glean.Schema.Hs.Types.Type where
  toQueryId = Glean.Schema.Query.Hs.Types.Type__with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hs.Types.Type__with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.Type_ = Glean.Schema.Hs.Types.Type
type instance Glean.QueryOf Glean.Schema.Hs.Types.Type = Glean.Schema.Query.Hs.Types.Type_

instance Glean.ToQuery Glean.Schema.Hs.Types.Type

instance Glean.PredicateQuery Glean.Schema.Hs.Types.PackageId where
  toQueryId = Glean.Schema.Query.Hs.Types.PackageId_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hs.Types.PackageId_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.PackageId = Glean.Schema.Hs.Types.PackageId
type instance Glean.QueryOf Glean.Schema.Hs.Types.PackageId = Glean.Schema.Query.Hs.Types.PackageId

instance Glean.ToQuery Glean.Schema.Hs.Types.PackageId

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.Module_key = Glean.Schema.Hs.Types.Module_key
type instance Glean.QueryOf Glean.Schema.Hs.Types.Module_key = Glean.Schema.Query.Hs.Types.Module_key

instance Glean.ToQuery Glean.Schema.Hs.Types.Module_key where
  toQuery (Glean.Schema.Hs.Types.Module_key x1 x2 x3) = Glean.Schema.Query.Hs.Types.Module_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Hs.Types.Module where
  toQueryId = Glean.Schema.Query.Hs.Types.Module_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hs.Types.Module_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.Module = Glean.Schema.Hs.Types.Module
type instance Glean.QueryOf Glean.Schema.Hs.Types.Module = Glean.Schema.Query.Hs.Types.Module

instance Glean.ToQuery Glean.Schema.Hs.Types.Module

instance Glean.PredicateQuery Glean.Schema.Hs.Types.DefinitionName where
  toQueryId = Glean.Schema.Query.Hs.Types.DefinitionName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hs.Types.DefinitionName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.DefinitionName = Glean.Schema.Hs.Types.DefinitionName
type instance Glean.QueryOf Glean.Schema.Hs.Types.DefinitionName = Glean.Schema.Query.Hs.Types.DefinitionName

instance Glean.ToQuery Glean.Schema.Hs.Types.DefinitionName

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.ModuleDefinitions_key = Glean.Schema.Hs.Types.ModuleDefinitions_key
type instance Glean.QueryOf Glean.Schema.Hs.Types.ModuleDefinitions_key = Glean.Schema.Query.Hs.Types.ModuleDefinitions_key

instance Glean.ToQuery Glean.Schema.Hs.Types.ModuleDefinitions_key where
  toQuery (Glean.Schema.Hs.Types.ModuleDefinitions_key x1 x2) = Glean.Schema.Query.Hs.Types.ModuleDefinitions_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Hs.Types.ModuleDefinitions_functionDefinitions_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Hs.Types.ModuleDefinitions where
  toQueryId = Glean.Schema.Query.Hs.Types.ModuleDefinitions_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hs.Types.ModuleDefinitions_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.ModuleDefinitions = Glean.Schema.Hs.Types.ModuleDefinitions
type instance Glean.QueryOf Glean.Schema.Hs.Types.ModuleDefinitions = Glean.Schema.Query.Hs.Types.ModuleDefinitions

instance Glean.ToQuery Glean.Schema.Hs.Types.ModuleDefinitions

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.FileXRefMap_key = Glean.Schema.Hs.Types.FileXRefMap_key
type instance Glean.QueryOf Glean.Schema.Hs.Types.FileXRefMap_key = Glean.Schema.Query.Hs.Types.FileXRefMap_key

instance Glean.ToQuery Glean.Schema.Hs.Types.FileXRefMap_key where
  toQuery (Glean.Schema.Hs.Types.FileXRefMap_key x1 x2) = Glean.Schema.Query.Hs.Types.FileXRefMap_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Hs.Types.FileXRefMap_refs_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Hs.Types.FileXRefMap where
  toQueryId = Glean.Schema.Query.Hs.Types.FileXRefMap_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hs.Types.FileXRefMap_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.FileXRefMap = Glean.Schema.Hs.Types.FileXRefMap
type instance Glean.QueryOf Glean.Schema.Hs.Types.FileXRefMap = Glean.Schema.Query.Hs.Types.FileXRefMap

instance Glean.ToQuery Glean.Schema.Hs.Types.FileXRefMap

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.Definition_key = Glean.Schema.Hs.Types.Definition_key
type instance Glean.QueryOf Glean.Schema.Hs.Types.Definition_key = Glean.Schema.Query.Hs.Types.Definition_key

instance Glean.ToQuery Glean.Schema.Hs.Types.Definition_key where
  toQuery (Glean.Schema.Hs.Types.Definition_key x1 x2) = Glean.Schema.Query.Hs.Types.Definition_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Hs.Types.Definition where
  toQueryId = Glean.Schema.Query.Hs.Types.Definition_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hs.Types.Definition_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.Definition = Glean.Schema.Hs.Types.Definition
type instance Glean.QueryOf Glean.Schema.Hs.Types.Definition = Glean.Schema.Query.Hs.Types.Definition

instance Glean.ToQuery Glean.Schema.Hs.Types.Definition

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.Definition_1_key = Glean.Schema.Hs.Types.Definition_1_key
type instance Glean.QueryOf Glean.Schema.Hs.Types.Definition_1_key = Glean.Schema.Query.Hs.Types.Definition_1_key

instance Glean.ToQuery Glean.Schema.Hs.Types.Definition_1_key where
  toQuery (Glean.Schema.Hs.Types.Definition_1_key x1 x2) = Glean.Schema.Query.Hs.Types.Definition_1_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Hs.Types.Definition_1 where
  toQueryId = Glean.Schema.Query.Hs.Types.Definition_1_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hs.Types.Definition_1_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.Definition_1 = Glean.Schema.Hs.Types.Definition_1
type instance Glean.QueryOf Glean.Schema.Hs.Types.Definition_1 = Glean.Schema.Query.Hs.Types.Definition_1

instance Glean.ToQuery Glean.Schema.Hs.Types.Definition_1

instance Glean.PredicateQuery Glean.Schema.Hs.Types.ModuleName where
  toQueryId = Glean.Schema.Query.Hs.Types.ModuleName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hs.Types.ModuleName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.ModuleName = Glean.Schema.Hs.Types.ModuleName
type instance Glean.QueryOf Glean.Schema.Hs.Types.ModuleName = Glean.Schema.Query.Hs.Types.ModuleName

instance Glean.ToQuery Glean.Schema.Hs.Types.ModuleName

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.ClassInstance_key = Glean.Schema.Hs.Types.ClassInstance_key
type instance Glean.QueryOf Glean.Schema.Hs.Types.ClassInstance_key = Glean.Schema.Query.Hs.Types.ClassInstance_key

instance Glean.ToQuery Glean.Schema.Hs.Types.ClassInstance_key where
  toQuery (Glean.Schema.Hs.Types.ClassInstance_key x1 x2 x3) = Glean.Schema.Query.Hs.Types.ClassInstance_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Hs.Types.ClassInstance where
  toQueryId = Glean.Schema.Query.Hs.Types.ClassInstance_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hs.Types.ClassInstance_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.ClassInstance = Glean.Schema.Hs.Types.ClassInstance
type instance Glean.QueryOf Glean.Schema.Hs.Types.ClassInstance = Glean.Schema.Query.Hs.Types.ClassInstance

instance Glean.ToQuery Glean.Schema.Hs.Types.ClassInstance

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.FunctionDefinition_key = Glean.Schema.Hs.Types.FunctionDefinition_key
type instance Glean.QueryOf Glean.Schema.Hs.Types.FunctionDefinition_key = Glean.Schema.Query.Hs.Types.FunctionDefinition_key

instance Glean.ToQuery Glean.Schema.Hs.Types.FunctionDefinition_key where
  toQuery (Glean.Schema.Hs.Types.FunctionDefinition_key x1 x2) = Glean.Schema.Query.Hs.Types.FunctionDefinition_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Hs.Types.FunctionDefinition where
  toQueryId = Glean.Schema.Query.Hs.Types.FunctionDefinition_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hs.Types.FunctionDefinition_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.FunctionDefinition = Glean.Schema.Hs.Types.FunctionDefinition
type instance Glean.QueryOf Glean.Schema.Hs.Types.FunctionDefinition = Glean.Schema.Query.Hs.Types.FunctionDefinition

instance Glean.ToQuery Glean.Schema.Hs.Types.FunctionDefinition

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.SourceModule_key = Glean.Schema.Hs.Types.SourceModule_key
type instance Glean.QueryOf Glean.Schema.Hs.Types.SourceModule_key = Glean.Schema.Query.Hs.Types.SourceModule_key

instance Glean.ToQuery Glean.Schema.Hs.Types.SourceModule_key where
  toQuery (Glean.Schema.Hs.Types.SourceModule_key x1 x2) = Glean.Schema.Query.Hs.Types.SourceModule_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Hs.Types.SourceModule where
  toQueryId = Glean.Schema.Query.Hs.Types.SourceModule_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hs.Types.SourceModule_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.SourceModule = Glean.Schema.Hs.Types.SourceModule
type instance Glean.QueryOf Glean.Schema.Hs.Types.SourceModule = Glean.Schema.Query.Hs.Types.SourceModule

instance Glean.ToQuery Glean.Schema.Hs.Types.SourceModule

instance Glean.PredicateQuery Glean.Schema.Hs.Types.ClassName where
  toQueryId = Glean.Schema.Query.Hs.Types.ClassName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hs.Types.ClassName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.ClassName = Glean.Schema.Hs.Types.ClassName
type instance Glean.QueryOf Glean.Schema.Hs.Types.ClassName = Glean.Schema.Query.Hs.Types.ClassName

instance Glean.ToQuery Glean.Schema.Hs.Types.ClassName

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.XRef_key = Glean.Schema.Hs.Types.XRef_key
type instance Glean.QueryOf Glean.Schema.Hs.Types.XRef_key = Glean.Schema.Query.Hs.Types.XRef_key

instance Glean.ToQuery Glean.Schema.Hs.Types.XRef_key where
  toQuery (Glean.Schema.Hs.Types.XRef_key x1 x2) = Glean.Schema.Query.Hs.Types.XRef_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Hs.Types.XRef where
  toQueryId = Glean.Schema.Query.Hs.Types.XRef_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hs.Types.XRef_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.XRef = Glean.Schema.Hs.Types.XRef
type instance Glean.QueryOf Glean.Schema.Hs.Types.XRef = Glean.Schema.Query.Hs.Types.XRef

instance Glean.ToQuery Glean.Schema.Hs.Types.XRef

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.XRefTarget = Glean.Schema.Hs.Types.XRefTarget
type instance Glean.QueryOf Glean.Schema.Hs.Types.XRefTarget = Glean.Schema.Query.Hs.Types.XRefTarget

instance Glean.ToQuery Glean.Schema.Hs.Types.XRefTarget where
  toQuery (Glean.Schema.Hs.Types.XRefTarget_definition x) = Data.Default.def { Glean.Schema.Query.Hs.Types.xRefTarget_definition = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Hs.Types.XRefTarget_typeclass x) = Data.Default.def { Glean.Schema.Query.Hs.Types.xRefTarget_typeclass = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Hs.Types.XRefTarget_hs_module x) = Data.Default.def { Glean.Schema.Query.Hs.Types.xRefTarget_hs_module = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Hs.Types.DefinitionName Glean.Schema.Query.Hs.Types.XRefTarget where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Hs.Types.xRefTarget_definition = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Hs.Types.ClassName Glean.Schema.Query.Hs.Types.XRefTarget where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Hs.Types.xRefTarget_typeclass = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Hs.Types.ModuleName Glean.Schema.Query.Hs.Types.XRefTarget where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Hs.Types.xRefTarget_hs_module = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Hs.Types.XReference = Glean.Schema.Hs.Types.XReference
type instance Glean.QueryOf Glean.Schema.Hs.Types.XReference = Glean.Schema.Query.Hs.Types.XReference

instance Glean.ToQuery Glean.Schema.Hs.Types.XReference where
  toQuery (Glean.Schema.Hs.Types.XReference x1 x2) = Glean.Schema.Query.Hs.Types.XReference (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Hs.Types.XReference_spans_array_exact . Prelude.map Glean.toQuery) x2))
