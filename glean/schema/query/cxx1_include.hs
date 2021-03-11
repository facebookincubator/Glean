-- @generated
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
import qualified Data.ByteString
import qualified Data.Default
import qualified Data.Text

import qualified Glean.Types as Glean
import qualified Glean.Typed as Glean
import qualified Glean.Query.Angle as Angle

import qualified Glean.Schema.Buck.Types
import qualified Glean.Schema.Query.Buck.Types

import qualified Glean.Schema.Builtin.Types
import qualified Glean.Schema.Query.Builtin.Types

import qualified Glean.Schema.Pp1.Types
import qualified Glean.Schema.Query.Pp1.Types

import qualified Glean.Schema.Src.Types
import qualified Glean.Schema.Query.Src.Types

import qualified Glean.Schema.Cxx1.Types


type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.NamespaceQName_key = Glean.Schema.Cxx1.Types.NamespaceQName_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.NamespaceQName_key = Glean.Schema.Query.Cxx1.Types.NamespaceQName_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.NamespaceQName_key where
  toQuery (Glean.Schema.Cxx1.Types.NamespaceQName_key x1 x2) = Glean.Schema.Query.Cxx1.Types.NamespaceQName_key (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Cxx1.Types.namespaceQName_name_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Cxx1.Types.namespaceQName_name_just = Prelude.Just (Glean.toQuery x)})) x1)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Cxx1.Types.namespaceQName_parent_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Cxx1.Types.namespaceQName_parent_just = Prelude.Just (Glean.toQuery x)})) x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.NamespaceQName where
  toQueryId = Glean.Schema.Query.Cxx1.Types.NamespaceQName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.NamespaceQName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.NamespaceQName = Glean.Schema.Cxx1.Types.NamespaceQName
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.NamespaceQName = Glean.Schema.Query.Cxx1.Types.NamespaceQName

instance Glean.ToQuery Glean.Schema.Cxx1.Types.NamespaceQName

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.DeclarationSrcRange_key = Glean.Schema.Cxx1.Types.DeclarationSrcRange_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.DeclarationSrcRange_key = Glean.Schema.Query.Cxx1.Types.DeclarationSrcRange_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.DeclarationSrcRange_key where
  toQuery (Glean.Schema.Cxx1.Types.DeclarationSrcRange_key x1 x2) = Glean.Schema.Query.Cxx1.Types.DeclarationSrcRange_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.DeclarationSrcRange where
  toQueryId = Glean.Schema.Query.Cxx1.Types.DeclarationSrcRange_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.DeclarationSrcRange_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.DeclarationSrcRange = Glean.Schema.Cxx1.Types.DeclarationSrcRange
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.DeclarationSrcRange = Glean.Schema.Query.Cxx1.Types.DeclarationSrcRange

instance Glean.ToQuery Glean.Schema.Cxx1.Types.DeclarationSrcRange

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.RecordDeclaration_key = Glean.Schema.Cxx1.Types.RecordDeclaration_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.RecordDeclaration_key = Glean.Schema.Query.Cxx1.Types.RecordDeclaration_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.RecordDeclaration_key where
  toQuery (Glean.Schema.Cxx1.Types.RecordDeclaration_key x1 x2 x3) = Glean.Schema.Query.Cxx1.Types.RecordDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.RecordDeclaration where
  toQueryId = Glean.Schema.Query.Cxx1.Types.RecordDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.RecordDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.RecordDeclaration = Glean.Schema.Cxx1.Types.RecordDeclaration
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.RecordDeclaration = Glean.Schema.Query.Cxx1.Types.RecordDeclaration

instance Glean.ToQuery Glean.Schema.Cxx1.Types.RecordDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.XRefIndirectTarget_key = Glean.Schema.Cxx1.Types.XRefIndirectTarget_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.XRefIndirectTarget_key = Glean.Schema.Query.Cxx1.Types.XRefIndirectTarget_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.XRefIndirectTarget_key where
  toQuery (Glean.Schema.Cxx1.Types.XRefIndirectTarget_key x1 x2) = Glean.Schema.Query.Cxx1.Types.XRefIndirectTarget_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.XRefIndirectTarget where
  toQueryId = Glean.Schema.Query.Cxx1.Types.XRefIndirectTarget_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.XRefIndirectTarget_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.XRefIndirectTarget = Glean.Schema.Cxx1.Types.XRefIndirectTarget
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.XRefIndirectTarget = Glean.Schema.Query.Cxx1.Types.XRefIndirectTarget

instance Glean.ToQuery Glean.Schema.Cxx1.Types.XRefIndirectTarget

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.DeclByName_key = Glean.Schema.Cxx1.Types.DeclByName_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.DeclByName_key = Glean.Schema.Query.Cxx1.Types.DeclByName_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.DeclByName_key where
  toQuery (Glean.Schema.Cxx1.Types.DeclByName_key x1 x2 x3) = Glean.Schema.Query.Cxx1.Types.DeclByName_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.DeclByName where
  toQueryId = Glean.Schema.Query.Cxx1.Types.DeclByName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.DeclByName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.DeclByName = Glean.Schema.Cxx1.Types.DeclByName
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.DeclByName = Glean.Schema.Query.Cxx1.Types.DeclByName

instance Glean.ToQuery Glean.Schema.Cxx1.Types.DeclByName

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.DeclInObjcContainer_key = Glean.Schema.Cxx1.Types.DeclInObjcContainer_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.DeclInObjcContainer_key = Glean.Schema.Query.Cxx1.Types.DeclInObjcContainer_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.DeclInObjcContainer_key where
  toQuery (Glean.Schema.Cxx1.Types.DeclInObjcContainer_key x1 x2) = Glean.Schema.Query.Cxx1.Types.DeclInObjcContainer_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.DeclInObjcContainer where
  toQueryId = Glean.Schema.Query.Cxx1.Types.DeclInObjcContainer_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.DeclInObjcContainer_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.DeclInObjcContainer = Glean.Schema.Cxx1.Types.DeclInObjcContainer
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.DeclInObjcContainer = Glean.Schema.Query.Cxx1.Types.DeclInObjcContainer

instance Glean.ToQuery Glean.Schema.Cxx1.Types.DeclInObjcContainer

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.DeclarationTargets_key = Glean.Schema.Cxx1.Types.DeclarationTargets_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.DeclarationTargets_key = Glean.Schema.Query.Cxx1.Types.DeclarationTargets_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.DeclarationTargets_key where
  toQuery (Glean.Schema.Cxx1.Types.DeclarationTargets_key x1 x2) = Glean.Schema.Query.Cxx1.Types.DeclarationTargets_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Cxx1.Types.DeclarationTargets_targets_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.DeclarationTargets where
  toQueryId = Glean.Schema.Query.Cxx1.Types.DeclarationTargets_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.DeclarationTargets_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.DeclarationTargets = Glean.Schema.Cxx1.Types.DeclarationTargets
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.DeclarationTargets = Glean.Schema.Query.Cxx1.Types.DeclarationTargets

instance Glean.ToQuery Glean.Schema.Cxx1.Types.DeclarationTargets

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcContainerInheritance_key = Glean.Schema.Cxx1.Types.ObjcContainerInheritance_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcContainerInheritance_key = Glean.Schema.Query.Cxx1.Types.ObjcContainerInheritance_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcContainerInheritance_key where
  toQuery (Glean.Schema.Cxx1.Types.ObjcContainerInheritance_key x1 x2) = Glean.Schema.Query.Cxx1.Types.ObjcContainerInheritance_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.ObjcContainerInheritance where
  toQueryId = Glean.Schema.Query.Cxx1.Types.ObjcContainerInheritance_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.ObjcContainerInheritance_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcContainerInheritance = Glean.Schema.Cxx1.Types.ObjcContainerInheritance
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcContainerInheritance = Glean.Schema.Query.Cxx1.Types.ObjcContainerInheritance

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcContainerInheritance

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcPropertyIVar_key = Glean.Schema.Cxx1.Types.ObjcPropertyIVar_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcPropertyIVar_key = Glean.Schema.Query.Cxx1.Types.ObjcPropertyIVar_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcPropertyIVar_key where
  toQuery (Glean.Schema.Cxx1.Types.ObjcPropertyIVar_key x1 x2) = Glean.Schema.Query.Cxx1.Types.ObjcPropertyIVar_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.ObjcPropertyIVar where
  toQueryId = Glean.Schema.Query.Cxx1.Types.ObjcPropertyIVar_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.ObjcPropertyIVar_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcPropertyIVar = Glean.Schema.Cxx1.Types.ObjcPropertyIVar
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcPropertyIVar = Glean.Schema.Query.Cxx1.Types.ObjcPropertyIVar

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcPropertyIVar

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.FunctionDefinition_key = Glean.Schema.Cxx1.Types.FunctionDefinition_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.FunctionDefinition_key = Glean.Schema.Query.Cxx1.Types.FunctionDefinition_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.FunctionDefinition_key where
  toQuery (Glean.Schema.Cxx1.Types.FunctionDefinition_key x1 x2) = Glean.Schema.Query.Cxx1.Types.FunctionDefinition_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.FunctionDefinition where
  toQueryId = Glean.Schema.Query.Cxx1.Types.FunctionDefinition_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.FunctionDefinition_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.FunctionDefinition = Glean.Schema.Cxx1.Types.FunctionDefinition
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.FunctionDefinition = Glean.Schema.Query.Cxx1.Types.FunctionDefinition

instance Glean.ToQuery Glean.Schema.Cxx1.Types.FunctionDefinition

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.TranslationUnitXRefs_key = Glean.Schema.Cxx1.Types.TranslationUnitXRefs_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.TranslationUnitXRefs_key = Glean.Schema.Query.Cxx1.Types.TranslationUnitXRefs_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.TranslationUnitXRefs_key where
  toQuery (Glean.Schema.Cxx1.Types.TranslationUnitXRefs_key x1 x2) = Glean.Schema.Query.Cxx1.Types.TranslationUnitXRefs_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Cxx1.Types.TranslationUnitXRefs_xrefs_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.TranslationUnitXRefs where
  toQueryId = Glean.Schema.Query.Cxx1.Types.TranslationUnitXRefs_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.TranslationUnitXRefs_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.TranslationUnitXRefs = Glean.Schema.Cxx1.Types.TranslationUnitXRefs
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.TranslationUnitXRefs = Glean.Schema.Query.Cxx1.Types.TranslationUnitXRefs

instance Glean.ToQuery Glean.Schema.Cxx1.Types.TranslationUnitXRefs

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.Signature_key = Glean.Schema.Cxx1.Types.Signature_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.Signature_key = Glean.Schema.Query.Cxx1.Types.Signature_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.Signature_key where
  toQuery (Glean.Schema.Cxx1.Types.Signature_key x1 x2) = Glean.Schema.Query.Cxx1.Types.Signature_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Cxx1.Types.Signature_parameters_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.Signature where
  toQueryId = Glean.Schema.Query.Cxx1.Types.Signature_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.Signature_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.Signature = Glean.Schema.Cxx1.Types.Signature
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.Signature = Glean.Schema.Query.Cxx1.Types.Signature

instance Glean.ToQuery Glean.Schema.Cxx1.Types.Signature

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.Attribute where
  toQueryId = Glean.Schema.Query.Cxx1.Types.Attribute_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.Attribute_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.Attribute = Glean.Schema.Cxx1.Types.Attribute
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.Attribute = Glean.Schema.Query.Cxx1.Types.Attribute

instance Glean.ToQuery Glean.Schema.Cxx1.Types.Attribute

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.DeclInRecord_key = Glean.Schema.Cxx1.Types.DeclInRecord_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.DeclInRecord_key = Glean.Schema.Query.Cxx1.Types.DeclInRecord_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.DeclInRecord_key where
  toQuery (Glean.Schema.Cxx1.Types.DeclInRecord_key x1 x2) = Glean.Schema.Query.Cxx1.Types.DeclInRecord_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.DeclInRecord where
  toQueryId = Glean.Schema.Query.Cxx1.Types.DeclInRecord_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.DeclInRecord_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.DeclInRecord = Glean.Schema.Cxx1.Types.DeclInRecord
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.DeclInRecord = Glean.Schema.Query.Cxx1.Types.DeclInRecord

instance Glean.ToQuery Glean.Schema.Cxx1.Types.DeclInRecord

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.FileXRefs_key = Glean.Schema.Cxx1.Types.FileXRefs_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.FileXRefs_key = Glean.Schema.Query.Cxx1.Types.FileXRefs_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.FileXRefs_key where
  toQuery (Glean.Schema.Cxx1.Types.FileXRefs_key x1 x2) = Glean.Schema.Query.Cxx1.Types.FileXRefs_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Cxx1.Types.FileXRefs_externals_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.FileXRefs where
  toQueryId = Glean.Schema.Query.Cxx1.Types.FileXRefs_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.FileXRefs_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.FileXRefs = Glean.Schema.Cxx1.Types.FileXRefs
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.FileXRefs = Glean.Schema.Query.Cxx1.Types.FileXRefs

instance Glean.ToQuery Glean.Schema.Cxx1.Types.FileXRefs

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.ObjcSelector where
  toQueryId = Glean.Schema.Query.Cxx1.Types.ObjcSelector_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.ObjcSelector_with_key . (Glean.Schema.Query.Cxx1.Types.ObjcSelector_array_exact . Prelude.map Glean.toQuery)

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcSelector = Glean.Schema.Cxx1.Types.ObjcSelector
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcSelector = Glean.Schema.Query.Cxx1.Types.ObjcSelector

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcSelector

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.NamespaceDefinition_key = Glean.Schema.Cxx1.Types.NamespaceDefinition_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.NamespaceDefinition_key = Glean.Schema.Query.Cxx1.Types.NamespaceDefinition_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.NamespaceDefinition_key where
  toQuery (Glean.Schema.Cxx1.Types.NamespaceDefinition_key x1 x2) = Glean.Schema.Query.Cxx1.Types.NamespaceDefinition_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.NamespaceDefinition where
  toQueryId = Glean.Schema.Query.Cxx1.Types.NamespaceDefinition_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.NamespaceDefinition_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.NamespaceDefinition = Glean.Schema.Cxx1.Types.NamespaceDefinition
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.NamespaceDefinition = Glean.Schema.Query.Cxx1.Types.NamespaceDefinition

instance Glean.ToQuery Glean.Schema.Cxx1.Types.NamespaceDefinition

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.FunctionAttribute_key = Glean.Schema.Cxx1.Types.FunctionAttribute_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.FunctionAttribute_key = Glean.Schema.Query.Cxx1.Types.FunctionAttribute_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.FunctionAttribute_key where
  toQuery (Glean.Schema.Cxx1.Types.FunctionAttribute_key x1 x2) = Glean.Schema.Query.Cxx1.Types.FunctionAttribute_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.FunctionAttribute where
  toQueryId = Glean.Schema.Query.Cxx1.Types.FunctionAttribute_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.FunctionAttribute_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.FunctionAttribute = Glean.Schema.Cxx1.Types.FunctionAttribute
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.FunctionAttribute = Glean.Schema.Query.Cxx1.Types.FunctionAttribute

instance Glean.ToQuery Glean.Schema.Cxx1.Types.FunctionAttribute

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.TypeAliasDeclaration_key = Glean.Schema.Cxx1.Types.TypeAliasDeclaration_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.TypeAliasDeclaration_key = Glean.Schema.Query.Cxx1.Types.TypeAliasDeclaration_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.TypeAliasDeclaration_key where
  toQuery (Glean.Schema.Cxx1.Types.TypeAliasDeclaration_key x1 x2 x3 x4) = Glean.Schema.Query.Cxx1.Types.TypeAliasDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just (Glean.toQuery x4))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.TypeAliasDeclaration where
  toQueryId = Glean.Schema.Query.Cxx1.Types.TypeAliasDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.TypeAliasDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.TypeAliasDeclaration = Glean.Schema.Cxx1.Types.TypeAliasDeclaration
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.TypeAliasDeclaration = Glean.Schema.Query.Cxx1.Types.TypeAliasDeclaration

instance Glean.ToQuery Glean.Schema.Cxx1.Types.TypeAliasDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.FileXRefMap_key = Glean.Schema.Cxx1.Types.FileXRefMap_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.FileXRefMap_key = Glean.Schema.Query.Cxx1.Types.FileXRefMap_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.FileXRefMap_key where
  toQuery (Glean.Schema.Cxx1.Types.FileXRefMap_key x1 x2 x3) = Glean.Schema.Query.Cxx1.Types.FileXRefMap_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Cxx1.Types.FileXRefMap_fixed_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Glean.Schema.Query.Cxx1.Types.FileXRefMap_variable_array_exact . Prelude.map (Glean.Schema.Query.Src.Types.ByteSpans_exact . Prelude.map Glean.toQuery)) x3))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.FileXRefMap where
  toQueryId = Glean.Schema.Query.Cxx1.Types.FileXRefMap_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.FileXRefMap_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.FileXRefMap = Glean.Schema.Cxx1.Types.FileXRefMap
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.FileXRefMap = Glean.Schema.Query.Cxx1.Types.FileXRefMap

instance Glean.ToQuery Glean.Schema.Cxx1.Types.FileXRefMap

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.EnumDeclaration_key = Glean.Schema.Cxx1.Types.EnumDeclaration_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.EnumDeclaration_key = Glean.Schema.Query.Cxx1.Types.EnumDeclaration_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.EnumDeclaration_key where
  toQuery (Glean.Schema.Cxx1.Types.EnumDeclaration_key x1 x2 x3 x4) = Glean.Schema.Query.Cxx1.Types.EnumDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Cxx1.Types.enumDeclaration_type_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Cxx1.Types.enumDeclaration_type_just = Prelude.Just (Glean.toQuery x)})) x3)) (Prelude.Just (Glean.toQuery x4))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.EnumDeclaration where
  toQueryId = Glean.Schema.Query.Cxx1.Types.EnumDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.EnumDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.EnumDeclaration = Glean.Schema.Cxx1.Types.EnumDeclaration
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.EnumDeclaration = Glean.Schema.Query.Cxx1.Types.EnumDeclaration

instance Glean.ToQuery Glean.Schema.Cxx1.Types.EnumDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcContainerDeclaration_key = Glean.Schema.Cxx1.Types.ObjcContainerDeclaration_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcContainerDeclaration_key = Glean.Schema.Query.Cxx1.Types.ObjcContainerDeclaration_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcContainerDeclaration_key where
  toQuery (Glean.Schema.Cxx1.Types.ObjcContainerDeclaration_key x1 x2) = Glean.Schema.Query.Cxx1.Types.ObjcContainerDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.ObjcContainerDeclaration where
  toQueryId = Glean.Schema.Query.Cxx1.Types.ObjcContainerDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.ObjcContainerDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcContainerDeclaration = Glean.Schema.Cxx1.Types.ObjcContainerDeclaration
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcContainerDeclaration = Glean.Schema.Query.Cxx1.Types.ObjcContainerDeclaration

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcContainerDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcMethodDeclaration_key = Glean.Schema.Cxx1.Types.ObjcMethodDeclaration_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcMethodDeclaration_key = Glean.Schema.Query.Cxx1.Types.ObjcMethodDeclaration_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcMethodDeclaration_key where
  toQuery (Glean.Schema.Cxx1.Types.ObjcMethodDeclaration_key x1 x2 x3 x4 x5 x6 x7) = Glean.Schema.Query.Cxx1.Types.ObjcMethodDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just (Glean.toQuery x4)) (Prelude.Just (Glean.toQuery x5)) (Prelude.Just (Glean.toQuery x6)) (Prelude.Just (Glean.toQuery x7))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.ObjcMethodDeclaration where
  toQueryId = Glean.Schema.Query.Cxx1.Types.ObjcMethodDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.ObjcMethodDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcMethodDeclaration = Glean.Schema.Cxx1.Types.ObjcMethodDeclaration
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcMethodDeclaration = Glean.Schema.Query.Cxx1.Types.ObjcMethodDeclaration

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcMethodDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.MethodOverrides_key = Glean.Schema.Cxx1.Types.MethodOverrides_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.MethodOverrides_key = Glean.Schema.Query.Cxx1.Types.MethodOverrides_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.MethodOverrides_key where
  toQuery (Glean.Schema.Cxx1.Types.MethodOverrides_key x1 x2) = Glean.Schema.Query.Cxx1.Types.MethodOverrides_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.MethodOverrides where
  toQueryId = Glean.Schema.Query.Cxx1.Types.MethodOverrides_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.MethodOverrides_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.MethodOverrides = Glean.Schema.Cxx1.Types.MethodOverrides
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.MethodOverrides = Glean.Schema.Query.Cxx1.Types.MethodOverrides

instance Glean.ToQuery Glean.Schema.Cxx1.Types.MethodOverrides

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcInterfaceToImplementation_key = Glean.Schema.Cxx1.Types.ObjcInterfaceToImplementation_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcInterfaceToImplementation_key = Glean.Schema.Query.Cxx1.Types.ObjcInterfaceToImplementation_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcInterfaceToImplementation_key where
  toQuery (Glean.Schema.Cxx1.Types.ObjcInterfaceToImplementation_key x1 x2) = Glean.Schema.Query.Cxx1.Types.ObjcInterfaceToImplementation_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.ObjcInterfaceToImplementation where
  toQueryId = Glean.Schema.Query.Cxx1.Types.ObjcInterfaceToImplementation_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.ObjcInterfaceToImplementation_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcInterfaceToImplementation = Glean.Schema.Cxx1.Types.ObjcInterfaceToImplementation
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcInterfaceToImplementation = Glean.Schema.Query.Cxx1.Types.ObjcInterfaceToImplementation

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcInterfaceToImplementation

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.FunctionName_key = Glean.Schema.Cxx1.Types.FunctionName_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.FunctionName_key = Glean.Schema.Query.Cxx1.Types.FunctionName_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.FunctionName_key where
  toQuery (Glean.Schema.Cxx1.Types.FunctionName_key_name x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.functionName_key_name = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.FunctionName_key_operator_ x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.functionName_key_operator_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.FunctionName_key_literalOperator x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.functionName_key_literalOperator = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.FunctionName_key_constructor x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.functionName_key_constructor = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.FunctionName_key_destructor x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.functionName_key_destructor = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.FunctionName_key_conversionOperator x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.functionName_key_conversionOperator = Prelude.Just (Glean.toQuery x) }

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.FunctionName where
  toQueryId = Glean.Schema.Query.Cxx1.Types.FunctionName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.FunctionName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.FunctionName = Glean.Schema.Cxx1.Types.FunctionName
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.FunctionName = Glean.Schema.Query.Cxx1.Types.FunctionName

instance Glean.ToQuery Glean.Schema.Cxx1.Types.FunctionName

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.Same_key = Glean.Schema.Cxx1.Types.Same_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.Same_key = Glean.Schema.Query.Cxx1.Types.Same_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.Same_key where
  toQuery (Glean.Schema.Cxx1.Types.Same_key x1 x2) = Glean.Schema.Query.Cxx1.Types.Same_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.Same where
  toQueryId = Glean.Schema.Query.Cxx1.Types.Same_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.Same_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.Same = Glean.Schema.Cxx1.Types.Same
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.Same = Glean.Schema.Query.Cxx1.Types.Same

instance Glean.ToQuery Glean.Schema.Cxx1.Types.Same

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.FunctionDeclaration_key = Glean.Schema.Cxx1.Types.FunctionDeclaration_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.FunctionDeclaration_key = Glean.Schema.Query.Cxx1.Types.FunctionDeclaration_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.FunctionDeclaration_key where
  toQuery (Glean.Schema.Cxx1.Types.FunctionDeclaration_key x1 x2 x3 x4) = Glean.Schema.Query.Cxx1.Types.FunctionDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Cxx1.Types.functionDeclaration_method_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Cxx1.Types.functionDeclaration_method_just = Prelude.Just (Glean.toQuery x)})) x3)) (Prelude.Just (Glean.toQuery x4))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.FunctionDeclaration where
  toQueryId = Glean.Schema.Query.Cxx1.Types.FunctionDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.FunctionDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.FunctionDeclaration = Glean.Schema.Cxx1.Types.FunctionDeclaration
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.FunctionDeclaration = Glean.Schema.Query.Cxx1.Types.FunctionDeclaration

instance Glean.ToQuery Glean.Schema.Cxx1.Types.FunctionDeclaration

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.Name where
  toQueryId = Glean.Schema.Query.Cxx1.Types.Name_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.Name_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.Name = Glean.Schema.Cxx1.Types.Name
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.Name = Glean.Schema.Query.Cxx1.Types.Name

instance Glean.ToQuery Glean.Schema.Cxx1.Types.Name

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.DeclImpliesRecord_key = Glean.Schema.Cxx1.Types.DeclImpliesRecord_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.DeclImpliesRecord_key = Glean.Schema.Query.Cxx1.Types.DeclImpliesRecord_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.DeclImpliesRecord_key where
  toQuery (Glean.Schema.Cxx1.Types.DeclImpliesRecord_key x1 x2) = Glean.Schema.Query.Cxx1.Types.DeclImpliesRecord_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.DeclImpliesRecord where
  toQueryId = Glean.Schema.Query.Cxx1.Types.DeclImpliesRecord_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.DeclImpliesRecord_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.DeclImpliesRecord = Glean.Schema.Cxx1.Types.DeclImpliesRecord
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.DeclImpliesRecord = Glean.Schema.Query.Cxx1.Types.DeclImpliesRecord

instance Glean.ToQuery Glean.Schema.Cxx1.Types.DeclImpliesRecord

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.FunctionQName_key = Glean.Schema.Cxx1.Types.FunctionQName_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.FunctionQName_key = Glean.Schema.Query.Cxx1.Types.FunctionQName_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.FunctionQName_key where
  toQuery (Glean.Schema.Cxx1.Types.FunctionQName_key x1 x2) = Glean.Schema.Query.Cxx1.Types.FunctionQName_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.FunctionQName where
  toQueryId = Glean.Schema.Query.Cxx1.Types.FunctionQName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.FunctionQName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.FunctionQName = Glean.Schema.Cxx1.Types.FunctionQName
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.FunctionQName = Glean.Schema.Query.Cxx1.Types.FunctionQName

instance Glean.ToQuery Glean.Schema.Cxx1.Types.FunctionQName

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.TranslationUnitTrace_key = Glean.Schema.Cxx1.Types.TranslationUnitTrace_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.TranslationUnitTrace_key = Glean.Schema.Query.Cxx1.Types.TranslationUnitTrace_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.TranslationUnitTrace_key where
  toQuery (Glean.Schema.Cxx1.Types.TranslationUnitTrace_key x1 x2) = Glean.Schema.Query.Cxx1.Types.TranslationUnitTrace_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.TranslationUnitTrace where
  toQueryId = Glean.Schema.Query.Cxx1.Types.TranslationUnitTrace_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.TranslationUnitTrace_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.TranslationUnitTrace = Glean.Schema.Cxx1.Types.TranslationUnitTrace
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.TranslationUnitTrace = Glean.Schema.Query.Cxx1.Types.TranslationUnitTrace

instance Glean.ToQuery Glean.Schema.Cxx1.Types.TranslationUnitTrace

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.Enumerator_key = Glean.Schema.Cxx1.Types.Enumerator_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.Enumerator_key = Glean.Schema.Query.Cxx1.Types.Enumerator_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.Enumerator_key where
  toQuery (Glean.Schema.Cxx1.Types.Enumerator_key x1 x2 x3) = Glean.Schema.Query.Cxx1.Types.Enumerator_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.Enumerator where
  toQueryId = Glean.Schema.Query.Cxx1.Types.Enumerator_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.Enumerator_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.Enumerator = Glean.Schema.Cxx1.Types.Enumerator
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.Enumerator = Glean.Schema.Query.Cxx1.Types.Enumerator

instance Glean.ToQuery Glean.Schema.Cxx1.Types.Enumerator

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcPropertyImplementation_key = Glean.Schema.Cxx1.Types.ObjcPropertyImplementation_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcPropertyImplementation_key = Glean.Schema.Query.Cxx1.Types.ObjcPropertyImplementation_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcPropertyImplementation_key where
  toQuery (Glean.Schema.Cxx1.Types.ObjcPropertyImplementation_key x1 x2 x3 x4) = Glean.Schema.Query.Cxx1.Types.ObjcPropertyImplementation_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Cxx1.Types.objcPropertyImplementation_ivar_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Cxx1.Types.objcPropertyImplementation_ivar_just = Prelude.Just (Glean.toQuery x)})) x3)) (Prelude.Just (Glean.toQuery x4))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.ObjcPropertyImplementation where
  toQueryId = Glean.Schema.Query.Cxx1.Types.ObjcPropertyImplementation_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.ObjcPropertyImplementation_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcPropertyImplementation = Glean.Schema.Cxx1.Types.ObjcPropertyImplementation
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcPropertyImplementation = Glean.Schema.Query.Cxx1.Types.ObjcPropertyImplementation

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcPropertyImplementation

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.Trace_key = Glean.Schema.Cxx1.Types.Trace_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.Trace_key = Glean.Schema.Query.Cxx1.Types.Trace_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.Trace_key where
  toQuery (Glean.Schema.Cxx1.Types.Trace_key x1 x2 x3) = Glean.Schema.Query.Cxx1.Types.Trace_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.Trace where
  toQueryId = Glean.Schema.Query.Cxx1.Types.Trace_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.Trace_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.Trace = Glean.Schema.Cxx1.Types.Trace
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.Trace = Glean.Schema.Query.Cxx1.Types.Trace

instance Glean.ToQuery Glean.Schema.Cxx1.Types.Trace

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.MethodOverridden_key = Glean.Schema.Cxx1.Types.MethodOverridden_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.MethodOverridden_key = Glean.Schema.Query.Cxx1.Types.MethodOverridden_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.MethodOverridden_key where
  toQuery (Glean.Schema.Cxx1.Types.MethodOverridden_key x1 x2) = Glean.Schema.Query.Cxx1.Types.MethodOverridden_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.MethodOverridden where
  toQueryId = Glean.Schema.Query.Cxx1.Types.MethodOverridden_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.MethodOverridden_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.MethodOverridden = Glean.Schema.Cxx1.Types.MethodOverridden
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.MethodOverridden = Glean.Schema.Query.Cxx1.Types.MethodOverridden

instance Glean.ToQuery Glean.Schema.Cxx1.Types.MethodOverridden

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.TargetUses_key = Glean.Schema.Cxx1.Types.TargetUses_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.TargetUses_key = Glean.Schema.Query.Cxx1.Types.TargetUses_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.TargetUses_key where
  toQuery (Glean.Schema.Cxx1.Types.TargetUses_key x1 x2 x3) = Glean.Schema.Query.Cxx1.Types.TargetUses_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Glean.Schema.Query.Src.Types.ByteSpans_exact . Prelude.map Glean.toQuery) x3))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.TargetUses where
  toQueryId = Glean.Schema.Query.Cxx1.Types.TargetUses_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.TargetUses_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.TargetUses = Glean.Schema.Cxx1.Types.TargetUses
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.TargetUses = Glean.Schema.Query.Cxx1.Types.TargetUses

instance Glean.ToQuery Glean.Schema.Cxx1.Types.TargetUses

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.UsingDirective_key = Glean.Schema.Cxx1.Types.UsingDirective_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.UsingDirective_key = Glean.Schema.Query.Cxx1.Types.UsingDirective_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.UsingDirective_key where
  toQuery (Glean.Schema.Cxx1.Types.UsingDirective_key x1 x2) = Glean.Schema.Query.Cxx1.Types.UsingDirective_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.UsingDirective where
  toQueryId = Glean.Schema.Query.Cxx1.Types.UsingDirective_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.UsingDirective_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.UsingDirective = Glean.Schema.Cxx1.Types.UsingDirective
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.UsingDirective = Glean.Schema.Query.Cxx1.Types.UsingDirective

instance Glean.ToQuery Glean.Schema.Cxx1.Types.UsingDirective

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.DeclFamilyOf_key = Glean.Schema.Cxx1.Types.DeclFamilyOf_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.DeclFamilyOf_key = Glean.Schema.Query.Cxx1.Types.DeclFamilyOf_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.DeclFamilyOf_key where
  toQuery (Glean.Schema.Cxx1.Types.DeclFamilyOf_key x1 x2) = Glean.Schema.Query.Cxx1.Types.DeclFamilyOf_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.DeclFamilyOf where
  toQueryId = Glean.Schema.Query.Cxx1.Types.DeclFamilyOf_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.DeclFamilyOf_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.DeclFamilyOf = Glean.Schema.Cxx1.Types.DeclFamilyOf
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.DeclFamilyOf = Glean.Schema.Query.Cxx1.Types.DeclFamilyOf

instance Glean.ToQuery Glean.Schema.Cxx1.Types.DeclFamilyOf

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.PPTrace_key = Glean.Schema.Cxx1.Types.PPTrace_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.PPTrace_key = Glean.Schema.Query.Cxx1.Types.PPTrace_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.PPTrace_key where
  toQuery (Glean.Schema.Cxx1.Types.PPTrace_key x1 x2) = Glean.Schema.Query.Cxx1.Types.PPTrace_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Cxx1.Types.PPTrace_events_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.PPTrace where
  toQueryId = Glean.Schema.Query.Cxx1.Types.PPTrace_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.PPTrace_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.PPTrace = Glean.Schema.Cxx1.Types.PPTrace
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.PPTrace = Glean.Schema.Query.Cxx1.Types.PPTrace

instance Glean.ToQuery Glean.Schema.Cxx1.Types.PPTrace

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.QName_key = Glean.Schema.Cxx1.Types.QName_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.QName_key = Glean.Schema.Query.Cxx1.Types.QName_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.QName_key where
  toQuery (Glean.Schema.Cxx1.Types.QName_key x1 x2) = Glean.Schema.Query.Cxx1.Types.QName_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.QName where
  toQueryId = Glean.Schema.Query.Cxx1.Types.QName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.QName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.QName = Glean.Schema.Cxx1.Types.QName
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.QName = Glean.Schema.Query.Cxx1.Types.QName

instance Glean.ToQuery Glean.Schema.Cxx1.Types.QName

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.VariableDeclaration_key = Glean.Schema.Cxx1.Types.VariableDeclaration_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.VariableDeclaration_key = Glean.Schema.Query.Cxx1.Types.VariableDeclaration_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.VariableDeclaration_key where
  toQuery (Glean.Schema.Cxx1.Types.VariableDeclaration_key x1 x2 x3 x4) = Glean.Schema.Query.Cxx1.Types.VariableDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just (Glean.toQuery x4))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.VariableDeclaration where
  toQueryId = Glean.Schema.Query.Cxx1.Types.VariableDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.VariableDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.VariableDeclaration = Glean.Schema.Cxx1.Types.VariableDeclaration
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.VariableDeclaration = Glean.Schema.Query.Cxx1.Types.VariableDeclaration

instance Glean.ToQuery Glean.Schema.Cxx1.Types.VariableDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.DeclarationSources_key = Glean.Schema.Cxx1.Types.DeclarationSources_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.DeclarationSources_key = Glean.Schema.Query.Cxx1.Types.DeclarationSources_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.DeclarationSources_key where
  toQuery (Glean.Schema.Cxx1.Types.DeclarationSources_key x1 x2) = Glean.Schema.Query.Cxx1.Types.DeclarationSources_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Cxx1.Types.DeclarationSources_sources_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.DeclarationSources where
  toQueryId = Glean.Schema.Query.Cxx1.Types.DeclarationSources_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.DeclarationSources_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.DeclarationSources = Glean.Schema.Cxx1.Types.DeclarationSources
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.DeclarationSources = Glean.Schema.Query.Cxx1.Types.DeclarationSources

instance Glean.ToQuery Glean.Schema.Cxx1.Types.DeclarationSources

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcContainerBase_key = Glean.Schema.Cxx1.Types.ObjcContainerBase_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcContainerBase_key = Glean.Schema.Query.Cxx1.Types.ObjcContainerBase_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcContainerBase_key where
  toQuery (Glean.Schema.Cxx1.Types.ObjcContainerBase_key x1 x2) = Glean.Schema.Query.Cxx1.Types.ObjcContainerBase_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.ObjcContainerBase where
  toQueryId = Glean.Schema.Query.Cxx1.Types.ObjcContainerBase_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.ObjcContainerBase_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcContainerBase = Glean.Schema.Cxx1.Types.ObjcContainerBase
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcContainerBase = Glean.Schema.Query.Cxx1.Types.ObjcContainerBase

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcContainerBase

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.UsingDeclaration_key = Glean.Schema.Cxx1.Types.UsingDeclaration_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.UsingDeclaration_key = Glean.Schema.Query.Cxx1.Types.UsingDeclaration_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.UsingDeclaration_key where
  toQuery (Glean.Schema.Cxx1.Types.UsingDeclaration_key x1 x2) = Glean.Schema.Query.Cxx1.Types.UsingDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.UsingDeclaration where
  toQueryId = Glean.Schema.Query.Cxx1.Types.UsingDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.UsingDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.UsingDeclaration = Glean.Schema.Cxx1.Types.UsingDeclaration
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.UsingDeclaration = Glean.Schema.Query.Cxx1.Types.UsingDeclaration

instance Glean.ToQuery Glean.Schema.Cxx1.Types.UsingDeclaration

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.ObjcMethodDefinition where
  toQueryId = Glean.Schema.Query.Cxx1.Types.ObjcMethodDefinition_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.ObjcMethodDefinition_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcMethodDefinition = Glean.Schema.Cxx1.Types.ObjcMethodDefinition
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcMethodDefinition = Glean.Schema.Query.Cxx1.Types.ObjcMethodDefinition

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcMethodDefinition

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.DeclarationInTrace_key = Glean.Schema.Cxx1.Types.DeclarationInTrace_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.DeclarationInTrace_key = Glean.Schema.Query.Cxx1.Types.DeclarationInTrace_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.DeclarationInTrace_key where
  toQuery (Glean.Schema.Cxx1.Types.DeclarationInTrace_key x1 x2) = Glean.Schema.Query.Cxx1.Types.DeclarationInTrace_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.DeclarationInTrace where
  toQueryId = Glean.Schema.Query.Cxx1.Types.DeclarationInTrace_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.DeclarationInTrace_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.DeclarationInTrace = Glean.Schema.Cxx1.Types.DeclarationInTrace
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.DeclarationInTrace = Glean.Schema.Query.Cxx1.Types.DeclarationInTrace

instance Glean.ToQuery Glean.Schema.Cxx1.Types.DeclarationInTrace

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcContainerDefinition_key = Glean.Schema.Cxx1.Types.ObjcContainerDefinition_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcContainerDefinition_key = Glean.Schema.Query.Cxx1.Types.ObjcContainerDefinition_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcContainerDefinition_key where
  toQuery (Glean.Schema.Cxx1.Types.ObjcContainerDefinition_key x1 x2 x3) = Glean.Schema.Query.Cxx1.Types.ObjcContainerDefinition_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Cxx1.Types.ObjcContainerDefinition_protocols_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.ObjcContainerDefinition where
  toQueryId = Glean.Schema.Query.Cxx1.Types.ObjcContainerDefinition_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.ObjcContainerDefinition_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcContainerDefinition = Glean.Schema.Cxx1.Types.ObjcContainerDefinition
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcContainerDefinition = Glean.Schema.Query.Cxx1.Types.ObjcContainerDefinition

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcContainerDefinition

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.EnumDefinition_key = Glean.Schema.Cxx1.Types.EnumDefinition_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.EnumDefinition_key = Glean.Schema.Query.Cxx1.Types.EnumDefinition_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.EnumDefinition_key where
  toQuery (Glean.Schema.Cxx1.Types.EnumDefinition_key x1 x2) = Glean.Schema.Query.Cxx1.Types.EnumDefinition_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Cxx1.Types.EnumDefinition_enumerators_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.EnumDefinition where
  toQueryId = Glean.Schema.Query.Cxx1.Types.EnumDefinition_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.EnumDefinition_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.EnumDefinition = Glean.Schema.Cxx1.Types.EnumDefinition
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.EnumDefinition = Glean.Schema.Query.Cxx1.Types.EnumDefinition

instance Glean.ToQuery Glean.Schema.Cxx1.Types.EnumDefinition

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.Declarations where
  toQueryId = Glean.Schema.Query.Cxx1.Types.Declarations_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.Declarations_with_key . (Glean.Schema.Query.Cxx1.Types.Declarations_array_exact . Prelude.map Glean.toQuery)

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.Declarations = Glean.Schema.Cxx1.Types.Declarations
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.Declarations = Glean.Schema.Query.Cxx1.Types.Declarations

instance Glean.ToQuery Glean.Schema.Cxx1.Types.Declarations

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.DeclToFamily_key = Glean.Schema.Cxx1.Types.DeclToFamily_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.DeclToFamily_key = Glean.Schema.Query.Cxx1.Types.DeclToFamily_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.DeclToFamily_key where
  toQuery (Glean.Schema.Cxx1.Types.DeclToFamily_key x1 x2) = Glean.Schema.Query.Cxx1.Types.DeclToFamily_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.DeclToFamily where
  toQueryId = Glean.Schema.Query.Cxx1.Types.DeclToFamily_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.DeclToFamily_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.DeclToFamily = Glean.Schema.Cxx1.Types.DeclToFamily
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.DeclToFamily = Glean.Schema.Query.Cxx1.Types.DeclToFamily

instance Glean.ToQuery Glean.Schema.Cxx1.Types.DeclToFamily

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.NamespaceDeclaration_key = Glean.Schema.Cxx1.Types.NamespaceDeclaration_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.NamespaceDeclaration_key = Glean.Schema.Query.Cxx1.Types.NamespaceDeclaration_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.NamespaceDeclaration_key where
  toQuery (Glean.Schema.Cxx1.Types.NamespaceDeclaration_key x1 x2) = Glean.Schema.Query.Cxx1.Types.NamespaceDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.NamespaceDeclaration where
  toQueryId = Glean.Schema.Query.Cxx1.Types.NamespaceDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.NamespaceDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.NamespaceDeclaration = Glean.Schema.Cxx1.Types.NamespaceDeclaration
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.NamespaceDeclaration = Glean.Schema.Query.Cxx1.Types.NamespaceDeclaration

instance Glean.ToQuery Glean.Schema.Cxx1.Types.NamespaceDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.DeclarationComment_key = Glean.Schema.Cxx1.Types.DeclarationComment_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.DeclarationComment_key = Glean.Schema.Query.Cxx1.Types.DeclarationComment_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.DeclarationComment_key where
  toQuery (Glean.Schema.Cxx1.Types.DeclarationComment_key x1 x2 x3) = Glean.Schema.Query.Cxx1.Types.DeclarationComment_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.DeclarationComment where
  toQueryId = Glean.Schema.Query.Cxx1.Types.DeclarationComment_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.DeclarationComment_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.DeclarationComment = Glean.Schema.Cxx1.Types.DeclarationComment
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.DeclarationComment = Glean.Schema.Query.Cxx1.Types.DeclarationComment

instance Glean.ToQuery Glean.Schema.Cxx1.Types.DeclarationComment

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcPropertyDeclaration_key = Glean.Schema.Cxx1.Types.ObjcPropertyDeclaration_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcPropertyDeclaration_key = Glean.Schema.Query.Cxx1.Types.ObjcPropertyDeclaration_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcPropertyDeclaration_key where
  toQuery (Glean.Schema.Cxx1.Types.ObjcPropertyDeclaration_key x1 x2 x3 x4 x5 x6 x7 x8) = Glean.Schema.Query.Cxx1.Types.ObjcPropertyDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just (Glean.toQuery x4)) (Prelude.Just (Glean.toQuery x5)) (Prelude.Just (Glean.toQuery x6)) (Prelude.Just (Glean.toQuery x7)) (Prelude.Just (Glean.toQuery x8))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.ObjcPropertyDeclaration where
  toQueryId = Glean.Schema.Query.Cxx1.Types.ObjcPropertyDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.ObjcPropertyDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcPropertyDeclaration = Glean.Schema.Cxx1.Types.ObjcPropertyDeclaration
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcPropertyDeclaration = Glean.Schema.Query.Cxx1.Types.ObjcPropertyDeclaration

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcPropertyDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.RecordDefinition_key = Glean.Schema.Cxx1.Types.RecordDefinition_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.RecordDefinition_key = Glean.Schema.Query.Cxx1.Types.RecordDefinition_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.RecordDefinition_key where
  toQuery (Glean.Schema.Cxx1.Types.RecordDefinition_key x1 x2 x3) = Glean.Schema.Query.Cxx1.Types.RecordDefinition_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Cxx1.Types.RecordDefinition_bases_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.RecordDefinition where
  toQueryId = Glean.Schema.Query.Cxx1.Types.RecordDefinition_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.RecordDefinition_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.RecordDefinition = Glean.Schema.Cxx1.Types.RecordDefinition
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.RecordDefinition = Glean.Schema.Query.Cxx1.Types.RecordDefinition

instance Glean.ToQuery Glean.Schema.Cxx1.Types.RecordDefinition

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcImplements_key = Glean.Schema.Cxx1.Types.ObjcImplements_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcImplements_key = Glean.Schema.Query.Cxx1.Types.ObjcImplements_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcImplements_key where
  toQuery (Glean.Schema.Cxx1.Types.ObjcImplements_key x1 x2) = Glean.Schema.Query.Cxx1.Types.ObjcImplements_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.ObjcImplements where
  toQueryId = Glean.Schema.Query.Cxx1.Types.ObjcImplements_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.ObjcImplements_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcImplements = Glean.Schema.Cxx1.Types.ObjcImplements
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcImplements = Glean.Schema.Query.Cxx1.Types.ObjcImplements

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcImplements

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.EnumeratorInEnum_key = Glean.Schema.Cxx1.Types.EnumeratorInEnum_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.EnumeratorInEnum_key = Glean.Schema.Query.Cxx1.Types.EnumeratorInEnum_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.EnumeratorInEnum_key where
  toQuery (Glean.Schema.Cxx1.Types.EnumeratorInEnum_key x1 x2) = Glean.Schema.Query.Cxx1.Types.EnumeratorInEnum_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.EnumeratorInEnum where
  toQueryId = Glean.Schema.Query.Cxx1.Types.EnumeratorInEnum_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.EnumeratorInEnum_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.EnumeratorInEnum = Glean.Schema.Cxx1.Types.EnumeratorInEnum
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.EnumeratorInEnum = Glean.Schema.Query.Cxx1.Types.EnumeratorInEnum

instance Glean.ToQuery Glean.Schema.Cxx1.Types.EnumeratorInEnum

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.DeclFamily where
  toQueryId = Glean.Schema.Query.Cxx1.Types.DeclFamily_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.DeclFamily_with_key . (Glean.Schema.Query.Cxx1.Types.DeclFamily_array_exact . Prelude.map Glean.toQuery)

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.DeclFamily = Glean.Schema.Cxx1.Types.DeclFamily
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.DeclFamily = Glean.Schema.Query.Cxx1.Types.DeclFamily

instance Glean.ToQuery Glean.Schema.Cxx1.Types.DeclFamily

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.Type where
  toQueryId = Glean.Schema.Query.Cxx1.Types.Type__with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.Type__with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.Type_ = Glean.Schema.Cxx1.Types.Type
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.Type = Glean.Schema.Query.Cxx1.Types.Type_

instance Glean.ToQuery Glean.Schema.Cxx1.Types.Type

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.RecordDerived_key = Glean.Schema.Cxx1.Types.RecordDerived_key
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.RecordDerived_key = Glean.Schema.Query.Cxx1.Types.RecordDerived_key

instance Glean.ToQuery Glean.Schema.Cxx1.Types.RecordDerived_key where
  toQuery (Glean.Schema.Cxx1.Types.RecordDerived_key x1 x2) = Glean.Schema.Query.Cxx1.Types.RecordDerived_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Cxx1.Types.RecordDerived where
  toQueryId = Glean.Schema.Query.Cxx1.Types.RecordDerived_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Cxx1.Types.RecordDerived_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.RecordDerived = Glean.Schema.Cxx1.Types.RecordDerived
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.RecordDerived = Glean.Schema.Query.Cxx1.Types.RecordDerived

instance Glean.ToQuery Glean.Schema.Cxx1.Types.RecordDerived

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.GlobalVariable = Glean.Schema.Cxx1.Types.GlobalVariable
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.GlobalVariable = Glean.Schema.Query.Cxx1.Types.GlobalVariable

instance Glean.ToQuery Glean.Schema.Cxx1.Types.GlobalVariable where
  toQuery (Glean.Schema.Cxx1.Types.GlobalVariable x1 x2 x3) = Glean.Schema.Query.Cxx1.Types.GlobalVariable (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.GlobalVariableKind = Glean.Schema.Cxx1.Types.GlobalVariableKind
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.GlobalVariableKind = Glean.Schema.Query.Cxx1.Types.GlobalVariableKind

instance Glean.ToQuery Glean.Schema.Cxx1.Types.GlobalVariableKind where
  toQuery Glean.Schema.Cxx1.Types.GlobalVariableKind_SimpleVariable = Glean.Schema.Query.Cxx1.Types.GlobalVariableKind_SimpleVariable
  toQuery Glean.Schema.Cxx1.Types.GlobalVariableKind_StaticVariable = Glean.Schema.Query.Cxx1.Types.GlobalVariableKind_StaticVariable
  toQuery Glean.Schema.Cxx1.Types.GlobalVariableKind_StaticMember = Glean.Schema.Query.Cxx1.Types.GlobalVariableKind_StaticMember

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.Scope = Glean.Schema.Cxx1.Types.Scope
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.Scope = Glean.Schema.Query.Cxx1.Types.Scope

instance Glean.ToQuery Glean.Schema.Cxx1.Types.Scope where
  toQuery (Glean.Schema.Cxx1.Types.Scope_global_ x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.scope_global_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.Scope_namespace_ x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.scope_namespace_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.Scope_recordWithAccess x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.scope_recordWithAccess = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.Scope_local x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.scope_local = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Builtin.Types.Unit Glean.Schema.Query.Cxx1.Types.Scope where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.scope_global_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.NamespaceQName Glean.Schema.Query.Cxx1.Types.Scope where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.scope_namespace_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.Scope_recordWithAccess_ Glean.Schema.Query.Cxx1.Types.Scope where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.scope_recordWithAccess = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.FunctionQName Glean.Schema.Query.Cxx1.Types.Scope where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.scope_local = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.FixedXRef = Glean.Schema.Cxx1.Types.FixedXRef
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.FixedXRef = Glean.Schema.Query.Cxx1.Types.FixedXRef

instance Glean.ToQuery Glean.Schema.Cxx1.Types.FixedXRef where
  toQuery (Glean.Schema.Cxx1.Types.FixedXRef x1 x2) = Glean.Schema.Query.Cxx1.Types.FixedXRef (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Src.Types.ByteSpans_exact . Prelude.map Glean.toQuery) x2))

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.PPEvent = Glean.Schema.Cxx1.Types.PPEvent
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.PPEvent = Glean.Schema.Query.Cxx1.Types.PPEvent

instance Glean.ToQuery Glean.Schema.Cxx1.Types.PPEvent where
  toQuery (Glean.Schema.Cxx1.Types.PPEvent_include_ x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.pPEvent_include_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.PPEvent_define x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.pPEvent_define = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.PPEvent_undef x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.pPEvent_undef = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.PPEvent_use x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.pPEvent_use = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.IncludeTrace Glean.Schema.Query.Cxx1.Types.PPEvent where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.pPEvent_include_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Pp1.Types.Define Glean.Schema.Query.Cxx1.Types.PPEvent where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.pPEvent_define = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Pp1.Types.Undef Glean.Schema.Query.Cxx1.Types.PPEvent where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.pPEvent_undef = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Pp1.Types.Use Glean.Schema.Query.Cxx1.Types.PPEvent where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.pPEvent_use = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.Access = Glean.Schema.Cxx1.Types.Access
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.Access = Glean.Schema.Query.Cxx1.Types.Access

instance Glean.ToQuery Glean.Schema.Cxx1.Types.Access where
  toQuery Glean.Schema.Cxx1.Types.Access_Public = Glean.Schema.Query.Cxx1.Types.Access_Public
  toQuery Glean.Schema.Cxx1.Types.Access_Protected = Glean.Schema.Query.Cxx1.Types.Access_Protected
  toQuery Glean.Schema.Cxx1.Types.Access_Private = Glean.Schema.Query.Cxx1.Types.Access_Private

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.TypeAliasKind = Glean.Schema.Cxx1.Types.TypeAliasKind
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.TypeAliasKind = Glean.Schema.Query.Cxx1.Types.TypeAliasKind

instance Glean.ToQuery Glean.Schema.Cxx1.Types.TypeAliasKind where
  toQuery Glean.Schema.Cxx1.Types.TypeAliasKind_Typedef = Glean.Schema.Query.Cxx1.Types.TypeAliasKind_Typedef
  toQuery Glean.Schema.Cxx1.Types.TypeAliasKind_Using = Glean.Schema.Query.Cxx1.Types.TypeAliasKind_Using

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcContainerId = Glean.Schema.Cxx1.Types.ObjcContainerId
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcContainerId = Glean.Schema.Query.Cxx1.Types.ObjcContainerId

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcContainerId where
  toQuery (Glean.Schema.Cxx1.Types.ObjcContainerId_protocol x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.objcContainerId_protocol = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.ObjcContainerId_interface_ x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.objcContainerId_interface_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.ObjcContainerId_categoryInterface x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.objcContainerId_categoryInterface = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.ObjcContainerId_extensionInterface x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.objcContainerId_extensionInterface = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.ObjcContainerId_implementation x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.objcContainerId_implementation = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.ObjcContainerId_categoryImplementation x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.objcContainerId_categoryImplementation = Prelude.Just (Glean.toQuery x) }

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.DeclKind = Glean.Schema.Cxx1.Types.DeclKind
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.DeclKind = Glean.Schema.Query.Cxx1.Types.DeclKind

instance Glean.ToQuery Glean.Schema.Cxx1.Types.DeclKind where
  toQuery Glean.Schema.Cxx1.Types.DeclKind_namespace_ = Glean.Schema.Query.Cxx1.Types.DeclKind_namespace_
  toQuery Glean.Schema.Cxx1.Types.DeclKind_usingDeclaration = Glean.Schema.Query.Cxx1.Types.DeclKind_usingDeclaration
  toQuery Glean.Schema.Cxx1.Types.DeclKind_usingDirective = Glean.Schema.Query.Cxx1.Types.DeclKind_usingDirective
  toQuery Glean.Schema.Cxx1.Types.DeclKind_record_ = Glean.Schema.Query.Cxx1.Types.DeclKind_record_
  toQuery Glean.Schema.Cxx1.Types.DeclKind_enum_ = Glean.Schema.Query.Cxx1.Types.DeclKind_enum_
  toQuery Glean.Schema.Cxx1.Types.DeclKind_enumerator = Glean.Schema.Query.Cxx1.Types.DeclKind_enumerator
  toQuery Glean.Schema.Cxx1.Types.DeclKind_function_ = Glean.Schema.Query.Cxx1.Types.DeclKind_function_
  toQuery Glean.Schema.Cxx1.Types.DeclKind_variable = Glean.Schema.Query.Cxx1.Types.DeclKind_variable
  toQuery Glean.Schema.Cxx1.Types.DeclKind_objcContainer = Glean.Schema.Query.Cxx1.Types.DeclKind_objcContainer
  toQuery Glean.Schema.Cxx1.Types.DeclKind_objcMethod = Glean.Schema.Query.Cxx1.Types.DeclKind_objcMethod
  toQuery Glean.Schema.Cxx1.Types.DeclKind_objcProperty = Glean.Schema.Query.Cxx1.Types.DeclKind_objcProperty
  toQuery Glean.Schema.Cxx1.Types.DeclKind_typeAlias = Glean.Schema.Query.Cxx1.Types.DeclKind_typeAlias
  toQuery Glean.Schema.Cxx1.Types.DeclKind_macro = Glean.Schema.Query.Cxx1.Types.DeclKind_macro

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.MethodSignature = Glean.Schema.Cxx1.Types.MethodSignature
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.MethodSignature = Glean.Schema.Query.Cxx1.Types.MethodSignature

instance Glean.ToQuery Glean.Schema.Cxx1.Types.MethodSignature where
  toQuery (Glean.Schema.Cxx1.Types.MethodSignature x1 x2 x3 x4) = Glean.Schema.Query.Cxx1.Types.MethodSignature (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just (Glean.toQuery x4))

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcPropertyKind = Glean.Schema.Cxx1.Types.ObjcPropertyKind
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcPropertyKind = Glean.Schema.Query.Cxx1.Types.ObjcPropertyKind

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcPropertyKind where
  toQuery Glean.Schema.Cxx1.Types.ObjcPropertyKind_Synthesize = Glean.Schema.Query.Cxx1.Types.ObjcPropertyKind_Synthesize
  toQuery Glean.Schema.Cxx1.Types.ObjcPropertyKind_Dynamic = Glean.Schema.Query.Cxx1.Types.ObjcPropertyKind_Dynamic

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.Declaration = Glean.Schema.Cxx1.Types.Declaration
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.Declaration = Glean.Schema.Query.Cxx1.Types.Declaration

instance Glean.ToQuery Glean.Schema.Cxx1.Types.Declaration where
  toQuery (Glean.Schema.Cxx1.Types.Declaration_namespace_ x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.declaration_namespace_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.Declaration_usingDeclaration x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.declaration_usingDeclaration = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.Declaration_usingDirective x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.declaration_usingDirective = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.Declaration_record_ x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.declaration_record_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.Declaration_enum_ x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.declaration_enum_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.Declaration_function_ x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.declaration_function_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.Declaration_variable x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.declaration_variable = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.Declaration_objcContainer x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.declaration_objcContainer = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.Declaration_objcMethod x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.declaration_objcMethod = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.Declaration_objcProperty x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.declaration_objcProperty = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.Declaration_typeAlias x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.declaration_typeAlias = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.NamespaceDeclaration Glean.Schema.Query.Cxx1.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.declaration_namespace_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.UsingDeclaration Glean.Schema.Query.Cxx1.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.declaration_usingDeclaration = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.UsingDirective Glean.Schema.Query.Cxx1.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.declaration_usingDirective = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.RecordDeclaration Glean.Schema.Query.Cxx1.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.declaration_record_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.EnumDeclaration Glean.Schema.Query.Cxx1.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.declaration_enum_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.FunctionDeclaration Glean.Schema.Query.Cxx1.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.declaration_function_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.VariableDeclaration Glean.Schema.Query.Cxx1.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.declaration_variable = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.ObjcContainerDeclaration Glean.Schema.Query.Cxx1.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.declaration_objcContainer = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.ObjcMethodDeclaration Glean.Schema.Query.Cxx1.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.declaration_objcMethod = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.ObjcPropertyDeclaration Glean.Schema.Query.Cxx1.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.declaration_objcProperty = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.TypeAliasDeclaration Glean.Schema.Query.Cxx1.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.declaration_typeAlias = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcIVar = Glean.Schema.Cxx1.Types.ObjcIVar
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcIVar = Glean.Schema.Query.Cxx1.Types.ObjcIVar

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcIVar where
  toQuery (Glean.Schema.Cxx1.Types.ObjcIVar x1 x2) = Glean.Schema.Query.Cxx1.Types.ObjcIVar (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Cxx1.Types.objcIVar_bitsize_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Cxx1.Types.objcIVar_bitsize_just = Prelude.Just (Glean.toQuery x)})) x2))

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.RecordKind = Glean.Schema.Cxx1.Types.RecordKind
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.RecordKind = Glean.Schema.Query.Cxx1.Types.RecordKind

instance Glean.ToQuery Glean.Schema.Cxx1.Types.RecordKind where
  toQuery (Glean.Schema.Cxx1.Types.RecordKind_struct_ x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.recordKind_struct_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.RecordKind_class_ x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.recordKind_class_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.RecordKind_union_ x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.recordKind_union_ = Prelude.Just (Glean.toQuery x) }

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.IncludeTrace = Glean.Schema.Cxx1.Types.IncludeTrace
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.IncludeTrace = Glean.Schema.Query.Cxx1.Types.IncludeTrace

instance Glean.ToQuery Glean.Schema.Cxx1.Types.IncludeTrace where
  toQuery (Glean.Schema.Cxx1.Types.IncludeTrace x1 x2) = Glean.Schema.Query.Cxx1.Types.IncludeTrace (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Cxx1.Types.includeTrace_trace_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Cxx1.Types.includeTrace_trace_just = Prelude.Just (Glean.toQuery x)})) x2))

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.Field = Glean.Schema.Cxx1.Types.Field
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.Field = Glean.Schema.Query.Cxx1.Types.Field

instance Glean.ToQuery Glean.Schema.Cxx1.Types.Field where
  toQuery (Glean.Schema.Cxx1.Types.Field x1 x2) = Glean.Schema.Query.Cxx1.Types.Field (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Cxx1.Types.field_bitsize_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Cxx1.Types.field_bitsize_just = Prelude.Just (Glean.toQuery x)})) x2))

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.XRefVia = Glean.Schema.Cxx1.Types.XRefVia
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.XRefVia = Glean.Schema.Query.Cxx1.Types.XRefVia

instance Glean.ToQuery Glean.Schema.Cxx1.Types.XRefVia where
  toQuery (Glean.Schema.Cxx1.Types.XRefVia_usingDeclaration x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.xRefVia_usingDeclaration = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.XRefVia_usingDirective x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.xRefVia_usingDirective = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.XRefVia_macro x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.xRefVia_macro = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.UsingDeclaration Glean.Schema.Query.Cxx1.Types.XRefVia where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.xRefVia_usingDeclaration = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.UsingDirective Glean.Schema.Query.Cxx1.Types.XRefVia where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.xRefVia_usingDirective = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Pp1.Types.Use Glean.Schema.Query.Cxx1.Types.XRefVia where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.xRefVia_macro = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.DeclIdent = Glean.Schema.Cxx1.Types.DeclIdent
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.DeclIdent = Glean.Schema.Query.Cxx1.Types.DeclIdent

instance Glean.ToQuery Glean.Schema.Cxx1.Types.DeclIdent where
  toQuery (Glean.Schema.Cxx1.Types.DeclIdent_name x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.declIdent_name = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.DeclIdent_macro x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.declIdent_macro = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.DeclIdent_selector x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.declIdent_selector = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.Name Glean.Schema.Query.Cxx1.Types.DeclIdent where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.declIdent_name = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Pp1.Types.Macro Glean.Schema.Query.Cxx1.Types.DeclIdent where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.declIdent_macro = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.ObjcSelector Glean.Schema.Query.Cxx1.Types.DeclIdent where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.declIdent_selector = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.GlobalVariableAttribute = Glean.Schema.Cxx1.Types.GlobalVariableAttribute
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.GlobalVariableAttribute = Glean.Schema.Query.Cxx1.Types.GlobalVariableAttribute

instance Glean.ToQuery Glean.Schema.Cxx1.Types.GlobalVariableAttribute where
  toQuery Glean.Schema.Cxx1.Types.GlobalVariableAttribute_Plain = Glean.Schema.Query.Cxx1.Types.GlobalVariableAttribute_Plain
  toQuery Glean.Schema.Cxx1.Types.GlobalVariableAttribute_Inline = Glean.Schema.Query.Cxx1.Types.GlobalVariableAttribute_Inline
  toQuery Glean.Schema.Cxx1.Types.GlobalVariableAttribute_Constexpr = Glean.Schema.Query.Cxx1.Types.GlobalVariableAttribute_Constexpr

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.ObjcCategoryId = Glean.Schema.Cxx1.Types.ObjcCategoryId
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.ObjcCategoryId = Glean.Schema.Query.Cxx1.Types.ObjcCategoryId

instance Glean.ToQuery Glean.Schema.Cxx1.Types.ObjcCategoryId where
  toQuery (Glean.Schema.Cxx1.Types.ObjcCategoryId x1 x2) = Glean.Schema.Query.Cxx1.Types.ObjcCategoryId (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.Parameter = Glean.Schema.Cxx1.Types.Parameter
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.Parameter = Glean.Schema.Query.Cxx1.Types.Parameter

instance Glean.ToQuery Glean.Schema.Cxx1.Types.Parameter where
  toQuery (Glean.Schema.Cxx1.Types.Parameter x1 x2) = Glean.Schema.Query.Cxx1.Types.Parameter (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.RefQualifier = Glean.Schema.Cxx1.Types.RefQualifier
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.RefQualifier = Glean.Schema.Query.Cxx1.Types.RefQualifier

instance Glean.ToQuery Glean.Schema.Cxx1.Types.RefQualifier where
  toQuery Glean.Schema.Cxx1.Types.RefQualifier_None_ = Glean.Schema.Query.Cxx1.Types.RefQualifier_None_
  toQuery Glean.Schema.Cxx1.Types.RefQualifier_LValue = Glean.Schema.Query.Cxx1.Types.RefQualifier_LValue
  toQuery Glean.Schema.Cxx1.Types.RefQualifier_RValue = Glean.Schema.Query.Cxx1.Types.RefQualifier_RValue

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.VariableKind = Glean.Schema.Cxx1.Types.VariableKind
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.VariableKind = Glean.Schema.Query.Cxx1.Types.VariableKind

instance Glean.ToQuery Glean.Schema.Cxx1.Types.VariableKind where
  toQuery (Glean.Schema.Cxx1.Types.VariableKind_global_ x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.variableKind_global_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.VariableKind_field x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.variableKind_field = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.VariableKind_ivar x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.variableKind_ivar = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.GlobalVariable Glean.Schema.Query.Cxx1.Types.VariableKind where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.variableKind_global_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.Field Glean.Schema.Query.Cxx1.Types.VariableKind where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.variableKind_field = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.ObjcIVar Glean.Schema.Query.Cxx1.Types.VariableKind where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.variableKind_ivar = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.XRefTarget = Glean.Schema.Cxx1.Types.XRefTarget
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.XRefTarget = Glean.Schema.Query.Cxx1.Types.XRefTarget

instance Glean.ToQuery Glean.Schema.Cxx1.Types.XRefTarget where
  toQuery (Glean.Schema.Cxx1.Types.XRefTarget_declaration x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.xRefTarget_declaration = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.XRefTarget_enumerator x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.xRefTarget_enumerator = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.XRefTarget_objcSelector x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.xRefTarget_objcSelector = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.XRefTarget_unknown x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.xRefTarget_unknown = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Cxx1.Types.XRefTarget_indirect x) = Data.Default.def { Glean.Schema.Query.Cxx1.Types.xRefTarget_indirect = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.Declaration Glean.Schema.Query.Cxx1.Types.XRefTarget where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.xRefTarget_declaration = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.Enumerator Glean.Schema.Query.Cxx1.Types.XRefTarget where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.xRefTarget_enumerator = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.ObjcSelector Glean.Schema.Query.Cxx1.Types.XRefTarget where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.xRefTarget_objcSelector = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Src.Types.Loc Glean.Schema.Query.Cxx1.Types.XRefTarget where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.xRefTarget_unknown = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Cxx1.Types.XRefIndirectTarget Glean.Schema.Query.Cxx1.Types.XRefTarget where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Cxx1.Types.xRefTarget_indirect = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.RecordBase = Glean.Schema.Cxx1.Types.RecordBase
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.RecordBase = Glean.Schema.Query.Cxx1.Types.RecordBase

instance Glean.ToQuery Glean.Schema.Cxx1.Types.RecordBase where
  toQuery (Glean.Schema.Cxx1.Types.RecordBase x1 x2 x3) = Glean.Schema.Query.Cxx1.Types.RecordBase (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

type instance Glean.QueryResult Glean.Schema.Query.Cxx1.Types.Scope_recordWithAccess_ = Glean.Schema.Cxx1.Types.Scope_recordWithAccess_
type instance Glean.QueryOf Glean.Schema.Cxx1.Types.Scope_recordWithAccess_ = Glean.Schema.Query.Cxx1.Types.Scope_recordWithAccess_

instance Glean.ToQuery Glean.Schema.Cxx1.Types.Scope_recordWithAccess_ where
  toQuery (Glean.Schema.Cxx1.Types.Scope_recordWithAccess_ x1 x2) = Glean.Schema.Query.Cxx1.Types.Scope_recordWithAccess_ (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))
