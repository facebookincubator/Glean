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

import qualified Glean.Schema.Code.Types
import qualified Glean.Schema.Query.Code.Types

import qualified Glean.Schema.Flow.Types
import qualified Glean.Schema.Query.Flow.Types

import qualified Glean.Schema.Src.Types
import qualified Glean.Schema.Query.Src.Types

import qualified Glean.Schema.Codemarkup.Types


type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowModuleNamespaceXRef_key = Glean.Schema.Codemarkup.Types.FlowModuleNamespaceXRef_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowModuleNamespaceXRef_key = Glean.Schema.Query.Codemarkup.Types.FlowModuleNamespaceXRef_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowModuleNamespaceXRef_key where
  toQuery (Glean.Schema.Codemarkup.Types.FlowModuleNamespaceXRef_key x1 x2) = Glean.Schema.Query.Codemarkup.Types.FlowModuleNamespaceXRef_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.FlowModuleNamespaceXRef where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.FlowModuleNamespaceXRef_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.FlowModuleNamespaceXRef_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowModuleNamespaceXRef = Glean.Schema.Codemarkup.Types.FlowModuleNamespaceXRef
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowModuleNamespaceXRef = Glean.Schema.Query.Codemarkup.Types.FlowModuleNamespaceXRef

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowModuleNamespaceXRef

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.HackEntityUses_key = Glean.Schema.Codemarkup.Types.HackEntityUses_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.HackEntityUses_key = Glean.Schema.Query.Codemarkup.Types.HackEntityUses_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.HackEntityUses_key where
  toQuery (Glean.Schema.Codemarkup.Types.HackEntityUses_key x1 x2 x3) = Glean.Schema.Query.Codemarkup.Types.HackEntityUses_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.HackEntityUses where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.HackEntityUses_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.HackEntityUses_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.HackEntityUses = Glean.Schema.Codemarkup.Types.HackEntityUses
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.HackEntityUses = Glean.Schema.Query.Codemarkup.Types.HackEntityUses

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.HackEntityUses

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FileDeclarations_key = Glean.Schema.Codemarkup.Types.FileDeclarations_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FileDeclarations_key = Glean.Schema.Query.Codemarkup.Types.FileDeclarations_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FileDeclarations_key where
  toQuery (Glean.Schema.Codemarkup.Types.FileDeclarations_key x1 x2) = Glean.Schema.Query.Codemarkup.Types.FileDeclarations_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.FileDeclarations where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.FileDeclarations_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.FileDeclarations_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FileDeclarations = Glean.Schema.Codemarkup.Types.FileDeclarations
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FileDeclarations = Glean.Schema.Query.Codemarkup.Types.FileDeclarations

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FileDeclarations

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowFileEntityXRefs_key = Glean.Schema.Codemarkup.Types.FlowFileEntityXRefs_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowFileEntityXRefs_key = Glean.Schema.Query.Codemarkup.Types.FlowFileEntityXRefs_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowFileEntityXRefs_key where
  toQuery (Glean.Schema.Codemarkup.Types.FlowFileEntityXRefs_key x1 x2 x3) = Glean.Schema.Query.Codemarkup.Types.FlowFileEntityXRefs_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.FlowFileEntityXRefs where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.FlowFileEntityXRefs_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.FlowFileEntityXRefs_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowFileEntityXRefs = Glean.Schema.Codemarkup.Types.FlowFileEntityXRefs
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowFileEntityXRefs = Glean.Schema.Query.Codemarkup.Types.FlowFileEntityXRefs

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowFileEntityXRefs

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.HaskellFileDirectXRefs_key = Glean.Schema.Codemarkup.Types.HaskellFileDirectXRefs_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.HaskellFileDirectXRefs_key = Glean.Schema.Query.Codemarkup.Types.HaskellFileDirectXRefs_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.HaskellFileDirectXRefs_key where
  toQuery (Glean.Schema.Codemarkup.Types.HaskellFileDirectXRefs_key x1 x2) = Glean.Schema.Query.Codemarkup.Types.HaskellFileDirectXRefs_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.HaskellFileDirectXRefs where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.HaskellFileDirectXRefs_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.HaskellFileDirectXRefs_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.HaskellFileDirectXRefs = Glean.Schema.Codemarkup.Types.HaskellFileDirectXRefs
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.HaskellFileDirectXRefs = Glean.Schema.Query.Codemarkup.Types.HaskellFileDirectXRefs

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.HaskellFileDirectXRefs

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.PythonFileDirectXRefs_key = Glean.Schema.Codemarkup.Types.PythonFileDirectXRefs_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.PythonFileDirectXRefs_key = Glean.Schema.Query.Codemarkup.Types.PythonFileDirectXRefs_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.PythonFileDirectXRefs_key where
  toQuery (Glean.Schema.Codemarkup.Types.PythonFileDirectXRefs_key x1 x2) = Glean.Schema.Query.Codemarkup.Types.PythonFileDirectXRefs_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.PythonFileDirectXRefs where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.PythonFileDirectXRefs_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.PythonFileDirectXRefs_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.PythonFileDirectXRefs = Glean.Schema.Codemarkup.Types.PythonFileDirectXRefs
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.PythonFileDirectXRefs = Glean.Schema.Query.Codemarkup.Types.PythonFileDirectXRefs

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.PythonFileDirectXRefs

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.PythonResolve_key = Glean.Schema.Codemarkup.Types.PythonResolve_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.PythonResolve_key = Glean.Schema.Query.Codemarkup.Types.PythonResolve_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.PythonResolve_key where
  toQuery (Glean.Schema.Codemarkup.Types.PythonResolve_key x1 x2) = Glean.Schema.Query.Codemarkup.Types.PythonResolve_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.PythonResolve where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.PythonResolve_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.PythonResolve_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.PythonResolve = Glean.Schema.Codemarkup.Types.PythonResolve
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.PythonResolve = Glean.Schema.Query.Codemarkup.Types.PythonResolve

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.PythonResolve

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.EntityToDeclaration_key = Glean.Schema.Codemarkup.Types.EntityToDeclaration_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.EntityToDeclaration_key = Glean.Schema.Query.Codemarkup.Types.EntityToDeclaration_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.EntityToDeclaration_key where
  toQuery (Glean.Schema.Codemarkup.Types.EntityToDeclaration_key x1 x2) = Glean.Schema.Query.Codemarkup.Types.EntityToDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.EntityToDeclaration where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.EntityToDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.EntityToDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.EntityToDeclaration = Glean.Schema.Codemarkup.Types.EntityToDeclaration
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.EntityToDeclaration = Glean.Schema.Query.Codemarkup.Types.EntityToDeclaration

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.EntityToDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FileEntities_key = Glean.Schema.Codemarkup.Types.FileEntities_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FileEntities_key = Glean.Schema.Query.Codemarkup.Types.FileEntities_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FileEntities_key where
  toQuery (Glean.Schema.Codemarkup.Types.FileEntities_key x1 x2 x3) = Glean.Schema.Query.Codemarkup.Types.FileEntities_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.FileEntities where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.FileEntities_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.FileEntities_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FileEntities = Glean.Schema.Codemarkup.Types.FileEntities
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FileEntities = Glean.Schema.Query.Codemarkup.Types.FileEntities

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FileEntities

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowXRefDeclInfo_key = Glean.Schema.Codemarkup.Types.FlowXRefDeclInfo_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowXRefDeclInfo_key = Glean.Schema.Query.Codemarkup.Types.FlowXRefDeclInfo_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowXRefDeclInfo_key where
  toQuery (Glean.Schema.Codemarkup.Types.FlowXRefDeclInfo_key x1 x2 x3 x4 x5) = Glean.Schema.Query.Codemarkup.Types.FlowXRefDeclInfo_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just (Glean.toQuery x4)) (Prelude.Just (Glean.toQuery x5))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.FlowXRefDeclInfo where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.FlowXRefDeclInfo_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.FlowXRefDeclInfo_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowXRefDeclInfo = Glean.Schema.Codemarkup.Types.FlowXRefDeclInfo
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowXRefDeclInfo = Glean.Schema.Query.Codemarkup.Types.FlowXRefDeclInfo

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowXRefDeclInfo

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowResolve_key = Glean.Schema.Codemarkup.Types.FlowResolve_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowResolve_key = Glean.Schema.Query.Codemarkup.Types.FlowResolve_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowResolve_key where
  toQuery (Glean.Schema.Codemarkup.Types.FlowResolve_key x1 x2) = Glean.Schema.Query.Codemarkup.Types.FlowResolve_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.FlowResolve where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.FlowResolve_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.FlowResolve_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowResolve = Glean.Schema.Codemarkup.Types.FlowResolve
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowResolve = Glean.Schema.Query.Codemarkup.Types.FlowResolve

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowResolve

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowTypeImportXRef_key = Glean.Schema.Codemarkup.Types.FlowTypeImportXRef_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowTypeImportXRef_key = Glean.Schema.Query.Codemarkup.Types.FlowTypeImportXRef_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowTypeImportXRef_key where
  toQuery (Glean.Schema.Codemarkup.Types.FlowTypeImportXRef_key x1 x2 x3) = Glean.Schema.Query.Codemarkup.Types.FlowTypeImportXRef_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.FlowTypeImportXRef where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.FlowTypeImportXRef_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.FlowTypeImportXRef_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowTypeImportXRef = Glean.Schema.Codemarkup.Types.FlowTypeImportXRef
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowTypeImportXRef = Glean.Schema.Query.Codemarkup.Types.FlowTypeImportXRef

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowTypeImportXRef

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.Resolve_key = Glean.Schema.Codemarkup.Types.Resolve_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.Resolve_key = Glean.Schema.Query.Codemarkup.Types.Resolve_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.Resolve_key where
  toQuery (Glean.Schema.Codemarkup.Types.Resolve_key x1 x2) = Glean.Schema.Query.Codemarkup.Types.Resolve_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.Resolve where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.Resolve_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.Resolve_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.Resolve = Glean.Schema.Codemarkup.Types.Resolve
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.Resolve = Glean.Schema.Query.Codemarkup.Types.Resolve

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.Resolve

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.HackFileDeclarations_key = Glean.Schema.Codemarkup.Types.HackFileDeclarations_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.HackFileDeclarations_key = Glean.Schema.Query.Codemarkup.Types.HackFileDeclarations_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.HackFileDeclarations_key where
  toQuery (Glean.Schema.Codemarkup.Types.HackFileDeclarations_key x1 x2) = Glean.Schema.Query.Codemarkup.Types.HackFileDeclarations_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.HackFileDeclarations where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.HackFileDeclarations_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.HackFileDeclarations_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.HackFileDeclarations = Glean.Schema.Codemarkup.Types.HackFileDeclarations
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.HackFileDeclarations = Glean.Schema.Query.Codemarkup.Types.HackFileDeclarations

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.HackFileDeclarations

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowCompatibleModuleExport_key = Glean.Schema.Codemarkup.Types.FlowCompatibleModuleExport_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowCompatibleModuleExport_key = Glean.Schema.Query.Codemarkup.Types.FlowCompatibleModuleExport_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowCompatibleModuleExport_key where
  toQuery (Glean.Schema.Codemarkup.Types.FlowCompatibleModuleExport_key x1 x2) = Glean.Schema.Query.Codemarkup.Types.FlowCompatibleModuleExport_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.FlowCompatibleModuleExport where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.FlowCompatibleModuleExport_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.FlowCompatibleModuleExport_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowCompatibleModuleExport = Glean.Schema.Codemarkup.Types.FlowCompatibleModuleExport
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowCompatibleModuleExport = Glean.Schema.Query.Codemarkup.Types.FlowCompatibleModuleExport

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowCompatibleModuleExport

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowSameModule_key = Glean.Schema.Codemarkup.Types.FlowSameModule_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowSameModule_key = Glean.Schema.Query.Codemarkup.Types.FlowSameModule_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowSameModule_key where
  toQuery (Glean.Schema.Codemarkup.Types.FlowSameModule_key x1 x2) = Glean.Schema.Query.Codemarkup.Types.FlowSameModule_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.FlowSameModule where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.FlowSameModule_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.FlowSameModule_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowSameModule = Glean.Schema.Codemarkup.Types.FlowSameModule
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowSameModule = Glean.Schema.Query.Codemarkup.Types.FlowSameModule

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowSameModule

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FileAnnotations_key = Glean.Schema.Codemarkup.Types.FileAnnotations_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FileAnnotations_key = Glean.Schema.Query.Codemarkup.Types.FileAnnotations_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FileAnnotations_key where
  toQuery (Glean.Schema.Codemarkup.Types.FileAnnotations_key x1 x2) = Glean.Schema.Query.Codemarkup.Types.FileAnnotations_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.FileAnnotations where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.FileAnnotations_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.FileAnnotations_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FileAnnotations = Glean.Schema.Codemarkup.Types.FileAnnotations
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FileAnnotations = Glean.Schema.Query.Codemarkup.Types.FileAnnotations

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FileAnnotations

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowXRefInfo_key = Glean.Schema.Codemarkup.Types.FlowXRefInfo_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowXRefInfo_key = Glean.Schema.Query.Codemarkup.Types.FlowXRefInfo_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowXRefInfo_key where
  toQuery (Glean.Schema.Codemarkup.Types.FlowXRefInfo_key x1 x2 x3 x4) = Glean.Schema.Query.Codemarkup.Types.FlowXRefInfo_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just (Glean.toQuery x4))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.FlowXRefInfo where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.FlowXRefInfo_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.FlowXRefInfo_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowXRefInfo = Glean.Schema.Codemarkup.Types.FlowXRefInfo
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowXRefInfo = Glean.Schema.Query.Codemarkup.Types.FlowXRefInfo

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowXRefInfo

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.HackResolve_key = Glean.Schema.Codemarkup.Types.HackResolve_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.HackResolve_key = Glean.Schema.Query.Codemarkup.Types.HackResolve_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.HackResolve_key where
  toQuery (Glean.Schema.Codemarkup.Types.HackResolve_key x1 x2) = Glean.Schema.Query.Codemarkup.Types.HackResolve_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.HackResolve where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.HackResolve_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.HackResolve_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.HackResolve = Glean.Schema.Codemarkup.Types.HackResolve
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.HackResolve = Glean.Schema.Query.Codemarkup.Types.HackResolve

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.HackResolve

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.HackFileDirectXRefs_key = Glean.Schema.Codemarkup.Types.HackFileDirectXRefs_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.HackFileDirectXRefs_key = Glean.Schema.Query.Codemarkup.Types.HackFileDirectXRefs_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.HackFileDirectXRefs_key where
  toQuery (Glean.Schema.Codemarkup.Types.HackFileDirectXRefs_key x1 x2) = Glean.Schema.Query.Codemarkup.Types.HackFileDirectXRefs_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.HackFileDirectXRefs where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.HackFileDirectXRefs_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.HackFileDirectXRefs_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.HackFileDirectXRefs = Glean.Schema.Codemarkup.Types.HackFileDirectXRefs
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.HackFileDirectXRefs = Glean.Schema.Query.Codemarkup.Types.HackFileDirectXRefs

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.HackFileDirectXRefs

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.EntityUses_key = Glean.Schema.Codemarkup.Types.EntityUses_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.EntityUses_key = Glean.Schema.Query.Codemarkup.Types.EntityUses_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.EntityUses_key where
  toQuery (Glean.Schema.Codemarkup.Types.EntityUses_key x1 x2 x3) = Glean.Schema.Query.Codemarkup.Types.EntityUses_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.EntityUses where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.EntityUses_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.EntityUses_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.EntityUses = Glean.Schema.Codemarkup.Types.EntityUses
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.EntityUses = Glean.Schema.Query.Codemarkup.Types.EntityUses

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.EntityUses

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.PythonFileEntityXRefs_key = Glean.Schema.Codemarkup.Types.PythonFileEntityXRefs_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.PythonFileEntityXRefs_key = Glean.Schema.Query.Codemarkup.Types.PythonFileEntityXRefs_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.PythonFileEntityXRefs_key where
  toQuery (Glean.Schema.Codemarkup.Types.PythonFileEntityXRefs_key x1 x2 x3) = Glean.Schema.Query.Codemarkup.Types.PythonFileEntityXRefs_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.PythonFileEntityXRefs where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.PythonFileEntityXRefs_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.PythonFileEntityXRefs_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.PythonFileEntityXRefs = Glean.Schema.Codemarkup.Types.PythonFileEntityXRefs
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.PythonFileEntityXRefs = Glean.Schema.Query.Codemarkup.Types.PythonFileEntityXRefs

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.PythonFileEntityXRefs

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowTypeExportLocation_key = Glean.Schema.Codemarkup.Types.FlowTypeExportLocation_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowTypeExportLocation_key = Glean.Schema.Query.Codemarkup.Types.FlowTypeExportLocation_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowTypeExportLocation_key where
  toQuery (Glean.Schema.Codemarkup.Types.FlowTypeExportLocation_key x1 x2 x3) = Glean.Schema.Query.Codemarkup.Types.FlowTypeExportLocation_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.FlowTypeExportLocation where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.FlowTypeExportLocation_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.FlowTypeExportLocation_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowTypeExportLocation = Glean.Schema.Codemarkup.Types.FlowTypeExportLocation
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowTypeExportLocation = Glean.Schema.Query.Codemarkup.Types.FlowTypeExportLocation

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowTypeExportLocation

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowFileDirectXRefs_key = Glean.Schema.Codemarkup.Types.FlowFileDirectXRefs_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowFileDirectXRefs_key = Glean.Schema.Query.Codemarkup.Types.FlowFileDirectXRefs_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowFileDirectXRefs_key where
  toQuery (Glean.Schema.Codemarkup.Types.FlowFileDirectXRefs_key x1 x2) = Glean.Schema.Query.Codemarkup.Types.FlowFileDirectXRefs_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.FlowFileDirectXRefs where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.FlowFileDirectXRefs_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.FlowFileDirectXRefs_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowFileDirectXRefs = Glean.Schema.Codemarkup.Types.FlowFileDirectXRefs
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowFileDirectXRefs = Glean.Schema.Query.Codemarkup.Types.FlowFileDirectXRefs

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowFileDirectXRefs

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.HackFileEntityXRefs_key = Glean.Schema.Codemarkup.Types.HackFileEntityXRefs_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.HackFileEntityXRefs_key = Glean.Schema.Query.Codemarkup.Types.HackFileEntityXRefs_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.HackFileEntityXRefs_key where
  toQuery (Glean.Schema.Codemarkup.Types.HackFileEntityXRefs_key x1 x2 x3) = Glean.Schema.Query.Codemarkup.Types.HackFileEntityXRefs_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.HackFileEntityXRefs where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.HackFileEntityXRefs_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.HackFileEntityXRefs_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.HackFileEntityXRefs = Glean.Schema.Codemarkup.Types.HackFileEntityXRefs
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.HackFileEntityXRefs = Glean.Schema.Query.Codemarkup.Types.HackFileEntityXRefs

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.HackFileEntityXRefs

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowImportXRef_key = Glean.Schema.Codemarkup.Types.FlowImportXRef_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowImportXRef_key = Glean.Schema.Query.Codemarkup.Types.FlowImportXRef_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowImportXRef_key where
  toQuery (Glean.Schema.Codemarkup.Types.FlowImportXRef_key x1 x2 x3) = Glean.Schema.Query.Codemarkup.Types.FlowImportXRef_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.FlowImportXRef where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.FlowImportXRef_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.FlowImportXRef_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowImportXRef = Glean.Schema.Codemarkup.Types.FlowImportXRef
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowImportXRef = Glean.Schema.Query.Codemarkup.Types.FlowImportXRef

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowImportXRef

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowModuleExportLocation_key = Glean.Schema.Codemarkup.Types.FlowModuleExportLocation_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowModuleExportLocation_key = Glean.Schema.Query.Codemarkup.Types.FlowModuleExportLocation_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowModuleExportLocation_key where
  toQuery (Glean.Schema.Codemarkup.Types.FlowModuleExportLocation_key x1 x2 x3) = Glean.Schema.Query.Codemarkup.Types.FlowModuleExportLocation_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.FlowModuleExportLocation where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.FlowModuleExportLocation_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.FlowModuleExportLocation_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowModuleExportLocation = Glean.Schema.Codemarkup.Types.FlowModuleExportLocation
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowModuleExportLocation = Glean.Schema.Query.Codemarkup.Types.FlowModuleExportLocation

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowModuleExportLocation

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.PythonFileDeclarations_key = Glean.Schema.Codemarkup.Types.PythonFileDeclarations_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.PythonFileDeclarations_key = Glean.Schema.Query.Codemarkup.Types.PythonFileDeclarations_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.PythonFileDeclarations_key where
  toQuery (Glean.Schema.Codemarkup.Types.PythonFileDeclarations_key x1 x2) = Glean.Schema.Query.Codemarkup.Types.PythonFileDeclarations_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.PythonFileDeclarations where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.PythonFileDeclarations_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.PythonFileDeclarations_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.PythonFileDeclarations = Glean.Schema.Codemarkup.Types.PythonFileDeclarations
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.PythonFileDeclarations = Glean.Schema.Query.Codemarkup.Types.PythonFileDeclarations

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.PythonFileDeclarations

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FileDirectXRefs_key = Glean.Schema.Codemarkup.Types.FileDirectXRefs_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FileDirectXRefs_key = Glean.Schema.Query.Codemarkup.Types.FileDirectXRefs_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FileDirectXRefs_key where
  toQuery (Glean.Schema.Codemarkup.Types.FileDirectXRefs_key x1 x2) = Glean.Schema.Query.Codemarkup.Types.FileDirectXRefs_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.FileDirectXRefs where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.FileDirectXRefs_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.FileDirectXRefs_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FileDirectXRefs = Glean.Schema.Codemarkup.Types.FileDirectXRefs
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FileDirectXRefs = Glean.Schema.Query.Codemarkup.Types.FileDirectXRefs

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FileDirectXRefs

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowDeclarationInfo_key = Glean.Schema.Codemarkup.Types.FlowDeclarationInfo_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowDeclarationInfo_key = Glean.Schema.Query.Codemarkup.Types.FlowDeclarationInfo_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowDeclarationInfo_key where
  toQuery (Glean.Schema.Codemarkup.Types.FlowDeclarationInfo_key x1 x2 x3) = Glean.Schema.Query.Codemarkup.Types.FlowDeclarationInfo_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.FlowDeclarationInfo where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.FlowDeclarationInfo_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.FlowDeclarationInfo_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowDeclarationInfo = Glean.Schema.Codemarkup.Types.FlowDeclarationInfo
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowDeclarationInfo = Glean.Schema.Query.Codemarkup.Types.FlowDeclarationInfo

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowDeclarationInfo

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FileEntityXRefs_key = Glean.Schema.Codemarkup.Types.FileEntityXRefs_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FileEntityXRefs_key = Glean.Schema.Query.Codemarkup.Types.FileEntityXRefs_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FileEntityXRefs_key where
  toQuery (Glean.Schema.Codemarkup.Types.FileEntityXRefs_key x1 x2 x3) = Glean.Schema.Query.Codemarkup.Types.FileEntityXRefs_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.FileEntityXRefs where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.FileEntityXRefs_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.FileEntityXRefs_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FileEntityXRefs = Glean.Schema.Codemarkup.Types.FileEntityXRefs
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FileEntityXRefs = Glean.Schema.Query.Codemarkup.Types.FileEntityXRefs

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FileEntityXRefs

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowFileDeclarations_key = Glean.Schema.Codemarkup.Types.FlowFileDeclarations_key
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowFileDeclarations_key = Glean.Schema.Query.Codemarkup.Types.FlowFileDeclarations_key

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowFileDeclarations_key where
  toQuery (Glean.Schema.Codemarkup.Types.FlowFileDeclarations_key x1 x2) = Glean.Schema.Query.Codemarkup.Types.FlowFileDeclarations_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Codemarkup.Types.FlowFileDeclarations where
  toQueryId = Glean.Schema.Query.Codemarkup.Types.FlowFileDeclarations_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Codemarkup.Types.FlowFileDeclarations_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.FlowFileDeclarations = Glean.Schema.Codemarkup.Types.FlowFileDeclarations
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.FlowFileDeclarations = Glean.Schema.Query.Codemarkup.Types.FlowFileDeclarations

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.FlowFileDeclarations

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.LinkTo = Glean.Schema.Codemarkup.Types.LinkTo
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.LinkTo = Glean.Schema.Query.Codemarkup.Types.LinkTo

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.LinkTo where
  toQuery (Glean.Schema.Codemarkup.Types.LinkTo_localRepo x) = Data.Default.def { Glean.Schema.Query.Codemarkup.Types.linkTo_localRepo = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Src.Types.FileLocation Glean.Schema.Query.Codemarkup.Types.LinkTo where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Codemarkup.Types.linkTo_localRepo = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.Annotation = Glean.Schema.Codemarkup.Types.Annotation
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.Annotation = Glean.Schema.Query.Codemarkup.Types.Annotation

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.Annotation where
  toQuery (Glean.Schema.Codemarkup.Types.Annotation x1 x2 x3) = Glean.Schema.Query.Codemarkup.Types.Annotation (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Codemarkup.Types.annotation_linkTo_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Codemarkup.Types.annotation_linkTo_just = Prelude.Just (Glean.toQuery x)})) x3))

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.Declaration = Glean.Schema.Codemarkup.Types.Declaration
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.Declaration = Glean.Schema.Query.Codemarkup.Types.Declaration

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.Declaration where
  toQuery (Glean.Schema.Codemarkup.Types.Declaration x1 x2 x3) = Glean.Schema.Query.Codemarkup.Types.Declaration (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

type instance Glean.QueryResult Glean.Schema.Query.Codemarkup.Types.DirectXRef = Glean.Schema.Codemarkup.Types.DirectXRef
type instance Glean.QueryOf Glean.Schema.Codemarkup.Types.DirectXRef = Glean.Schema.Query.Codemarkup.Types.DirectXRef

instance Glean.ToQuery Glean.Schema.Codemarkup.Types.DirectXRef where
  toQuery (Glean.Schema.Codemarkup.Types.DirectXRef x1 x2) = Glean.Schema.Query.Codemarkup.Types.DirectXRef (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))
