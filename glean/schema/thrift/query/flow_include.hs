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

import qualified Glean.Schema.Src.Types
import qualified Glean.Schema.Query.Src.Types

import qualified Glean.Schema.Flow.Types


type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.ImportDeclaration_key = Glean.Schema.Flow.Types.ImportDeclaration_key
type instance Glean.QueryOf Glean.Schema.Flow.Types.ImportDeclaration_key = Glean.Schema.Query.Flow.Types.ImportDeclaration_key

instance Glean.ToQuery Glean.Schema.Flow.Types.ImportDeclaration_key where
  toQuery (Glean.Schema.Flow.Types.ImportDeclaration_key x1 x2) = Glean.Schema.Query.Flow.Types.ImportDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Flow.Types.ImportDeclaration where
  toQueryId = Glean.Schema.Query.Flow.Types.ImportDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.ImportDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.ImportDeclaration = Glean.Schema.Flow.Types.ImportDeclaration
type instance Glean.QueryOf Glean.Schema.Flow.Types.ImportDeclaration = Glean.Schema.Query.Flow.Types.ImportDeclaration

instance Glean.ToQuery Glean.Schema.Flow.Types.ImportDeclaration

instance Glean.PredicateQuery Glean.Schema.Flow.Types.Documentation where
  toQueryId = Glean.Schema.Query.Flow.Types.Documentation_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.Documentation_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.Documentation = Glean.Schema.Flow.Types.Documentation
type instance Glean.QueryOf Glean.Schema.Flow.Types.Documentation = Glean.Schema.Query.Flow.Types.Documentation

instance Glean.ToQuery Glean.Schema.Flow.Types.Documentation

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.Module_key = Glean.Schema.Flow.Types.Module_key
type instance Glean.QueryOf Glean.Schema.Flow.Types.Module_key = Glean.Schema.Query.Flow.Types.Module_key

instance Glean.ToQuery Glean.Schema.Flow.Types.Module_key where
  toQuery (Glean.Schema.Flow.Types.Module_key_file x) = Data.Default.def { Glean.Schema.Query.Flow.Types.module_key_file = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Flow.Types.Module_key_builtin x) = Data.Default.def { Glean.Schema.Query.Flow.Types.module_key_builtin = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Flow.Types.Module_key_lib x) = Data.Default.def { Glean.Schema.Query.Flow.Types.module_key_lib = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Flow.Types.Module_key_noSource x) = Data.Default.def { Glean.Schema.Query.Flow.Types.module_key_noSource = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Flow.Types.Module_key_string_ x) = Data.Default.def { Glean.Schema.Query.Flow.Types.module_key_string_ = Prelude.Just (Glean.toQuery x) }

instance Glean.PredicateQuery Glean.Schema.Flow.Types.Module where
  toQueryId = Glean.Schema.Query.Flow.Types.Module_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.Module_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.Module = Glean.Schema.Flow.Types.Module
type instance Glean.QueryOf Glean.Schema.Flow.Types.Module = Glean.Schema.Query.Flow.Types.Module

instance Glean.ToQuery Glean.Schema.Flow.Types.Module

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.StringToFileModule_key = Glean.Schema.Flow.Types.StringToFileModule_key
type instance Glean.QueryOf Glean.Schema.Flow.Types.StringToFileModule_key = Glean.Schema.Query.Flow.Types.StringToFileModule_key

instance Glean.ToQuery Glean.Schema.Flow.Types.StringToFileModule_key where
  toQuery (Glean.Schema.Flow.Types.StringToFileModule_key x1 x2) = Glean.Schema.Query.Flow.Types.StringToFileModule_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Flow.Types.StringToFileModule where
  toQueryId = Glean.Schema.Query.Flow.Types.StringToFileModule_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.StringToFileModule_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.StringToFileModule = Glean.Schema.Flow.Types.StringToFileModule
type instance Glean.QueryOf Glean.Schema.Flow.Types.StringToFileModule = Glean.Schema.Query.Flow.Types.StringToFileModule

instance Glean.ToQuery Glean.Schema.Flow.Types.StringToFileModule

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.FileXRef_key = Glean.Schema.Flow.Types.FileXRef_key
type instance Glean.QueryOf Glean.Schema.Flow.Types.FileXRef_key = Glean.Schema.Query.Flow.Types.FileXRef_key

instance Glean.ToQuery Glean.Schema.Flow.Types.FileXRef_key where
  toQuery (Glean.Schema.Flow.Types.FileXRef_key x1 x2) = Glean.Schema.Query.Flow.Types.FileXRef_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Flow.Types.FileXRef where
  toQueryId = Glean.Schema.Query.Flow.Types.FileXRef_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.FileXRef_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.FileXRef = Glean.Schema.Flow.Types.FileXRef
type instance Glean.QueryOf Glean.Schema.Flow.Types.FileXRef = Glean.Schema.Query.Flow.Types.FileXRef

instance Glean.ToQuery Glean.Schema.Flow.Types.FileXRef

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.Export_key = Glean.Schema.Flow.Types.Export_key
type instance Glean.QueryOf Glean.Schema.Flow.Types.Export_key = Glean.Schema.Query.Flow.Types.Export_key

instance Glean.ToQuery Glean.Schema.Flow.Types.Export_key where
  toQuery (Glean.Schema.Flow.Types.Export_key_commonJS x) = Data.Default.def { Glean.Schema.Query.Flow.Types.export_key_commonJS = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Flow.Types.Export_key_commonJSMember x) = Data.Default.def { Glean.Schema.Query.Flow.Types.export_key_commonJSMember = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Flow.Types.Export_key_named x) = Data.Default.def { Glean.Schema.Query.Flow.Types.export_key_named = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Flow.Types.Export_key_default_ x) = Data.Default.def { Glean.Schema.Query.Flow.Types.export_key_default_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Flow.Types.Export_key_star x) = Data.Default.def { Glean.Schema.Query.Flow.Types.export_key_star = Prelude.Just (Glean.toQuery x) }

instance Glean.PredicateQuery Glean.Schema.Flow.Types.Export where
  toQueryId = Glean.Schema.Query.Flow.Types.Export_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.Export_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.Export = Glean.Schema.Flow.Types.Export
type instance Glean.QueryOf Glean.Schema.Flow.Types.Export = Glean.Schema.Query.Flow.Types.Export

instance Glean.ToQuery Glean.Schema.Flow.Types.Export

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.FileDeclaration_key = Glean.Schema.Flow.Types.FileDeclaration_key
type instance Glean.QueryOf Glean.Schema.Flow.Types.FileDeclaration_key = Glean.Schema.Query.Flow.Types.FileDeclaration_key

instance Glean.ToQuery Glean.Schema.Flow.Types.FileDeclaration_key where
  toQuery (Glean.Schema.Flow.Types.FileDeclaration_key x1 x2) = Glean.Schema.Query.Flow.Types.FileDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Flow.Types.FileDeclaration where
  toQueryId = Glean.Schema.Query.Flow.Types.FileDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.FileDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.FileDeclaration = Glean.Schema.Flow.Types.FileDeclaration
type instance Glean.QueryOf Glean.Schema.Flow.Types.FileDeclaration = Glean.Schema.Query.Flow.Types.FileDeclaration

instance Glean.ToQuery Glean.Schema.Flow.Types.FileDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.ModuleTypeExport_key = Glean.Schema.Flow.Types.ModuleTypeExport_key
type instance Glean.QueryOf Glean.Schema.Flow.Types.ModuleTypeExport_key = Glean.Schema.Query.Flow.Types.ModuleTypeExport_key

instance Glean.ToQuery Glean.Schema.Flow.Types.ModuleTypeExport_key where
  toQuery (Glean.Schema.Flow.Types.ModuleTypeExport_key x1 x2) = Glean.Schema.Query.Flow.Types.ModuleTypeExport_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Flow.Types.ModuleTypeExport where
  toQueryId = Glean.Schema.Query.Flow.Types.ModuleTypeExport_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.ModuleTypeExport_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.ModuleTypeExport = Glean.Schema.Flow.Types.ModuleTypeExport
type instance Glean.QueryOf Glean.Schema.Flow.Types.ModuleTypeExport = Glean.Schema.Query.Flow.Types.ModuleTypeExport

instance Glean.ToQuery Glean.Schema.Flow.Types.ModuleTypeExport

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.LocalDeclarationReference_key = Glean.Schema.Flow.Types.LocalDeclarationReference_key
type instance Glean.QueryOf Glean.Schema.Flow.Types.LocalDeclarationReference_key = Glean.Schema.Query.Flow.Types.LocalDeclarationReference_key

instance Glean.ToQuery Glean.Schema.Flow.Types.LocalDeclarationReference_key where
  toQuery (Glean.Schema.Flow.Types.LocalDeclarationReference_key x1 x2) = Glean.Schema.Query.Flow.Types.LocalDeclarationReference_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Flow.Types.LocalDeclarationReference where
  toQueryId = Glean.Schema.Query.Flow.Types.LocalDeclarationReference_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.LocalDeclarationReference_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.LocalDeclarationReference = Glean.Schema.Flow.Types.LocalDeclarationReference
type instance Glean.QueryOf Glean.Schema.Flow.Types.LocalDeclarationReference = Glean.Schema.Query.Flow.Types.LocalDeclarationReference

instance Glean.ToQuery Glean.Schema.Flow.Types.LocalDeclarationReference

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.DeclarationInfo_key = Glean.Schema.Flow.Types.DeclarationInfo_key
type instance Glean.QueryOf Glean.Schema.Flow.Types.DeclarationInfo_key = Glean.Schema.Query.Flow.Types.DeclarationInfo_key

instance Glean.ToQuery Glean.Schema.Flow.Types.DeclarationInfo_key where
  toQuery (Glean.Schema.Flow.Types.DeclarationInfo_key x1 x2 x3) = Glean.Schema.Query.Flow.Types.DeclarationInfo_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Flow.Types.declarationInfo_documentation_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Flow.Types.declarationInfo_documentation_just = Prelude.Just (Glean.toQuery x)})) x3))

instance Glean.PredicateQuery Glean.Schema.Flow.Types.DeclarationInfo where
  toQueryId = Glean.Schema.Query.Flow.Types.DeclarationInfo_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.DeclarationInfo_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.DeclarationInfo = Glean.Schema.Flow.Types.DeclarationInfo
type instance Glean.QueryOf Glean.Schema.Flow.Types.DeclarationInfo = Glean.Schema.Query.Flow.Types.DeclarationInfo

instance Glean.ToQuery Glean.Schema.Flow.Types.DeclarationInfo

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.SourceOfTypeExport_key = Glean.Schema.Flow.Types.SourceOfTypeExport_key
type instance Glean.QueryOf Glean.Schema.Flow.Types.SourceOfTypeExport_key = Glean.Schema.Query.Flow.Types.SourceOfTypeExport_key

instance Glean.ToQuery Glean.Schema.Flow.Types.SourceOfTypeExport_key where
  toQuery (Glean.Schema.Flow.Types.SourceOfTypeExport_key x1 x2) = Glean.Schema.Query.Flow.Types.SourceOfTypeExport_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Flow.Types.SourceOfTypeExport where
  toQueryId = Glean.Schema.Query.Flow.Types.SourceOfTypeExport_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.SourceOfTypeExport_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.SourceOfTypeExport = Glean.Schema.Flow.Types.SourceOfTypeExport
type instance Glean.QueryOf Glean.Schema.Flow.Types.SourceOfTypeExport = Glean.Schema.Query.Flow.Types.SourceOfTypeExport

instance Glean.ToQuery Glean.Schema.Flow.Types.SourceOfTypeExport

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.Declaration_key = Glean.Schema.Flow.Types.Declaration_key
type instance Glean.QueryOf Glean.Schema.Flow.Types.Declaration_key = Glean.Schema.Query.Flow.Types.Declaration_key

instance Glean.ToQuery Glean.Schema.Flow.Types.Declaration_key where
  toQuery (Glean.Schema.Flow.Types.Declaration_key x1 x2) = Glean.Schema.Query.Flow.Types.Declaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Flow.Types.Declaration where
  toQueryId = Glean.Schema.Query.Flow.Types.Declaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.Declaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.Declaration = Glean.Schema.Flow.Types.Declaration
type instance Glean.QueryOf Glean.Schema.Flow.Types.Declaration = Glean.Schema.Query.Flow.Types.Declaration

instance Glean.ToQuery Glean.Schema.Flow.Types.Declaration

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.TypeImportDeclaration_key = Glean.Schema.Flow.Types.TypeImportDeclaration_key
type instance Glean.QueryOf Glean.Schema.Flow.Types.TypeImportDeclaration_key = Glean.Schema.Query.Flow.Types.TypeImportDeclaration_key

instance Glean.ToQuery Glean.Schema.Flow.Types.TypeImportDeclaration_key where
  toQuery (Glean.Schema.Flow.Types.TypeImportDeclaration_key x1 x2) = Glean.Schema.Query.Flow.Types.TypeImportDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Flow.Types.TypeImportDeclaration where
  toQueryId = Glean.Schema.Query.Flow.Types.TypeImportDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.TypeImportDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.TypeImportDeclaration = Glean.Schema.Flow.Types.TypeImportDeclaration
type instance Glean.QueryOf Glean.Schema.Flow.Types.TypeImportDeclaration = Glean.Schema.Query.Flow.Types.TypeImportDeclaration

instance Glean.ToQuery Glean.Schema.Flow.Types.TypeImportDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.Range_key = Glean.Schema.Flow.Types.Range_key
type instance Glean.QueryOf Glean.Schema.Flow.Types.Range_key = Glean.Schema.Query.Flow.Types.Range_key

instance Glean.ToQuery Glean.Schema.Flow.Types.Range_key where
  toQuery (Glean.Schema.Flow.Types.Range_key x1 x2) = Glean.Schema.Query.Flow.Types.Range_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Flow.Types.Range where
  toQueryId = Glean.Schema.Query.Flow.Types.Range_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.Range_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.Range = Glean.Schema.Flow.Types.Range
type instance Glean.QueryOf Glean.Schema.Flow.Types.Range = Glean.Schema.Query.Flow.Types.Range

instance Glean.ToQuery Glean.Schema.Flow.Types.Range

instance Glean.PredicateQuery Glean.Schema.Flow.Types.Name where
  toQueryId = Glean.Schema.Query.Flow.Types.Name_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.Name_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.Name = Glean.Schema.Flow.Types.Name
type instance Glean.QueryOf Glean.Schema.Flow.Types.Name = Glean.Schema.Query.Flow.Types.Name

instance Glean.ToQuery Glean.Schema.Flow.Types.Name

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.TypeExport_key = Glean.Schema.Flow.Types.TypeExport_key
type instance Glean.QueryOf Glean.Schema.Flow.Types.TypeExport_key = Glean.Schema.Query.Flow.Types.TypeExport_key

instance Glean.ToQuery Glean.Schema.Flow.Types.TypeExport_key where
  toQuery (Glean.Schema.Flow.Types.TypeExport_key_named x) = Data.Default.def { Glean.Schema.Query.Flow.Types.typeExport_key_named = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Flow.Types.TypeExport_key_star x) = Data.Default.def { Glean.Schema.Query.Flow.Types.typeExport_key_star = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Flow.Types.Name Glean.Schema.Query.Flow.Types.TypeExport_key where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Flow.Types.typeExport_key_named = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Flow.Types.Module Glean.Schema.Query.Flow.Types.TypeExport_key where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Flow.Types.typeExport_key_star = Prelude.Just q }

instance Glean.PredicateQuery Glean.Schema.Flow.Types.TypeExport where
  toQueryId = Glean.Schema.Query.Flow.Types.TypeExport_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.TypeExport_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.TypeExport = Glean.Schema.Flow.Types.TypeExport
type instance Glean.QueryOf Glean.Schema.Flow.Types.TypeExport = Glean.Schema.Query.Flow.Types.TypeExport

instance Glean.ToQuery Glean.Schema.Flow.Types.TypeExport

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.MemberDeclarationInfo_key = Glean.Schema.Flow.Types.MemberDeclarationInfo_key
type instance Glean.QueryOf Glean.Schema.Flow.Types.MemberDeclarationInfo_key = Glean.Schema.Query.Flow.Types.MemberDeclarationInfo_key

instance Glean.ToQuery Glean.Schema.Flow.Types.MemberDeclarationInfo_key where
  toQuery (Glean.Schema.Flow.Types.MemberDeclarationInfo_key x1 x2 x3) = Glean.Schema.Query.Flow.Types.MemberDeclarationInfo_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Flow.Types.memberDeclarationInfo_documentation_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Flow.Types.memberDeclarationInfo_documentation_just = Prelude.Just (Glean.toQuery x)})) x3))

instance Glean.PredicateQuery Glean.Schema.Flow.Types.MemberDeclarationInfo where
  toQueryId = Glean.Schema.Query.Flow.Types.MemberDeclarationInfo_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.MemberDeclarationInfo_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.MemberDeclarationInfo = Glean.Schema.Flow.Types.MemberDeclarationInfo
type instance Glean.QueryOf Glean.Schema.Flow.Types.MemberDeclarationInfo = Glean.Schema.Query.Flow.Types.MemberDeclarationInfo

instance Glean.ToQuery Glean.Schema.Flow.Types.MemberDeclarationInfo

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.TypeDeclarationInfo_key = Glean.Schema.Flow.Types.TypeDeclarationInfo_key
type instance Glean.QueryOf Glean.Schema.Flow.Types.TypeDeclarationInfo_key = Glean.Schema.Query.Flow.Types.TypeDeclarationInfo_key

instance Glean.ToQuery Glean.Schema.Flow.Types.TypeDeclarationInfo_key where
  toQuery (Glean.Schema.Flow.Types.TypeDeclarationInfo_key x1 x2 x3) = Glean.Schema.Query.Flow.Types.TypeDeclarationInfo_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Flow.Types.typeDeclarationInfo_documentation_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Flow.Types.typeDeclarationInfo_documentation_just = Prelude.Just (Glean.toQuery x)})) x3))

instance Glean.PredicateQuery Glean.Schema.Flow.Types.TypeDeclarationInfo where
  toQueryId = Glean.Schema.Query.Flow.Types.TypeDeclarationInfo_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.TypeDeclarationInfo_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.TypeDeclarationInfo = Glean.Schema.Flow.Types.TypeDeclarationInfo
type instance Glean.QueryOf Glean.Schema.Flow.Types.TypeDeclarationInfo = Glean.Schema.Query.Flow.Types.TypeDeclarationInfo

instance Glean.ToQuery Glean.Schema.Flow.Types.TypeDeclarationInfo

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.ModuleExport_key = Glean.Schema.Flow.Types.ModuleExport_key
type instance Glean.QueryOf Glean.Schema.Flow.Types.ModuleExport_key = Glean.Schema.Query.Flow.Types.ModuleExport_key

instance Glean.ToQuery Glean.Schema.Flow.Types.ModuleExport_key where
  toQuery (Glean.Schema.Flow.Types.ModuleExport_key x1 x2) = Glean.Schema.Query.Flow.Types.ModuleExport_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Flow.Types.ModuleExport where
  toQueryId = Glean.Schema.Query.Flow.Types.ModuleExport_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.ModuleExport_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.ModuleExport = Glean.Schema.Flow.Types.ModuleExport
type instance Glean.QueryOf Glean.Schema.Flow.Types.ModuleExport = Glean.Schema.Query.Flow.Types.ModuleExport

instance Glean.ToQuery Glean.Schema.Flow.Types.ModuleExport

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.TypeDeclarationReference_key = Glean.Schema.Flow.Types.TypeDeclarationReference_key
type instance Glean.QueryOf Glean.Schema.Flow.Types.TypeDeclarationReference_key = Glean.Schema.Query.Flow.Types.TypeDeclarationReference_key

instance Glean.ToQuery Glean.Schema.Flow.Types.TypeDeclarationReference_key where
  toQuery (Glean.Schema.Flow.Types.TypeDeclarationReference_key x1 x2) = Glean.Schema.Query.Flow.Types.TypeDeclarationReference_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Flow.Types.TypeDeclarationReference where
  toQueryId = Glean.Schema.Query.Flow.Types.TypeDeclarationReference_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.TypeDeclarationReference_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.TypeDeclarationReference = Glean.Schema.Flow.Types.TypeDeclarationReference
type instance Glean.QueryOf Glean.Schema.Flow.Types.TypeDeclarationReference = Glean.Schema.Query.Flow.Types.TypeDeclarationReference

instance Glean.ToQuery Glean.Schema.Flow.Types.TypeDeclarationReference

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.MemberDeclarationReference_key = Glean.Schema.Flow.Types.MemberDeclarationReference_key
type instance Glean.QueryOf Glean.Schema.Flow.Types.MemberDeclarationReference_key = Glean.Schema.Query.Flow.Types.MemberDeclarationReference_key

instance Glean.ToQuery Glean.Schema.Flow.Types.MemberDeclarationReference_key where
  toQuery (Glean.Schema.Flow.Types.MemberDeclarationReference_key x1 x2) = Glean.Schema.Query.Flow.Types.MemberDeclarationReference_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Flow.Types.MemberDeclarationReference where
  toQueryId = Glean.Schema.Query.Flow.Types.MemberDeclarationReference_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.MemberDeclarationReference_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.MemberDeclarationReference = Glean.Schema.Flow.Types.MemberDeclarationReference
type instance Glean.QueryOf Glean.Schema.Flow.Types.MemberDeclarationReference = Glean.Schema.Query.Flow.Types.MemberDeclarationReference

instance Glean.ToQuery Glean.Schema.Flow.Types.MemberDeclarationReference

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.TypeDeclaration_key = Glean.Schema.Flow.Types.TypeDeclaration_key
type instance Glean.QueryOf Glean.Schema.Flow.Types.TypeDeclaration_key = Glean.Schema.Query.Flow.Types.TypeDeclaration_key

instance Glean.ToQuery Glean.Schema.Flow.Types.TypeDeclaration_key where
  toQuery (Glean.Schema.Flow.Types.TypeDeclaration_key x1 x2) = Glean.Schema.Query.Flow.Types.TypeDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Flow.Types.TypeDeclaration where
  toQueryId = Glean.Schema.Query.Flow.Types.TypeDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.TypeDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.TypeDeclaration = Glean.Schema.Flow.Types.TypeDeclaration
type instance Glean.QueryOf Glean.Schema.Flow.Types.TypeDeclaration = Glean.Schema.Query.Flow.Types.TypeDeclaration

instance Glean.ToQuery Glean.Schema.Flow.Types.TypeDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.MemberDeclaration_key = Glean.Schema.Flow.Types.MemberDeclaration_key
type instance Glean.QueryOf Glean.Schema.Flow.Types.MemberDeclaration_key = Glean.Schema.Query.Flow.Types.MemberDeclaration_key

instance Glean.ToQuery Glean.Schema.Flow.Types.MemberDeclaration_key where
  toQuery (Glean.Schema.Flow.Types.MemberDeclaration_key x1 x2) = Glean.Schema.Query.Flow.Types.MemberDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Flow.Types.MemberDeclaration where
  toQueryId = Glean.Schema.Query.Flow.Types.MemberDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.MemberDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.MemberDeclaration = Glean.Schema.Flow.Types.MemberDeclaration
type instance Glean.QueryOf Glean.Schema.Flow.Types.MemberDeclaration = Glean.Schema.Query.Flow.Types.MemberDeclaration

instance Glean.ToQuery Glean.Schema.Flow.Types.MemberDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.SourceOfExport_key = Glean.Schema.Flow.Types.SourceOfExport_key
type instance Glean.QueryOf Glean.Schema.Flow.Types.SourceOfExport_key = Glean.Schema.Query.Flow.Types.SourceOfExport_key

instance Glean.ToQuery Glean.Schema.Flow.Types.SourceOfExport_key where
  toQuery (Glean.Schema.Flow.Types.SourceOfExport_key x1 x2) = Glean.Schema.Query.Flow.Types.SourceOfExport_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Flow.Types.SourceOfExport where
  toQueryId = Glean.Schema.Query.Flow.Types.SourceOfExport_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.SourceOfExport_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.SourceOfExport = Glean.Schema.Flow.Types.SourceOfExport
type instance Glean.QueryOf Glean.Schema.Flow.Types.SourceOfExport = Glean.Schema.Query.Flow.Types.SourceOfExport

instance Glean.ToQuery Glean.Schema.Flow.Types.SourceOfExport

instance Glean.PredicateQuery Glean.Schema.Flow.Types.Type where
  toQueryId = Glean.Schema.Query.Flow.Types.Type__with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.Type__with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.Type_ = Glean.Schema.Flow.Types.Type
type instance Glean.QueryOf Glean.Schema.Flow.Types.Type = Glean.Schema.Query.Flow.Types.Type_

instance Glean.ToQuery Glean.Schema.Flow.Types.Type

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.FileOfStringModule_key = Glean.Schema.Flow.Types.FileOfStringModule_key
type instance Glean.QueryOf Glean.Schema.Flow.Types.FileOfStringModule_key = Glean.Schema.Query.Flow.Types.FileOfStringModule_key

instance Glean.ToQuery Glean.Schema.Flow.Types.FileOfStringModule_key where
  toQuery (Glean.Schema.Flow.Types.FileOfStringModule_key x1 x2) = Glean.Schema.Query.Flow.Types.FileOfStringModule_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Flow.Types.FileOfStringModule where
  toQueryId = Glean.Schema.Query.Flow.Types.FileOfStringModule_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Flow.Types.FileOfStringModule_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.FileOfStringModule = Glean.Schema.Flow.Types.FileOfStringModule
type instance Glean.QueryOf Glean.Schema.Flow.Types.FileOfStringModule = Glean.Schema.Query.Flow.Types.FileOfStringModule

instance Glean.ToQuery Glean.Schema.Flow.Types.FileOfStringModule

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.XRef = Glean.Schema.Flow.Types.XRef
type instance Glean.QueryOf Glean.Schema.Flow.Types.XRef = Glean.Schema.Query.Flow.Types.XRef

instance Glean.ToQuery Glean.Schema.Flow.Types.XRef where
  toQuery (Glean.Schema.Flow.Types.XRef_localRef x) = Data.Default.def { Glean.Schema.Query.Flow.Types.xRef_localRef = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Flow.Types.XRef_memberRef x) = Data.Default.def { Glean.Schema.Query.Flow.Types.xRef_memberRef = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Flow.Types.XRef_typeRef x) = Data.Default.def { Glean.Schema.Query.Flow.Types.xRef_typeRef = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Flow.Types.LocalDeclarationReference Glean.Schema.Query.Flow.Types.XRef where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Flow.Types.xRef_localRef = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Flow.Types.MemberDeclarationReference Glean.Schema.Query.Flow.Types.XRef where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Flow.Types.xRef_memberRef = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Flow.Types.TypeDeclarationReference Glean.Schema.Query.Flow.Types.XRef where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Flow.Types.xRef_typeRef = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.SomeDeclaration = Glean.Schema.Flow.Types.SomeDeclaration
type instance Glean.QueryOf Glean.Schema.Flow.Types.SomeDeclaration = Glean.Schema.Query.Flow.Types.SomeDeclaration

instance Glean.ToQuery Glean.Schema.Flow.Types.SomeDeclaration where
  toQuery (Glean.Schema.Flow.Types.SomeDeclaration_localDecl x) = Data.Default.def { Glean.Schema.Query.Flow.Types.someDeclaration_localDecl = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Flow.Types.SomeDeclaration_memberDecl x) = Data.Default.def { Glean.Schema.Query.Flow.Types.someDeclaration_memberDecl = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Flow.Types.SomeDeclaration_typeDecl x) = Data.Default.def { Glean.Schema.Query.Flow.Types.someDeclaration_typeDecl = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Flow.Types.Declaration Glean.Schema.Query.Flow.Types.SomeDeclaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Flow.Types.someDeclaration_localDecl = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Flow.Types.MemberDeclaration Glean.Schema.Query.Flow.Types.SomeDeclaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Flow.Types.someDeclaration_memberDecl = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Flow.Types.TypeDeclaration Glean.Schema.Query.Flow.Types.SomeDeclaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Flow.Types.someDeclaration_typeDecl = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.ImportDeclaration_import_ = Glean.Schema.Flow.Types.ImportDeclaration_import_
type instance Glean.QueryOf Glean.Schema.Flow.Types.ImportDeclaration_import_ = Glean.Schema.Query.Flow.Types.ImportDeclaration_import_

instance Glean.ToQuery Glean.Schema.Flow.Types.ImportDeclaration_import_ where
  toQuery (Glean.Schema.Flow.Types.ImportDeclaration_import__moduleExport x) = Data.Default.def { Glean.Schema.Query.Flow.Types.importDeclaration_import__moduleExport = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Flow.Types.ImportDeclaration_import__moduleNamespace x) = Data.Default.def { Glean.Schema.Query.Flow.Types.importDeclaration_import__moduleNamespace = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Flow.Types.ModuleExport Glean.Schema.Query.Flow.Types.ImportDeclaration_import_ where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Flow.Types.importDeclaration_import__moduleExport = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Flow.Types.Module Glean.Schema.Query.Flow.Types.ImportDeclaration_import_ where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Flow.Types.importDeclaration_import__moduleNamespace = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.SourceOfTypeExport_source = Glean.Schema.Flow.Types.SourceOfTypeExport_source
type instance Glean.QueryOf Glean.Schema.Flow.Types.SourceOfTypeExport_source = Glean.Schema.Query.Flow.Types.SourceOfTypeExport_source

instance Glean.ToQuery Glean.Schema.Flow.Types.SourceOfTypeExport_source where
  toQuery (Glean.Schema.Flow.Types.SourceOfTypeExport_source_typeDeclaration x) = Data.Default.def { Glean.Schema.Query.Flow.Types.sourceOfTypeExport_source_typeDeclaration = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Flow.Types.SourceOfTypeExport_source_moduleTypeExport x) = Data.Default.def { Glean.Schema.Query.Flow.Types.sourceOfTypeExport_source_moduleTypeExport = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Flow.Types.SourceOfTypeExport_source_moduleNamespace x) = Data.Default.def { Glean.Schema.Query.Flow.Types.sourceOfTypeExport_source_moduleNamespace = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Flow.Types.TypeDeclaration Glean.Schema.Query.Flow.Types.SourceOfTypeExport_source where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Flow.Types.sourceOfTypeExport_source_typeDeclaration = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Flow.Types.ModuleTypeExport Glean.Schema.Query.Flow.Types.SourceOfTypeExport_source where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Flow.Types.sourceOfTypeExport_source_moduleTypeExport = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Flow.Types.Module Glean.Schema.Query.Flow.Types.SourceOfTypeExport_source where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Flow.Types.sourceOfTypeExport_source_moduleNamespace = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.TypeImportDeclaration_import_ = Glean.Schema.Flow.Types.TypeImportDeclaration_import_
type instance Glean.QueryOf Glean.Schema.Flow.Types.TypeImportDeclaration_import_ = Glean.Schema.Query.Flow.Types.TypeImportDeclaration_import_

instance Glean.ToQuery Glean.Schema.Flow.Types.TypeImportDeclaration_import_ where
  toQuery (Glean.Schema.Flow.Types.TypeImportDeclaration_import__type x) = Data.Default.def { Glean.Schema.Query.Flow.Types.typeImportDeclaration_import__type = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Flow.Types.TypeImportDeclaration_import__typeof_ x) = Data.Default.def { Glean.Schema.Query.Flow.Types.typeImportDeclaration_import__typeof_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Flow.Types.TypeImportDeclaration_import__moduleTypeof x) = Data.Default.def { Glean.Schema.Query.Flow.Types.typeImportDeclaration_import__moduleTypeof = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Flow.Types.ModuleTypeExport Glean.Schema.Query.Flow.Types.TypeImportDeclaration_import_ where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Flow.Types.typeImportDeclaration_import__type = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Flow.Types.ModuleExport Glean.Schema.Query.Flow.Types.TypeImportDeclaration_import_ where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Flow.Types.typeImportDeclaration_import__typeof_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Flow.Types.Module Glean.Schema.Query.Flow.Types.TypeImportDeclaration_import_ where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Flow.Types.typeImportDeclaration_import__moduleTypeof = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Flow.Types.SourceOfExport_source = Glean.Schema.Flow.Types.SourceOfExport_source
type instance Glean.QueryOf Glean.Schema.Flow.Types.SourceOfExport_source = Glean.Schema.Query.Flow.Types.SourceOfExport_source

instance Glean.ToQuery Glean.Schema.Flow.Types.SourceOfExport_source where
  toQuery (Glean.Schema.Flow.Types.SourceOfExport_source_declaration x) = Data.Default.def { Glean.Schema.Query.Flow.Types.sourceOfExport_source_declaration = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Flow.Types.SourceOfExport_source_memberDeclaration x) = Data.Default.def { Glean.Schema.Query.Flow.Types.sourceOfExport_source_memberDeclaration = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Flow.Types.SourceOfExport_source_moduleExport x) = Data.Default.def { Glean.Schema.Query.Flow.Types.sourceOfExport_source_moduleExport = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Flow.Types.SourceOfExport_source_moduleNamespace x) = Data.Default.def { Glean.Schema.Query.Flow.Types.sourceOfExport_source_moduleNamespace = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Flow.Types.Declaration Glean.Schema.Query.Flow.Types.SourceOfExport_source where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Flow.Types.sourceOfExport_source_declaration = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Flow.Types.MemberDeclaration Glean.Schema.Query.Flow.Types.SourceOfExport_source where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Flow.Types.sourceOfExport_source_memberDeclaration = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Flow.Types.ModuleExport Glean.Schema.Query.Flow.Types.SourceOfExport_source where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Flow.Types.sourceOfExport_source_moduleExport = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Flow.Types.Module Glean.Schema.Query.Flow.Types.SourceOfExport_source where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Flow.Types.sourceOfExport_source_moduleNamespace = Prelude.Just q }
