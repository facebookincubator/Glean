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

import qualified Glean.Schema.Hack.Types


type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.ClassDeclaration_key = Glean.Schema.Hack.Types.ClassDeclaration_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.ClassDeclaration_key = Glean.Schema.Query.Hack.Types.ClassDeclaration_key

instance Glean.ToQuery Glean.Schema.Hack.Types.ClassDeclaration_key where
  toQuery (Glean.Schema.Hack.Types.ClassDeclaration_key x1) = Glean.Schema.Query.Hack.Types.ClassDeclaration_key (Prelude.Just (Glean.toQuery x1))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.ClassDeclaration where
  toQueryId = Glean.Schema.Query.Hack.Types.ClassDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.ClassDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.ClassDeclaration = Glean.Schema.Hack.Types.ClassDeclaration
type instance Glean.QueryOf Glean.Schema.Hack.Types.ClassDeclaration = Glean.Schema.Query.Hack.Types.ClassDeclaration

instance Glean.ToQuery Glean.Schema.Hack.Types.ClassDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.QName_key = Glean.Schema.Hack.Types.QName_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.QName_key = Glean.Schema.Query.Hack.Types.QName_key

instance Glean.ToQuery Glean.Schema.Hack.Types.QName_key where
  toQuery (Glean.Schema.Hack.Types.QName_key x1 x2) = Glean.Schema.Query.Hack.Types.QName_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Hack.Types.qName_namespace__nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Hack.Types.qName_namespace__just = Prelude.Just (Glean.toQuery x)})) x2))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.QName where
  toQueryId = Glean.Schema.Query.Hack.Types.QName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.QName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.QName = Glean.Schema.Hack.Types.QName
type instance Glean.QueryOf Glean.Schema.Hack.Types.QName = Glean.Schema.Query.Hack.Types.QName

instance Glean.ToQuery Glean.Schema.Hack.Types.QName

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.TargetUses_key = Glean.Schema.Hack.Types.TargetUses_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.TargetUses_key = Glean.Schema.Query.Hack.Types.TargetUses_key

instance Glean.ToQuery Glean.Schema.Hack.Types.TargetUses_key where
  toQuery (Glean.Schema.Hack.Types.TargetUses_key x1 x2 x3) = Glean.Schema.Query.Hack.Types.TargetUses_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Glean.Schema.Query.Src.Types.ByteSpans_exact . Prelude.map Glean.toQuery) x3))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.TargetUses where
  toQueryId = Glean.Schema.Query.Hack.Types.TargetUses_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.TargetUses_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.TargetUses = Glean.Schema.Hack.Types.TargetUses
type instance Glean.QueryOf Glean.Schema.Hack.Types.TargetUses = Glean.Schema.Query.Hack.Types.TargetUses

instance Glean.ToQuery Glean.Schema.Hack.Types.TargetUses

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.Filename_key = Glean.Schema.Hack.Types.Filename_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.Filename_key = Glean.Schema.Query.Hack.Types.Filename_key

instance Glean.ToQuery Glean.Schema.Hack.Types.Filename_key where
  toQuery (Glean.Schema.Hack.Types.Filename_key x1 x2) = Glean.Schema.Query.Hack.Types.Filename_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.Filename where
  toQueryId = Glean.Schema.Query.Hack.Types.Filename_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.Filename_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.Filename = Glean.Schema.Hack.Types.Filename
type instance Glean.QueryOf Glean.Schema.Hack.Types.Filename = Glean.Schema.Query.Hack.Types.Filename

instance Glean.ToQuery Glean.Schema.Hack.Types.Filename

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.TargetUsesAbs_key = Glean.Schema.Hack.Types.TargetUsesAbs_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.TargetUsesAbs_key = Glean.Schema.Query.Hack.Types.TargetUsesAbs_key

instance Glean.ToQuery Glean.Schema.Hack.Types.TargetUsesAbs_key where
  toQuery (Glean.Schema.Hack.Types.TargetUsesAbs_key x1 x2 x3) = Glean.Schema.Query.Hack.Types.TargetUsesAbs_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Glean.Schema.Query.Src.Types.ByteSpans_exact . Prelude.map Glean.toQuery) x3))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.TargetUsesAbs where
  toQueryId = Glean.Schema.Query.Hack.Types.TargetUsesAbs_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.TargetUsesAbs_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.TargetUsesAbs = Glean.Schema.Hack.Types.TargetUsesAbs
type instance Glean.QueryOf Glean.Schema.Hack.Types.TargetUsesAbs = Glean.Schema.Query.Hack.Types.TargetUsesAbs

instance Glean.ToQuery Glean.Schema.Hack.Types.TargetUsesAbs

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.TraitDefinition_key = Glean.Schema.Hack.Types.TraitDefinition_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.TraitDefinition_key = Glean.Schema.Query.Hack.Types.TraitDefinition_key

instance Glean.ToQuery Glean.Schema.Hack.Types.TraitDefinition_key where
  toQuery (Glean.Schema.Hack.Types.TraitDefinition_key x1 x2 x3 x4 x5 x6 x7 x8 x9) = Glean.Schema.Query.Hack.Types.TraitDefinition_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.TraitDefinition_members_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Hack.Types.traitDefinition_namespace__nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Hack.Types.traitDefinition_namespace__just = Prelude.Just (Glean.toQuery x)})) x3)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.TraitDefinition_implements__array_exact . Prelude.map Glean.toQuery) x4)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.TraitDefinition_uses_array_exact . Prelude.map Glean.toQuery) x5)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.TraitDefinition_attributes_array_exact . Prelude.map Glean.toQuery) x6)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.TraitDefinition_typeParams_array_exact . Prelude.map Glean.toQuery) x7)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.TraitDefinition_requireExtends_array_exact . Prelude.map Glean.toQuery) x8)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.TraitDefinition_requireImplements_array_exact . Prelude.map Glean.toQuery) x9))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.TraitDefinition where
  toQueryId = Glean.Schema.Query.Hack.Types.TraitDefinition_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.TraitDefinition_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.TraitDefinition = Glean.Schema.Hack.Types.TraitDefinition
type instance Glean.QueryOf Glean.Schema.Hack.Types.TraitDefinition = Glean.Schema.Query.Hack.Types.TraitDefinition

instance Glean.ToQuery Glean.Schema.Hack.Types.TraitDefinition

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.DeclarationName_key = Glean.Schema.Hack.Types.DeclarationName_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.DeclarationName_key = Glean.Schema.Query.Hack.Types.DeclarationName_key

instance Glean.ToQuery Glean.Schema.Hack.Types.DeclarationName_key where
  toQuery (Glean.Schema.Hack.Types.DeclarationName_key x1 x2) = Glean.Schema.Query.Hack.Types.DeclarationName_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.DeclarationName where
  toQueryId = Glean.Schema.Query.Hack.Types.DeclarationName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.DeclarationName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.DeclarationName = Glean.Schema.Hack.Types.DeclarationName
type instance Glean.QueryOf Glean.Schema.Hack.Types.DeclarationName = Glean.Schema.Query.Hack.Types.DeclarationName

instance Glean.ToQuery Glean.Schema.Hack.Types.DeclarationName

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.FunctionDefinition_key = Glean.Schema.Hack.Types.FunctionDefinition_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.FunctionDefinition_key = Glean.Schema.Query.Hack.Types.FunctionDefinition_key

instance Glean.ToQuery Glean.Schema.Hack.Types.FunctionDefinition_key where
  toQuery (Glean.Schema.Hack.Types.FunctionDefinition_key x1 x2 x3 x4 x5 x6) = Glean.Schema.Query.Hack.Types.FunctionDefinition_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Hack.Types.functionDefinition_namespace__nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Hack.Types.functionDefinition_namespace__just = Prelude.Just (Glean.toQuery x)})) x4)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.FunctionDefinition_attributes_array_exact . Prelude.map Glean.toQuery) x5)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.FunctionDefinition_typeParams_array_exact . Prelude.map Glean.toQuery) x6))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.FunctionDefinition where
  toQueryId = Glean.Schema.Query.Hack.Types.FunctionDefinition_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.FunctionDefinition_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.FunctionDefinition = Glean.Schema.Hack.Types.FunctionDefinition
type instance Glean.QueryOf Glean.Schema.Hack.Types.FunctionDefinition = Glean.Schema.Query.Hack.Types.FunctionDefinition

instance Glean.ToQuery Glean.Schema.Hack.Types.FunctionDefinition

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.PropertyDeclaration_key = Glean.Schema.Hack.Types.PropertyDeclaration_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.PropertyDeclaration_key = Glean.Schema.Query.Hack.Types.PropertyDeclaration_key

instance Glean.ToQuery Glean.Schema.Hack.Types.PropertyDeclaration_key where
  toQuery (Glean.Schema.Hack.Types.PropertyDeclaration_key x1 x2) = Glean.Schema.Query.Hack.Types.PropertyDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.PropertyDeclaration where
  toQueryId = Glean.Schema.Query.Hack.Types.PropertyDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.PropertyDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.PropertyDeclaration = Glean.Schema.Hack.Types.PropertyDeclaration
type instance Glean.QueryOf Glean.Schema.Hack.Types.PropertyDeclaration = Glean.Schema.Query.Hack.Types.PropertyDeclaration

instance Glean.ToQuery Glean.Schema.Hack.Types.PropertyDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.PropertyDefinition_key = Glean.Schema.Hack.Types.PropertyDefinition_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.PropertyDefinition_key = Glean.Schema.Query.Hack.Types.PropertyDefinition_key

instance Glean.ToQuery Glean.Schema.Hack.Types.PropertyDefinition_key where
  toQuery (Glean.Schema.Hack.Types.PropertyDefinition_key x1 x2 x3 x4 x5 x6 x7) = Glean.Schema.Query.Hack.Types.PropertyDefinition_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Hack.Types.propertyDefinition_type_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Hack.Types.propertyDefinition_type_just = Prelude.Just (Glean.toQuery x)})) x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just (Glean.toQuery x4)) (Prelude.Just (Glean.toQuery x5)) (Prelude.Just (Glean.toQuery x6)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.PropertyDefinition_attributes_array_exact . Prelude.map Glean.toQuery) x7))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.PropertyDefinition where
  toQueryId = Glean.Schema.Query.Hack.Types.PropertyDefinition_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.PropertyDefinition_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.PropertyDefinition = Glean.Schema.Hack.Types.PropertyDefinition
type instance Glean.QueryOf Glean.Schema.Hack.Types.PropertyDefinition = Glean.Schema.Query.Hack.Types.PropertyDefinition

instance Glean.ToQuery Glean.Schema.Hack.Types.PropertyDefinition

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.ContainerChild_key = Glean.Schema.Hack.Types.ContainerChild_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.ContainerChild_key = Glean.Schema.Query.Hack.Types.ContainerChild_key

instance Glean.ToQuery Glean.Schema.Hack.Types.ContainerChild_key where
  toQuery (Glean.Schema.Hack.Types.ContainerChild_key x1 x2) = Glean.Schema.Query.Hack.Types.ContainerChild_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.ContainerChild where
  toQueryId = Glean.Schema.Query.Hack.Types.ContainerChild_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.ContainerChild_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.ContainerChild = Glean.Schema.Hack.Types.ContainerChild
type instance Glean.QueryOf Glean.Schema.Hack.Types.ContainerChild = Glean.Schema.Query.Hack.Types.ContainerChild

instance Glean.ToQuery Glean.Schema.Hack.Types.ContainerChild

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.SymbolNamespace_key = Glean.Schema.Hack.Types.SymbolNamespace_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.SymbolNamespace_key = Glean.Schema.Query.Hack.Types.SymbolNamespace_key

instance Glean.ToQuery Glean.Schema.Hack.Types.SymbolNamespace_key where
  toQuery (Glean.Schema.Hack.Types.SymbolNamespace_key x1 x2) = Glean.Schema.Query.Hack.Types.SymbolNamespace_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.SymbolNamespace where
  toQueryId = Glean.Schema.Query.Hack.Types.SymbolNamespace_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.SymbolNamespace_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.SymbolNamespace = Glean.Schema.Hack.Types.SymbolNamespace
type instance Glean.QueryOf Glean.Schema.Hack.Types.SymbolNamespace = Glean.Schema.Query.Hack.Types.SymbolNamespace

instance Glean.ToQuery Glean.Schema.Hack.Types.SymbolNamespace

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.UserAttribute_key = Glean.Schema.Hack.Types.UserAttribute_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.UserAttribute_key = Glean.Schema.Query.Hack.Types.UserAttribute_key

instance Glean.ToQuery Glean.Schema.Hack.Types.UserAttribute_key where
  toQuery (Glean.Schema.Hack.Types.UserAttribute_key x1 x2) = Glean.Schema.Query.Hack.Types.UserAttribute_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.UserAttribute_parameters_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.UserAttribute where
  toQueryId = Glean.Schema.Query.Hack.Types.UserAttribute_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.UserAttribute_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.UserAttribute = Glean.Schema.Hack.Types.UserAttribute
type instance Glean.QueryOf Glean.Schema.Hack.Types.UserAttribute = Glean.Schema.Query.Hack.Types.UserAttribute

instance Glean.ToQuery Glean.Schema.Hack.Types.UserAttribute

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.DeclarationSource_key = Glean.Schema.Hack.Types.DeclarationSource_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.DeclarationSource_key = Glean.Schema.Query.Hack.Types.DeclarationSource_key

instance Glean.ToQuery Glean.Schema.Hack.Types.DeclarationSource_key where
  toQuery (Glean.Schema.Hack.Types.DeclarationSource_key x1 x2) = Glean.Schema.Query.Hack.Types.DeclarationSource_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.DeclarationSource where
  toQueryId = Glean.Schema.Query.Hack.Types.DeclarationSource_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.DeclarationSource_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.DeclarationSource = Glean.Schema.Hack.Types.DeclarationSource
type instance Glean.QueryOf Glean.Schema.Hack.Types.DeclarationSource = Glean.Schema.Query.Hack.Types.DeclarationSource

instance Glean.ToQuery Glean.Schema.Hack.Types.DeclarationSource

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.NamespaceQName_key = Glean.Schema.Hack.Types.NamespaceQName_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.NamespaceQName_key = Glean.Schema.Query.Hack.Types.NamespaceQName_key

instance Glean.ToQuery Glean.Schema.Hack.Types.NamespaceQName_key where
  toQuery (Glean.Schema.Hack.Types.NamespaceQName_key x1 x2) = Glean.Schema.Query.Hack.Types.NamespaceQName_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Hack.Types.namespaceQName_parent_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Hack.Types.namespaceQName_parent_just = Prelude.Just (Glean.toQuery x)})) x2))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.NamespaceQName where
  toQueryId = Glean.Schema.Query.Hack.Types.NamespaceQName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.NamespaceQName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.NamespaceQName = Glean.Schema.Hack.Types.NamespaceQName
type instance Glean.QueryOf Glean.Schema.Hack.Types.NamespaceQName = Glean.Schema.Query.Hack.Types.NamespaceQName

instance Glean.ToQuery Glean.Schema.Hack.Types.NamespaceQName

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.InterfaceDeclaration_key = Glean.Schema.Hack.Types.InterfaceDeclaration_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.InterfaceDeclaration_key = Glean.Schema.Query.Hack.Types.InterfaceDeclaration_key

instance Glean.ToQuery Glean.Schema.Hack.Types.InterfaceDeclaration_key where
  toQuery (Glean.Schema.Hack.Types.InterfaceDeclaration_key x1) = Glean.Schema.Query.Hack.Types.InterfaceDeclaration_key (Prelude.Just (Glean.toQuery x1))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.InterfaceDeclaration where
  toQueryId = Glean.Schema.Query.Hack.Types.InterfaceDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.InterfaceDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.InterfaceDeclaration = Glean.Schema.Hack.Types.InterfaceDeclaration
type instance Glean.QueryOf Glean.Schema.Hack.Types.InterfaceDeclaration = Glean.Schema.Query.Hack.Types.InterfaceDeclaration

instance Glean.ToQuery Glean.Schema.Hack.Types.InterfaceDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.Kind_key = Glean.Schema.Hack.Types.Kind_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.Kind_key = Glean.Schema.Query.Hack.Types.Kind_key

instance Glean.ToQuery Glean.Schema.Hack.Types.Kind_key where
  toQuery (Glean.Schema.Hack.Types.Kind_key x1 x2) = Glean.Schema.Query.Hack.Types.Kind_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.Kind where
  toQueryId = Glean.Schema.Query.Hack.Types.Kind_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.Kind_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.Kind = Glean.Schema.Hack.Types.Kind
type instance Glean.QueryOf Glean.Schema.Hack.Types.Kind = Glean.Schema.Query.Hack.Types.Kind

instance Glean.ToQuery Glean.Schema.Hack.Types.Kind

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.InterfaceDefinition_key = Glean.Schema.Hack.Types.InterfaceDefinition_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.InterfaceDefinition_key = Glean.Schema.Query.Hack.Types.InterfaceDefinition_key

instance Glean.ToQuery Glean.Schema.Hack.Types.InterfaceDefinition_key where
  toQuery (Glean.Schema.Hack.Types.InterfaceDefinition_key x1 x2 x3 x4 x5 x6 x7) = Glean.Schema.Query.Hack.Types.InterfaceDefinition_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.InterfaceDefinition_members_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Hack.Types.interfaceDefinition_namespace__nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Hack.Types.interfaceDefinition_namespace__just = Prelude.Just (Glean.toQuery x)})) x3)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.InterfaceDefinition_extends__array_exact . Prelude.map Glean.toQuery) x4)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.InterfaceDefinition_attributes_array_exact . Prelude.map Glean.toQuery) x5)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.InterfaceDefinition_typeParams_array_exact . Prelude.map Glean.toQuery) x6)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.InterfaceDefinition_requireExtends_array_exact . Prelude.map Glean.toQuery) x7))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.InterfaceDefinition where
  toQueryId = Glean.Schema.Query.Hack.Types.InterfaceDefinition_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.InterfaceDefinition_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.InterfaceDefinition = Glean.Schema.Hack.Types.InterfaceDefinition
type instance Glean.QueryOf Glean.Schema.Hack.Types.InterfaceDefinition = Glean.Schema.Query.Hack.Types.InterfaceDefinition

instance Glean.ToQuery Glean.Schema.Hack.Types.InterfaceDefinition

instance Glean.PredicateQuery Glean.Schema.Hack.Types.Comment where
  toQueryId = Glean.Schema.Query.Hack.Types.Comment_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.Comment_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.Comment = Glean.Schema.Hack.Types.Comment
type instance Glean.QueryOf Glean.Schema.Hack.Types.Comment = Glean.Schema.Query.Hack.Types.Comment

instance Glean.ToQuery Glean.Schema.Hack.Types.Comment

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.Identifier_key = Glean.Schema.Hack.Types.Identifier_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.Identifier_key = Glean.Schema.Query.Hack.Types.Identifier_key

instance Glean.ToQuery Glean.Schema.Hack.Types.Identifier_key where
  toQuery (Glean.Schema.Hack.Types.Identifier_key x1 x2) = Glean.Schema.Query.Hack.Types.Identifier_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.Identifier where
  toQueryId = Glean.Schema.Query.Hack.Types.Identifier_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.Identifier_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.Identifier = Glean.Schema.Hack.Types.Identifier
type instance Glean.QueryOf Glean.Schema.Hack.Types.Identifier = Glean.Schema.Query.Hack.Types.Identifier

instance Glean.ToQuery Glean.Schema.Hack.Types.Identifier

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.NamespaceMember_key = Glean.Schema.Hack.Types.NamespaceMember_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.NamespaceMember_key = Glean.Schema.Query.Hack.Types.NamespaceMember_key

instance Glean.ToQuery Glean.Schema.Hack.Types.NamespaceMember_key where
  toQuery (Glean.Schema.Hack.Types.NamespaceMember_key x1 x2) = Glean.Schema.Query.Hack.Types.NamespaceMember_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.NamespaceMember where
  toQueryId = Glean.Schema.Query.Hack.Types.NamespaceMember_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.NamespaceMember_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.NamespaceMember = Glean.Schema.Hack.Types.NamespaceMember
type instance Glean.QueryOf Glean.Schema.Hack.Types.NamespaceMember = Glean.Schema.Query.Hack.Types.NamespaceMember

instance Glean.ToQuery Glean.Schema.Hack.Types.NamespaceMember

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.ContainerParent_key = Glean.Schema.Hack.Types.ContainerParent_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.ContainerParent_key = Glean.Schema.Query.Hack.Types.ContainerParent_key

instance Glean.ToQuery Glean.Schema.Hack.Types.ContainerParent_key where
  toQuery (Glean.Schema.Hack.Types.ContainerParent_key x1 x2) = Glean.Schema.Query.Hack.Types.ContainerParent_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.ContainerParent where
  toQueryId = Glean.Schema.Query.Hack.Types.ContainerParent_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.ContainerParent_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.ContainerParent = Glean.Schema.Hack.Types.ContainerParent
type instance Glean.QueryOf Glean.Schema.Hack.Types.ContainerParent = Glean.Schema.Query.Hack.Types.ContainerParent

instance Glean.ToQuery Glean.Schema.Hack.Types.ContainerParent

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.GlobalConstDefinition_key = Glean.Schema.Hack.Types.GlobalConstDefinition_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.GlobalConstDefinition_key = Glean.Schema.Query.Hack.Types.GlobalConstDefinition_key

instance Glean.ToQuery Glean.Schema.Hack.Types.GlobalConstDefinition_key where
  toQuery (Glean.Schema.Hack.Types.GlobalConstDefinition_key x1 x2 x3 x4) = Glean.Schema.Query.Hack.Types.GlobalConstDefinition_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Hack.Types.globalConstDefinition_type_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Hack.Types.globalConstDefinition_type_just = Prelude.Just (Glean.toQuery x)})) x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Hack.Types.globalConstDefinition_namespace__nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Hack.Types.globalConstDefinition_namespace__just = Prelude.Just (Glean.toQuery x)})) x3)) (Prelude.Just (Glean.toQuery x4))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.GlobalConstDefinition where
  toQueryId = Glean.Schema.Query.Hack.Types.GlobalConstDefinition_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.GlobalConstDefinition_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.GlobalConstDefinition = Glean.Schema.Hack.Types.GlobalConstDefinition
type instance Glean.QueryOf Glean.Schema.Hack.Types.GlobalConstDefinition = Glean.Schema.Query.Hack.Types.GlobalConstDefinition

instance Glean.ToQuery Glean.Schema.Hack.Types.GlobalConstDefinition

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.EnumDefinition_key = Glean.Schema.Hack.Types.EnumDefinition_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.EnumDefinition_key = Glean.Schema.Query.Hack.Types.EnumDefinition_key

instance Glean.ToQuery Glean.Schema.Hack.Types.EnumDefinition_key where
  toQuery (Glean.Schema.Hack.Types.EnumDefinition_key x1 x2 x3 x4 x5 x6) = Glean.Schema.Query.Hack.Types.EnumDefinition_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Hack.Types.enumDefinition_enumBase_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Hack.Types.enumDefinition_enumBase_just = Prelude.Just (Glean.toQuery x)})) x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Hack.Types.enumDefinition_enumConstraint_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Hack.Types.enumDefinition_enumConstraint_just = Prelude.Just (Glean.toQuery x)})) x3)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.EnumDefinition_enumerators_array_exact . Prelude.map Glean.toQuery) x4)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Hack.Types.enumDefinition_namespace__nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Hack.Types.enumDefinition_namespace__just = Prelude.Just (Glean.toQuery x)})) x5)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.EnumDefinition_attributes_array_exact . Prelude.map Glean.toQuery) x6))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.EnumDefinition where
  toQueryId = Glean.Schema.Query.Hack.Types.EnumDefinition_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.EnumDefinition_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.EnumDefinition = Glean.Schema.Hack.Types.EnumDefinition
type instance Glean.QueryOf Glean.Schema.Hack.Types.EnumDefinition = Glean.Schema.Query.Hack.Types.EnumDefinition

instance Glean.ToQuery Glean.Schema.Hack.Types.EnumDefinition

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.Symbol_key = Glean.Schema.Hack.Types.Symbol_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.Symbol_key = Glean.Schema.Query.Hack.Types.Symbol_key

instance Glean.ToQuery Glean.Schema.Hack.Types.Symbol_key where
  toQuery (Glean.Schema.Hack.Types.Symbol_key x1 x2 x3 x4 x5 x6 x7 x8) = Glean.Schema.Query.Hack.Types.Symbol_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just (Glean.toQuery x4)) (Prelude.Just (Glean.toQuery x5)) (Prelude.Just (Glean.toQuery x6)) (Prelude.Just (Glean.toQuery x7)) (Prelude.Just (Glean.toQuery x8))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.Symbol where
  toQueryId = Glean.Schema.Query.Hack.Types.Symbol_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.Symbol_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.Symbol = Glean.Schema.Hack.Types.Symbol
type instance Glean.QueryOf Glean.Schema.Hack.Types.Symbol = Glean.Schema.Query.Hack.Types.Symbol

instance Glean.ToQuery Glean.Schema.Hack.Types.Symbol

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.ClassConstDefinition_key = Glean.Schema.Hack.Types.ClassConstDefinition_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.ClassConstDefinition_key = Glean.Schema.Query.Hack.Types.ClassConstDefinition_key

instance Glean.ToQuery Glean.Schema.Hack.Types.ClassConstDefinition_key where
  toQuery (Glean.Schema.Hack.Types.ClassConstDefinition_key x1 x2 x3) = Glean.Schema.Query.Hack.Types.ClassConstDefinition_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Hack.Types.classConstDefinition_type_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Hack.Types.classConstDefinition_type_just = Prelude.Just (Glean.toQuery x)})) x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Hack.Types.classConstDefinition_value_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Hack.Types.classConstDefinition_value_just = Prelude.Just (Glean.toQuery x)})) x3))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.ClassConstDefinition where
  toQueryId = Glean.Schema.Query.Hack.Types.ClassConstDefinition_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.ClassConstDefinition_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.ClassConstDefinition = Glean.Schema.Hack.Types.ClassConstDefinition
type instance Glean.QueryOf Glean.Schema.Hack.Types.ClassConstDefinition = Glean.Schema.Query.Hack.Types.ClassConstDefinition

instance Glean.ToQuery Glean.Schema.Hack.Types.ClassConstDefinition

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.GlobalConstDeclaration_key = Glean.Schema.Hack.Types.GlobalConstDeclaration_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.GlobalConstDeclaration_key = Glean.Schema.Query.Hack.Types.GlobalConstDeclaration_key

instance Glean.ToQuery Glean.Schema.Hack.Types.GlobalConstDeclaration_key where
  toQuery (Glean.Schema.Hack.Types.GlobalConstDeclaration_key x1) = Glean.Schema.Query.Hack.Types.GlobalConstDeclaration_key (Prelude.Just (Glean.toQuery x1))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.GlobalConstDeclaration where
  toQueryId = Glean.Schema.Query.Hack.Types.GlobalConstDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.GlobalConstDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.GlobalConstDeclaration = Glean.Schema.Hack.Types.GlobalConstDeclaration
type instance Glean.QueryOf Glean.Schema.Hack.Types.GlobalConstDeclaration = Glean.Schema.Query.Hack.Types.GlobalConstDeclaration

instance Glean.ToQuery Glean.Schema.Hack.Types.GlobalConstDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.DeclarationTarget_key = Glean.Schema.Hack.Types.DeclarationTarget_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.DeclarationTarget_key = Glean.Schema.Query.Hack.Types.DeclarationTarget_key

instance Glean.ToQuery Glean.Schema.Hack.Types.DeclarationTarget_key where
  toQuery (Glean.Schema.Hack.Types.DeclarationTarget_key x1 x2) = Glean.Schema.Query.Hack.Types.DeclarationTarget_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.DeclarationTarget where
  toQueryId = Glean.Schema.Query.Hack.Types.DeclarationTarget_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.DeclarationTarget_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.DeclarationTarget = Glean.Schema.Hack.Types.DeclarationTarget
type instance Glean.QueryOf Glean.Schema.Hack.Types.DeclarationTarget = Glean.Schema.Query.Hack.Types.DeclarationTarget

instance Glean.ToQuery Glean.Schema.Hack.Types.DeclarationTarget

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.FileXRefs_key = Glean.Schema.Hack.Types.FileXRefs_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.FileXRefs_key = Glean.Schema.Query.Hack.Types.FileXRefs_key

instance Glean.ToQuery Glean.Schema.Hack.Types.FileXRefs_key where
  toQuery (Glean.Schema.Hack.Types.FileXRefs_key x1 x2) = Glean.Schema.Query.Hack.Types.FileXRefs_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.FileXRefs_xrefs_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.FileXRefs where
  toQueryId = Glean.Schema.Query.Hack.Types.FileXRefs_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.FileXRefs_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.FileXRefs = Glean.Schema.Hack.Types.FileXRefs
type instance Glean.QueryOf Glean.Schema.Hack.Types.FileXRefs = Glean.Schema.Query.Hack.Types.FileXRefs

instance Glean.ToQuery Glean.Schema.Hack.Types.FileXRefs

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.DeclarationSpan_key = Glean.Schema.Hack.Types.DeclarationSpan_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.DeclarationSpan_key = Glean.Schema.Query.Hack.Types.DeclarationSpan_key

instance Glean.ToQuery Glean.Schema.Hack.Types.DeclarationSpan_key where
  toQuery (Glean.Schema.Hack.Types.DeclarationSpan_key x1 x2 x3) = Glean.Schema.Query.Hack.Types.DeclarationSpan_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.DeclarationSpan where
  toQueryId = Glean.Schema.Query.Hack.Types.DeclarationSpan_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.DeclarationSpan_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.DeclarationSpan = Glean.Schema.Hack.Types.DeclarationSpan
type instance Glean.QueryOf Glean.Schema.Hack.Types.DeclarationSpan = Glean.Schema.Query.Hack.Types.DeclarationSpan

instance Glean.ToQuery Glean.Schema.Hack.Types.DeclarationSpan

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.Enumerator_key = Glean.Schema.Hack.Types.Enumerator_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.Enumerator_key = Glean.Schema.Query.Hack.Types.Enumerator_key

instance Glean.ToQuery Glean.Schema.Hack.Types.Enumerator_key where
  toQuery (Glean.Schema.Hack.Types.Enumerator_key x1 x2) = Glean.Schema.Query.Hack.Types.Enumerator_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.Enumerator where
  toQueryId = Glean.Schema.Query.Hack.Types.Enumerator_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.Enumerator_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.Enumerator = Glean.Schema.Hack.Types.Enumerator
type instance Glean.QueryOf Glean.Schema.Hack.Types.Enumerator = Glean.Schema.Query.Hack.Types.Enumerator

instance Glean.ToQuery Glean.Schema.Hack.Types.Enumerator

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.Signature_key = Glean.Schema.Hack.Types.Signature_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.Signature_key = Glean.Schema.Query.Hack.Types.Signature_key

instance Glean.ToQuery Glean.Schema.Hack.Types.Signature_key where
  toQuery (Glean.Schema.Hack.Types.Signature_key x1 x2) = Glean.Schema.Query.Hack.Types.Signature_key (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Hack.Types.signature_returns_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Hack.Types.signature_returns_just = Prelude.Just (Glean.toQuery x)})) x1)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.Signature_parameters_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.Signature where
  toQueryId = Glean.Schema.Query.Hack.Types.Signature_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.Signature_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.Signature = Glean.Schema.Hack.Types.Signature
type instance Glean.QueryOf Glean.Schema.Hack.Types.Signature = Glean.Schema.Query.Hack.Types.Signature

instance Glean.ToQuery Glean.Schema.Hack.Types.Signature

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.TypedefDeclaration_key = Glean.Schema.Hack.Types.TypedefDeclaration_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.TypedefDeclaration_key = Glean.Schema.Query.Hack.Types.TypedefDeclaration_key

instance Glean.ToQuery Glean.Schema.Hack.Types.TypedefDeclaration_key where
  toQuery (Glean.Schema.Hack.Types.TypedefDeclaration_key x1 x2 x3 x4 x5) = Glean.Schema.Query.Hack.Types.TypedefDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Hack.Types.typedefDeclaration_namespace__nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Hack.Types.typedefDeclaration_namespace__just = Prelude.Just (Glean.toQuery x)})) x3)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.TypedefDeclaration_attributes_array_exact . Prelude.map Glean.toQuery) x4)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.TypedefDeclaration_typeParams_array_exact . Prelude.map Glean.toQuery) x5))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.TypedefDeclaration where
  toQueryId = Glean.Schema.Query.Hack.Types.TypedefDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.TypedefDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.TypedefDeclaration = Glean.Schema.Hack.Types.TypedefDeclaration
type instance Glean.QueryOf Glean.Schema.Hack.Types.TypedefDeclaration = Glean.Schema.Query.Hack.Types.TypedefDeclaration

instance Glean.ToQuery Glean.Schema.Hack.Types.TypedefDeclaration

instance Glean.PredicateQuery Glean.Schema.Hack.Types.Name where
  toQueryId = Glean.Schema.Query.Hack.Types.Name_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.Name_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.Name = Glean.Schema.Hack.Types.Name
type instance Glean.QueryOf Glean.Schema.Hack.Types.Name = Glean.Schema.Query.Hack.Types.Name

instance Glean.ToQuery Glean.Schema.Hack.Types.Name

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.MethodDeclaration_key = Glean.Schema.Hack.Types.MethodDeclaration_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.MethodDeclaration_key = Glean.Schema.Query.Hack.Types.MethodDeclaration_key

instance Glean.ToQuery Glean.Schema.Hack.Types.MethodDeclaration_key where
  toQuery (Glean.Schema.Hack.Types.MethodDeclaration_key x1 x2) = Glean.Schema.Query.Hack.Types.MethodDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.MethodDeclaration where
  toQueryId = Glean.Schema.Query.Hack.Types.MethodDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.MethodDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.MethodDeclaration = Glean.Schema.Hack.Types.MethodDeclaration
type instance Glean.QueryOf Glean.Schema.Hack.Types.MethodDeclaration = Glean.Schema.Query.Hack.Types.MethodDeclaration

instance Glean.ToQuery Glean.Schema.Hack.Types.MethodDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.TraitDeclaration_key = Glean.Schema.Hack.Types.TraitDeclaration_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.TraitDeclaration_key = Glean.Schema.Query.Hack.Types.TraitDeclaration_key

instance Glean.ToQuery Glean.Schema.Hack.Types.TraitDeclaration_key where
  toQuery (Glean.Schema.Hack.Types.TraitDeclaration_key x1) = Glean.Schema.Query.Hack.Types.TraitDeclaration_key (Prelude.Just (Glean.toQuery x1))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.TraitDeclaration where
  toQueryId = Glean.Schema.Query.Hack.Types.TraitDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.TraitDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.TraitDeclaration = Glean.Schema.Hack.Types.TraitDeclaration
type instance Glean.QueryOf Glean.Schema.Hack.Types.TraitDeclaration = Glean.Schema.Query.Hack.Types.TraitDeclaration

instance Glean.ToQuery Glean.Schema.Hack.Types.TraitDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.FunctionDeclaration_key = Glean.Schema.Hack.Types.FunctionDeclaration_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.FunctionDeclaration_key = Glean.Schema.Query.Hack.Types.FunctionDeclaration_key

instance Glean.ToQuery Glean.Schema.Hack.Types.FunctionDeclaration_key where
  toQuery (Glean.Schema.Hack.Types.FunctionDeclaration_key x1) = Glean.Schema.Query.Hack.Types.FunctionDeclaration_key (Prelude.Just (Glean.toQuery x1))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.FunctionDeclaration where
  toQueryId = Glean.Schema.Query.Hack.Types.FunctionDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.FunctionDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.FunctionDeclaration = Glean.Schema.Hack.Types.FunctionDeclaration
type instance Glean.QueryOf Glean.Schema.Hack.Types.FunctionDeclaration = Glean.Schema.Query.Hack.Types.FunctionDeclaration

instance Glean.ToQuery Glean.Schema.Hack.Types.FunctionDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.MethodDefinition_key = Glean.Schema.Hack.Types.MethodDefinition_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.MethodDefinition_key = Glean.Schema.Query.Hack.Types.MethodDefinition_key

instance Glean.ToQuery Glean.Schema.Hack.Types.MethodDefinition_key where
  toQuery (Glean.Schema.Hack.Types.MethodDefinition_key x1 x2 x3 x4 x5 x6 x7 x8 x9) = Glean.Schema.Query.Hack.Types.MethodDefinition_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just (Glean.toQuery x4)) (Prelude.Just (Glean.toQuery x5)) (Prelude.Just (Glean.toQuery x6)) (Prelude.Just (Glean.toQuery x7)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.MethodDefinition_attributes_array_exact . Prelude.map Glean.toQuery) x8)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.MethodDefinition_typeParams_array_exact . Prelude.map Glean.toQuery) x9))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.MethodDefinition where
  toQueryId = Glean.Schema.Query.Hack.Types.MethodDefinition_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.MethodDefinition_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.MethodDefinition = Glean.Schema.Hack.Types.MethodDefinition
type instance Glean.QueryOf Glean.Schema.Hack.Types.MethodDefinition = Glean.Schema.Query.Hack.Types.MethodDefinition

instance Glean.ToQuery Glean.Schema.Hack.Types.MethodDefinition

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.FileDeclarations_key = Glean.Schema.Hack.Types.FileDeclarations_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.FileDeclarations_key = Glean.Schema.Query.Hack.Types.FileDeclarations_key

instance Glean.ToQuery Glean.Schema.Hack.Types.FileDeclarations_key where
  toQuery (Glean.Schema.Hack.Types.FileDeclarations_key x1 x2) = Glean.Schema.Query.Hack.Types.FileDeclarations_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.FileDeclarations_declarations_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.FileDeclarations where
  toQueryId = Glean.Schema.Query.Hack.Types.FileDeclarations_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.FileDeclarations_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.FileDeclarations = Glean.Schema.Hack.Types.FileDeclarations
type instance Glean.QueryOf Glean.Schema.Hack.Types.FileDeclarations = Glean.Schema.Query.Hack.Types.FileDeclarations

instance Glean.ToQuery Glean.Schema.Hack.Types.FileDeclarations

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.DeclarationLocation_key = Glean.Schema.Hack.Types.DeclarationLocation_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.DeclarationLocation_key = Glean.Schema.Query.Hack.Types.DeclarationLocation_key

instance Glean.ToQuery Glean.Schema.Hack.Types.DeclarationLocation_key where
  toQuery (Glean.Schema.Hack.Types.DeclarationLocation_key x1 x2 x3) = Glean.Schema.Query.Hack.Types.DeclarationLocation_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.DeclarationLocation where
  toQueryId = Glean.Schema.Query.Hack.Types.DeclarationLocation_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.DeclarationLocation_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.DeclarationLocation = Glean.Schema.Hack.Types.DeclarationLocation
type instance Glean.QueryOf Glean.Schema.Hack.Types.DeclarationLocation = Glean.Schema.Query.Hack.Types.DeclarationLocation

instance Glean.ToQuery Glean.Schema.Hack.Types.DeclarationLocation

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.TypeConstDeclaration_key = Glean.Schema.Hack.Types.TypeConstDeclaration_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.TypeConstDeclaration_key = Glean.Schema.Query.Hack.Types.TypeConstDeclaration_key

instance Glean.ToQuery Glean.Schema.Hack.Types.TypeConstDeclaration_key where
  toQuery (Glean.Schema.Hack.Types.TypeConstDeclaration_key x1 x2) = Glean.Schema.Query.Hack.Types.TypeConstDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.TypeConstDeclaration where
  toQueryId = Glean.Schema.Query.Hack.Types.TypeConstDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.TypeConstDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.TypeConstDeclaration = Glean.Schema.Hack.Types.TypeConstDeclaration
type instance Glean.QueryOf Glean.Schema.Hack.Types.TypeConstDeclaration = Glean.Schema.Query.Hack.Types.TypeConstDeclaration

instance Glean.ToQuery Glean.Schema.Hack.Types.TypeConstDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.NameLowerCase_key = Glean.Schema.Hack.Types.NameLowerCase_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.NameLowerCase_key = Glean.Schema.Query.Hack.Types.NameLowerCase_key

instance Glean.ToQuery Glean.Schema.Hack.Types.NameLowerCase_key where
  toQuery (Glean.Schema.Hack.Types.NameLowerCase_key x1 x2) = Glean.Schema.Query.Hack.Types.NameLowerCase_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.NameLowerCase where
  toQueryId = Glean.Schema.Query.Hack.Types.NameLowerCase_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.NameLowerCase_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.NameLowerCase = Glean.Schema.Hack.Types.NameLowerCase
type instance Glean.QueryOf Glean.Schema.Hack.Types.NameLowerCase = Glean.Schema.Query.Hack.Types.NameLowerCase

instance Glean.ToQuery Glean.Schema.Hack.Types.NameLowerCase

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.TypeConstDefinition_key = Glean.Schema.Hack.Types.TypeConstDefinition_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.TypeConstDefinition_key = Glean.Schema.Query.Hack.Types.TypeConstDefinition_key

instance Glean.ToQuery Glean.Schema.Hack.Types.TypeConstDefinition_key where
  toQuery (Glean.Schema.Hack.Types.TypeConstDefinition_key x1 x2 x3 x4) = Glean.Schema.Query.Hack.Types.TypeConstDefinition_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Hack.Types.typeConstDefinition_type_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Hack.Types.typeConstDefinition_type_just = Prelude.Just (Glean.toQuery x)})) x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.TypeConstDefinition_attributes_array_exact . Prelude.map Glean.toQuery) x4))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.TypeConstDefinition where
  toQueryId = Glean.Schema.Query.Hack.Types.TypeConstDefinition_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.TypeConstDefinition_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.TypeConstDefinition = Glean.Schema.Hack.Types.TypeConstDefinition
type instance Glean.QueryOf Glean.Schema.Hack.Types.TypeConstDefinition = Glean.Schema.Query.Hack.Types.TypeConstDefinition

instance Glean.ToQuery Glean.Schema.Hack.Types.TypeConstDefinition

instance Glean.PredicateQuery Glean.Schema.Hack.Types.Type where
  toQueryId = Glean.Schema.Query.Hack.Types.Type__with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.Type__with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.Type_ = Glean.Schema.Hack.Types.Type
type instance Glean.QueryOf Glean.Schema.Hack.Types.Type = Glean.Schema.Query.Hack.Types.Type_

instance Glean.ToQuery Glean.Schema.Hack.Types.Type

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.DeclarationComment_key = Glean.Schema.Hack.Types.DeclarationComment_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.DeclarationComment_key = Glean.Schema.Query.Hack.Types.DeclarationComment_key

instance Glean.ToQuery Glean.Schema.Hack.Types.DeclarationComment_key where
  toQuery (Glean.Schema.Hack.Types.DeclarationComment_key x1 x2 x3 x4) = Glean.Schema.Query.Hack.Types.DeclarationComment_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Hack.Types.declarationComment_span_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Hack.Types.declarationComment_span_just = Prelude.Just (Glean.toQuery x)})) x3)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Hack.Types.declarationComment_comment_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Hack.Types.declarationComment_comment_just = Prelude.Just (Glean.toQuery x)})) x4))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.DeclarationComment where
  toQueryId = Glean.Schema.Query.Hack.Types.DeclarationComment_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.DeclarationComment_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.DeclarationComment = Glean.Schema.Hack.Types.DeclarationComment
type instance Glean.QueryOf Glean.Schema.Hack.Types.DeclarationComment = Glean.Schema.Query.Hack.Types.DeclarationComment

instance Glean.ToQuery Glean.Schema.Hack.Types.DeclarationComment

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.ClassConstDeclaration_key = Glean.Schema.Hack.Types.ClassConstDeclaration_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.ClassConstDeclaration_key = Glean.Schema.Query.Hack.Types.ClassConstDeclaration_key

instance Glean.ToQuery Glean.Schema.Hack.Types.ClassConstDeclaration_key where
  toQuery (Glean.Schema.Hack.Types.ClassConstDeclaration_key x1 x2) = Glean.Schema.Query.Hack.Types.ClassConstDeclaration_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.ClassConstDeclaration where
  toQueryId = Glean.Schema.Query.Hack.Types.ClassConstDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.ClassConstDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.ClassConstDeclaration = Glean.Schema.Hack.Types.ClassConstDeclaration
type instance Glean.QueryOf Glean.Schema.Hack.Types.ClassConstDeclaration = Glean.Schema.Query.Hack.Types.ClassConstDeclaration

instance Glean.ToQuery Glean.Schema.Hack.Types.ClassConstDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.EnumDeclaration_key = Glean.Schema.Hack.Types.EnumDeclaration_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.EnumDeclaration_key = Glean.Schema.Query.Hack.Types.EnumDeclaration_key

instance Glean.ToQuery Glean.Schema.Hack.Types.EnumDeclaration_key where
  toQuery (Glean.Schema.Hack.Types.EnumDeclaration_key x1) = Glean.Schema.Query.Hack.Types.EnumDeclaration_key (Prelude.Just (Glean.toQuery x1))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.EnumDeclaration where
  toQueryId = Glean.Schema.Query.Hack.Types.EnumDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.EnumDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.EnumDeclaration = Glean.Schema.Hack.Types.EnumDeclaration
type instance Glean.QueryOf Glean.Schema.Hack.Types.EnumDeclaration = Glean.Schema.Query.Hack.Types.EnumDeclaration

instance Glean.ToQuery Glean.Schema.Hack.Types.EnumDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.NamespaceDeclaration_key = Glean.Schema.Hack.Types.NamespaceDeclaration_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.NamespaceDeclaration_key = Glean.Schema.Query.Hack.Types.NamespaceDeclaration_key

instance Glean.ToQuery Glean.Schema.Hack.Types.NamespaceDeclaration_key where
  toQuery (Glean.Schema.Hack.Types.NamespaceDeclaration_key x1) = Glean.Schema.Query.Hack.Types.NamespaceDeclaration_key (Prelude.Just (Glean.toQuery x1))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.NamespaceDeclaration where
  toQueryId = Glean.Schema.Query.Hack.Types.NamespaceDeclaration_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.NamespaceDeclaration_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.NamespaceDeclaration = Glean.Schema.Hack.Types.NamespaceDeclaration
type instance Glean.QueryOf Glean.Schema.Hack.Types.NamespaceDeclaration = Glean.Schema.Query.Hack.Types.NamespaceDeclaration

instance Glean.ToQuery Glean.Schema.Hack.Types.NamespaceDeclaration

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.ClassDefinition_key = Glean.Schema.Hack.Types.ClassDefinition_key
type instance Glean.QueryOf Glean.Schema.Hack.Types.ClassDefinition_key = Glean.Schema.Query.Hack.Types.ClassDefinition_key

instance Glean.ToQuery Glean.Schema.Hack.Types.ClassDefinition_key where
  toQuery (Glean.Schema.Hack.Types.ClassDefinition_key x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = Glean.Schema.Query.Hack.Types.ClassDefinition_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.ClassDefinition_members_array_exact . Prelude.map Glean.toQuery) x4)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Hack.Types.classDefinition_namespace__nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Hack.Types.classDefinition_namespace__just = Prelude.Just (Glean.toQuery x)})) x5)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Hack.Types.classDefinition_extends__nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Hack.Types.classDefinition_extends__just = Prelude.Just (Glean.toQuery x)})) x6)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.ClassDefinition_implements__array_exact . Prelude.map Glean.toQuery) x7)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.ClassDefinition_uses_array_exact . Prelude.map Glean.toQuery) x8)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.ClassDefinition_attributes_array_exact . Prelude.map Glean.toQuery) x9)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.ClassDefinition_typeParams_array_exact . Prelude.map Glean.toQuery) x10))

instance Glean.PredicateQuery Glean.Schema.Hack.Types.ClassDefinition where
  toQueryId = Glean.Schema.Query.Hack.Types.ClassDefinition_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Hack.Types.ClassDefinition_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.ClassDefinition = Glean.Schema.Hack.Types.ClassDefinition
type instance Glean.QueryOf Glean.Schema.Hack.Types.ClassDefinition = Glean.Schema.Query.Hack.Types.ClassDefinition

instance Glean.ToQuery Glean.Schema.Hack.Types.ClassDefinition

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.TypeParameter = Glean.Schema.Hack.Types.TypeParameter
type instance Glean.QueryOf Glean.Schema.Hack.Types.TypeParameter = Glean.Schema.Query.Hack.Types.TypeParameter

instance Glean.ToQuery Glean.Schema.Hack.Types.TypeParameter where
  toQuery (Glean.Schema.Hack.Types.TypeParameter x1 x2 x3 x4 x5) = Glean.Schema.Query.Hack.Types.TypeParameter (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.TypeParameter_constraints_array_exact . Prelude.map Glean.toQuery) x4)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.TypeParameter_attributes_array_exact . Prelude.map Glean.toQuery) x5))

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.ContainerDeclaration = Glean.Schema.Hack.Types.ContainerDeclaration
type instance Glean.QueryOf Glean.Schema.Hack.Types.ContainerDeclaration = Glean.Schema.Query.Hack.Types.ContainerDeclaration

instance Glean.ToQuery Glean.Schema.Hack.Types.ContainerDeclaration where
  toQuery (Glean.Schema.Hack.Types.ContainerDeclaration_class_ x) = Data.Default.def { Glean.Schema.Query.Hack.Types.containerDeclaration_class_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Hack.Types.ContainerDeclaration_interface_ x) = Data.Default.def { Glean.Schema.Query.Hack.Types.containerDeclaration_interface_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Hack.Types.ContainerDeclaration_trait x) = Data.Default.def { Glean.Schema.Query.Hack.Types.containerDeclaration_trait = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Hack.Types.ClassDeclaration Glean.Schema.Query.Hack.Types.ContainerDeclaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Hack.Types.containerDeclaration_class_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Hack.Types.InterfaceDeclaration Glean.Schema.Query.Hack.Types.ContainerDeclaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Hack.Types.containerDeclaration_interface_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Hack.Types.TraitDeclaration Glean.Schema.Query.Hack.Types.ContainerDeclaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Hack.Types.containerDeclaration_trait = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.Visibility = Glean.Schema.Hack.Types.Visibility
type instance Glean.QueryOf Glean.Schema.Hack.Types.Visibility = Glean.Schema.Query.Hack.Types.Visibility

instance Glean.ToQuery Glean.Schema.Hack.Types.Visibility where
  toQuery Glean.Schema.Hack.Types.Visibility_Private = Glean.Schema.Query.Hack.Types.Visibility_Private
  toQuery Glean.Schema.Hack.Types.Visibility_Protected = Glean.Schema.Query.Hack.Types.Visibility_Protected
  toQuery Glean.Schema.Hack.Types.Visibility_Public = Glean.Schema.Query.Hack.Types.Visibility_Public

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.XRef = Glean.Schema.Hack.Types.XRef
type instance Glean.QueryOf Glean.Schema.Hack.Types.XRef = Glean.Schema.Query.Hack.Types.XRef

instance Glean.ToQuery Glean.Schema.Hack.Types.XRef where
  toQuery (Glean.Schema.Hack.Types.XRef x1 x2) = Glean.Schema.Query.Hack.Types.XRef (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Src.Types.ByteSpans_exact . Prelude.map Glean.toQuery) x2))

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.Parameter = Glean.Schema.Hack.Types.Parameter
type instance Glean.QueryOf Glean.Schema.Hack.Types.Parameter = Glean.Schema.Query.Hack.Types.Parameter

instance Glean.ToQuery Glean.Schema.Hack.Types.Parameter where
  toQuery (Glean.Schema.Hack.Types.Parameter x1 x2 x3 x4 x5 x6) = Glean.Schema.Query.Hack.Types.Parameter (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Hack.Types.parameter_type_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Hack.Types.parameter_type_just = Prelude.Just (Glean.toQuery x)})) x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just (Glean.toQuery x4)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Hack.Types.parameter_defaultValue_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Hack.Types.parameter_defaultValue_just = Prelude.Just (Glean.toQuery x)})) x5)) (Prelude.Just ((Glean.Schema.Query.Hack.Types.Parameter_attributes_array_exact . Prelude.map Glean.toQuery) x6))

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.ReifyKind = Glean.Schema.Hack.Types.ReifyKind
type instance Glean.QueryOf Glean.Schema.Hack.Types.ReifyKind = Glean.Schema.Query.Hack.Types.ReifyKind

instance Glean.ToQuery Glean.Schema.Hack.Types.ReifyKind where
  toQuery Glean.Schema.Hack.Types.ReifyKind_Erased = Glean.Schema.Query.Hack.Types.ReifyKind_Erased
  toQuery Glean.Schema.Hack.Types.ReifyKind_Reified = Glean.Schema.Query.Hack.Types.ReifyKind_Reified
  toQuery Glean.Schema.Hack.Types.ReifyKind_SoftReified = Glean.Schema.Query.Hack.Types.ReifyKind_SoftReified

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.Context = Glean.Schema.Hack.Types.Context
type instance Glean.QueryOf Glean.Schema.Hack.Types.Context = Glean.Schema.Query.Hack.Types.Context

instance Glean.ToQuery Glean.Schema.Hack.Types.Context where
  toQuery (Glean.Schema.Hack.Types.Context x1 x2 x3) = Glean.Schema.Query.Hack.Types.Context (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.Constraint = Glean.Schema.Hack.Types.Constraint
type instance Glean.QueryOf Glean.Schema.Hack.Types.Constraint = Glean.Schema.Query.Hack.Types.Constraint

instance Glean.ToQuery Glean.Schema.Hack.Types.Constraint where
  toQuery (Glean.Schema.Hack.Types.Constraint x1 x2) = Glean.Schema.Query.Hack.Types.Constraint (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.Variance = Glean.Schema.Hack.Types.Variance
type instance Glean.QueryOf Glean.Schema.Hack.Types.Variance = Glean.Schema.Query.Hack.Types.Variance

instance Glean.ToQuery Glean.Schema.Hack.Types.Variance where
  toQuery Glean.Schema.Hack.Types.Variance_Contravariant = Glean.Schema.Query.Hack.Types.Variance_Contravariant
  toQuery Glean.Schema.Hack.Types.Variance_Covariant = Glean.Schema.Query.Hack.Types.Variance_Covariant
  toQuery Glean.Schema.Hack.Types.Variance_Invariant = Glean.Schema.Query.Hack.Types.Variance_Invariant

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.TypeConstKind = Glean.Schema.Hack.Types.TypeConstKind
type instance Glean.QueryOf Glean.Schema.Hack.Types.TypeConstKind = Glean.Schema.Query.Hack.Types.TypeConstKind

instance Glean.ToQuery Glean.Schema.Hack.Types.TypeConstKind where
  toQuery Glean.Schema.Hack.Types.TypeConstKind_Abstract = Glean.Schema.Query.Hack.Types.TypeConstKind_Abstract
  toQuery Glean.Schema.Hack.Types.TypeConstKind_Concrete = Glean.Schema.Query.Hack.Types.TypeConstKind_Concrete
  toQuery Glean.Schema.Hack.Types.TypeConstKind_PartiallyAbstract = Glean.Schema.Query.Hack.Types.TypeConstKind_PartiallyAbstract

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.ConstraintKind = Glean.Schema.Hack.Types.ConstraintKind
type instance Glean.QueryOf Glean.Schema.Hack.Types.ConstraintKind = Glean.Schema.Query.Hack.Types.ConstraintKind

instance Glean.ToQuery Glean.Schema.Hack.Types.ConstraintKind where
  toQuery Glean.Schema.Hack.Types.ConstraintKind_As = Glean.Schema.Query.Hack.Types.ConstraintKind_As
  toQuery Glean.Schema.Hack.Types.ConstraintKind_Equal = Glean.Schema.Query.Hack.Types.ConstraintKind_Equal
  toQuery Glean.Schema.Hack.Types.ConstraintKind_Super = Glean.Schema.Query.Hack.Types.ConstraintKind_Super

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.Declaration = Glean.Schema.Hack.Types.Declaration
type instance Glean.QueryOf Glean.Schema.Hack.Types.Declaration = Glean.Schema.Query.Hack.Types.Declaration

instance Glean.ToQuery Glean.Schema.Hack.Types.Declaration where
  toQuery (Glean.Schema.Hack.Types.Declaration_classConst x) = Data.Default.def { Glean.Schema.Query.Hack.Types.declaration_classConst = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Hack.Types.Declaration_container x) = Data.Default.def { Glean.Schema.Query.Hack.Types.declaration_container = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Hack.Types.Declaration_enum_ x) = Data.Default.def { Glean.Schema.Query.Hack.Types.declaration_enum_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Hack.Types.Declaration_enumerator x) = Data.Default.def { Glean.Schema.Query.Hack.Types.declaration_enumerator = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Hack.Types.Declaration_function_ x) = Data.Default.def { Glean.Schema.Query.Hack.Types.declaration_function_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Hack.Types.Declaration_globalConst x) = Data.Default.def { Glean.Schema.Query.Hack.Types.declaration_globalConst = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Hack.Types.Declaration_namespace_ x) = Data.Default.def { Glean.Schema.Query.Hack.Types.declaration_namespace_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Hack.Types.Declaration_method x) = Data.Default.def { Glean.Schema.Query.Hack.Types.declaration_method = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Hack.Types.Declaration_property_ x) = Data.Default.def { Glean.Schema.Query.Hack.Types.declaration_property_ = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Hack.Types.Declaration_typeConst x) = Data.Default.def { Glean.Schema.Query.Hack.Types.declaration_typeConst = Prelude.Just (Glean.toQuery x) }
  toQuery (Glean.Schema.Hack.Types.Declaration_typedef_ x) = Data.Default.def { Glean.Schema.Query.Hack.Types.declaration_typedef_ = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Hack.Types.ClassConstDeclaration Glean.Schema.Query.Hack.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Hack.Types.declaration_classConst = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Hack.Types.ContainerDeclaration Glean.Schema.Query.Hack.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Hack.Types.declaration_container = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Hack.Types.EnumDeclaration Glean.Schema.Query.Hack.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Hack.Types.declaration_enum_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Hack.Types.Enumerator Glean.Schema.Query.Hack.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Hack.Types.declaration_enumerator = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Hack.Types.FunctionDeclaration Glean.Schema.Query.Hack.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Hack.Types.declaration_function_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Hack.Types.GlobalConstDeclaration Glean.Schema.Query.Hack.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Hack.Types.declaration_globalConst = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Hack.Types.NamespaceDeclaration Glean.Schema.Query.Hack.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Hack.Types.declaration_namespace_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Hack.Types.MethodDeclaration Glean.Schema.Query.Hack.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Hack.Types.declaration_method = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Hack.Types.PropertyDeclaration Glean.Schema.Query.Hack.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Hack.Types.declaration_property_ = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Hack.Types.TypeConstDeclaration Glean.Schema.Query.Hack.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Hack.Types.declaration_typeConst = Prelude.Just q }

instance Glean.SumQuery Glean.Schema.Query.Hack.Types.TypedefDeclaration Glean.Schema.Query.Hack.Types.Declaration where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Hack.Types.declaration_typedef_ = Prelude.Just q }

type instance Glean.QueryResult Glean.Schema.Query.Hack.Types.XRefTarget = Glean.Schema.Hack.Types.XRefTarget
type instance Glean.QueryOf Glean.Schema.Hack.Types.XRefTarget = Glean.Schema.Query.Hack.Types.XRefTarget

instance Glean.ToQuery Glean.Schema.Hack.Types.XRefTarget where
  toQuery (Glean.Schema.Hack.Types.XRefTarget_declaration x) = Data.Default.def { Glean.Schema.Query.Hack.Types.xRefTarget_declaration = Prelude.Just (Glean.toQuery x) }

instance Glean.SumQuery Glean.Schema.Query.Hack.Types.Declaration Glean.Schema.Query.Hack.Types.XRefTarget where
  injectQuery q = Data.Default.def
    { Glean.Schema.Query.Hack.Types.xRefTarget_declaration = Prelude.Just q }
