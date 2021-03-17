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

import qualified Glean.Schema.Graphql.Types


type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.VariableDef_key = Glean.Schema.Graphql.Types.VariableDef_key
type instance Glean.QueryOf Glean.Schema.Graphql.Types.VariableDef_key = Glean.Schema.Query.Graphql.Types.VariableDef_key

instance Glean.ToQuery Glean.Schema.Graphql.Types.VariableDef_key where
  toQuery (Glean.Schema.Graphql.Types.VariableDef_key x1 x2 x3 x4) = Glean.Schema.Query.Graphql.Types.VariableDef_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.VariableDef_directives_array_exact . Prelude.map Glean.toQuery) x3)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Graphql.Types.variableDef_defaultValue_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Graphql.Types.variableDef_defaultValue_just = Prelude.Just (Glean.toQuery x)})) x4))

instance Glean.PredicateQuery Glean.Schema.Graphql.Types.VariableDef where
  toQueryId = Glean.Schema.Query.Graphql.Types.VariableDef_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Graphql.Types.VariableDef_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.VariableDef = Glean.Schema.Graphql.Types.VariableDef
type instance Glean.QueryOf Glean.Schema.Graphql.Types.VariableDef = Glean.Schema.Query.Graphql.Types.VariableDef

instance Glean.ToQuery Glean.Schema.Graphql.Types.VariableDef

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.ScalarTypeDef_key = Glean.Schema.Graphql.Types.ScalarTypeDef_key
type instance Glean.QueryOf Glean.Schema.Graphql.Types.ScalarTypeDef_key = Glean.Schema.Query.Graphql.Types.ScalarTypeDef_key

instance Glean.ToQuery Glean.Schema.Graphql.Types.ScalarTypeDef_key where
  toQuery (Glean.Schema.Graphql.Types.ScalarTypeDef_key x1 x2) = Glean.Schema.Query.Graphql.Types.ScalarTypeDef_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.ScalarTypeDef_directives_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Graphql.Types.ScalarTypeDef where
  toQueryId = Glean.Schema.Query.Graphql.Types.ScalarTypeDef_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Graphql.Types.ScalarTypeDef_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.ScalarTypeDef = Glean.Schema.Graphql.Types.ScalarTypeDef
type instance Glean.QueryOf Glean.Schema.Graphql.Types.ScalarTypeDef = Glean.Schema.Query.Graphql.Types.ScalarTypeDef

instance Glean.ToQuery Glean.Schema.Graphql.Types.ScalarTypeDef

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.InterfaceTypeDef_key = Glean.Schema.Graphql.Types.InterfaceTypeDef_key
type instance Glean.QueryOf Glean.Schema.Graphql.Types.InterfaceTypeDef_key = Glean.Schema.Query.Graphql.Types.InterfaceTypeDef_key

instance Glean.ToQuery Glean.Schema.Graphql.Types.InterfaceTypeDef_key where
  toQuery (Glean.Schema.Graphql.Types.InterfaceTypeDef_key x1 x2 x3) = Glean.Schema.Query.Graphql.Types.InterfaceTypeDef_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.InterfaceTypeDef_fields_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.InterfaceTypeDef_directives_array_exact . Prelude.map Glean.toQuery) x3))

instance Glean.PredicateQuery Glean.Schema.Graphql.Types.InterfaceTypeDef where
  toQueryId = Glean.Schema.Query.Graphql.Types.InterfaceTypeDef_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Graphql.Types.InterfaceTypeDef_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.InterfaceTypeDef = Glean.Schema.Graphql.Types.InterfaceTypeDef
type instance Glean.QueryOf Glean.Schema.Graphql.Types.InterfaceTypeDef = Glean.Schema.Query.Graphql.Types.InterfaceTypeDef

instance Glean.ToQuery Glean.Schema.Graphql.Types.InterfaceTypeDef

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.InputValueDef_key = Glean.Schema.Graphql.Types.InputValueDef_key
type instance Glean.QueryOf Glean.Schema.Graphql.Types.InputValueDef_key = Glean.Schema.Query.Graphql.Types.InputValueDef_key

instance Glean.ToQuery Glean.Schema.Graphql.Types.InputValueDef_key where
  toQuery (Glean.Schema.Graphql.Types.InputValueDef_key x1 x2 x3 x4) = Glean.Schema.Query.Graphql.Types.InputValueDef_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.InputValueDef_directives_array_exact . Prelude.map Glean.toQuery) x3)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Graphql.Types.inputValueDef_defaultValue_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Graphql.Types.inputValueDef_defaultValue_just = Prelude.Just (Glean.toQuery x)})) x4))

instance Glean.PredicateQuery Glean.Schema.Graphql.Types.InputValueDef where
  toQueryId = Glean.Schema.Query.Graphql.Types.InputValueDef_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Graphql.Types.InputValueDef_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.InputValueDef = Glean.Schema.Graphql.Types.InputValueDef
type instance Glean.QueryOf Glean.Schema.Graphql.Types.InputValueDef = Glean.Schema.Query.Graphql.Types.InputValueDef

instance Glean.ToQuery Glean.Schema.Graphql.Types.InputValueDef

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.Query_key = Glean.Schema.Graphql.Types.Query_key
type instance Glean.QueryOf Glean.Schema.Graphql.Types.Query_key = Glean.Schema.Query.Graphql.Types.Query_key

instance Glean.ToQuery Glean.Schema.Graphql.Types.Query_key where
  toQuery (Glean.Schema.Graphql.Types.Query_key x1 x2 x3 x4) = Glean.Schema.Query.Graphql.Types.Query_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.Query_directives_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.Query_variableDefs_array_exact . Prelude.map Glean.toQuery) x3)) (Prelude.Just (Glean.toQuery x4))

instance Glean.PredicateQuery Glean.Schema.Graphql.Types.Query where
  toQueryId = Glean.Schema.Query.Graphql.Types.Query_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Graphql.Types.Query_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.Query = Glean.Schema.Graphql.Types.Query
type instance Glean.QueryOf Glean.Schema.Graphql.Types.Query = Glean.Schema.Query.Graphql.Types.Query

instance Glean.ToQuery Glean.Schema.Graphql.Types.Query

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.DirectiveDef_key = Glean.Schema.Graphql.Types.DirectiveDef_key
type instance Glean.QueryOf Glean.Schema.Graphql.Types.DirectiveDef_key = Glean.Schema.Query.Graphql.Types.DirectiveDef_key

instance Glean.ToQuery Glean.Schema.Graphql.Types.DirectiveDef_key where
  toQuery (Glean.Schema.Graphql.Types.DirectiveDef_key x1 x2 x3) = Glean.Schema.Query.Graphql.Types.DirectiveDef_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.DirectiveDef_argumentDefs_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.DirectiveDef_locations_array_exact . Prelude.map Glean.toQuery) x3))

instance Glean.PredicateQuery Glean.Schema.Graphql.Types.DirectiveDef where
  toQueryId = Glean.Schema.Query.Graphql.Types.DirectiveDef_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Graphql.Types.DirectiveDef_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.DirectiveDef = Glean.Schema.Graphql.Types.DirectiveDef
type instance Glean.QueryOf Glean.Schema.Graphql.Types.DirectiveDef = Glean.Schema.Query.Graphql.Types.DirectiveDef

instance Glean.ToQuery Glean.Schema.Graphql.Types.DirectiveDef

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.Fragment_key = Glean.Schema.Graphql.Types.Fragment_key
type instance Glean.QueryOf Glean.Schema.Graphql.Types.Fragment_key = Glean.Schema.Query.Graphql.Types.Fragment_key

instance Glean.ToQuery Glean.Schema.Graphql.Types.Fragment_key where
  toQuery (Glean.Schema.Graphql.Types.Fragment_key x1 x2 x3 x4 x5) = Glean.Schema.Query.Graphql.Types.Fragment_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.Fragment_variableDefs_array_exact . Prelude.map Glean.toQuery) x3)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.Fragment_directives_array_exact . Prelude.map Glean.toQuery) x4)) (Prelude.Just (Glean.toQuery x5))

instance Glean.PredicateQuery Glean.Schema.Graphql.Types.Fragment where
  toQueryId = Glean.Schema.Query.Graphql.Types.Fragment_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Graphql.Types.Fragment_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.Fragment = Glean.Schema.Graphql.Types.Fragment
type instance Glean.QueryOf Glean.Schema.Graphql.Types.Fragment = Glean.Schema.Query.Graphql.Types.Fragment

instance Glean.ToQuery Glean.Schema.Graphql.Types.Fragment

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.Argument_key = Glean.Schema.Graphql.Types.Argument_key
type instance Glean.QueryOf Glean.Schema.Graphql.Types.Argument_key = Glean.Schema.Query.Graphql.Types.Argument_key

instance Glean.ToQuery Glean.Schema.Graphql.Types.Argument_key where
  toQuery (Glean.Schema.Graphql.Types.Argument_key x1 x2) = Glean.Schema.Query.Graphql.Types.Argument_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Graphql.Types.Argument where
  toQueryId = Glean.Schema.Query.Graphql.Types.Argument_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Graphql.Types.Argument_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.Argument = Glean.Schema.Graphql.Types.Argument
type instance Glean.QueryOf Glean.Schema.Graphql.Types.Argument = Glean.Schema.Query.Graphql.Types.Argument

instance Glean.ToQuery Glean.Schema.Graphql.Types.Argument

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.InlineFragment_key = Glean.Schema.Graphql.Types.InlineFragment_key
type instance Glean.QueryOf Glean.Schema.Graphql.Types.InlineFragment_key = Glean.Schema.Query.Graphql.Types.InlineFragment_key

instance Glean.ToQuery Glean.Schema.Graphql.Types.InlineFragment_key where
  toQuery (Glean.Schema.Graphql.Types.InlineFragment_key x1 x2 x3 x4) = Glean.Schema.Query.Graphql.Types.InlineFragment_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.InlineFragment_directives_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Graphql.Types.inlineFragment_typeCondition_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Graphql.Types.inlineFragment_typeCondition_just = Prelude.Just (Glean.toQuery x)})) x4))

instance Glean.PredicateQuery Glean.Schema.Graphql.Types.InlineFragment where
  toQueryId = Glean.Schema.Query.Graphql.Types.InlineFragment_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Graphql.Types.InlineFragment_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.InlineFragment = Glean.Schema.Graphql.Types.InlineFragment
type instance Glean.QueryOf Glean.Schema.Graphql.Types.InlineFragment = Glean.Schema.Query.Graphql.Types.InlineFragment

instance Glean.ToQuery Glean.Schema.Graphql.Types.InlineFragment

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.EnumTypeDef_key = Glean.Schema.Graphql.Types.EnumTypeDef_key
type instance Glean.QueryOf Glean.Schema.Graphql.Types.EnumTypeDef_key = Glean.Schema.Query.Graphql.Types.EnumTypeDef_key

instance Glean.ToQuery Glean.Schema.Graphql.Types.EnumTypeDef_key where
  toQuery (Glean.Schema.Graphql.Types.EnumTypeDef_key x1 x2 x3) = Glean.Schema.Query.Graphql.Types.EnumTypeDef_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.EnumTypeDef_values_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.EnumTypeDef_directives_array_exact . Prelude.map Glean.toQuery) x3))

instance Glean.PredicateQuery Glean.Schema.Graphql.Types.EnumTypeDef where
  toQueryId = Glean.Schema.Query.Graphql.Types.EnumTypeDef_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Graphql.Types.EnumTypeDef_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.EnumTypeDef = Glean.Schema.Graphql.Types.EnumTypeDef
type instance Glean.QueryOf Glean.Schema.Graphql.Types.EnumTypeDef = Glean.Schema.Query.Graphql.Types.EnumTypeDef

instance Glean.ToQuery Glean.Schema.Graphql.Types.EnumTypeDef

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.UnionTypeDef_key = Glean.Schema.Graphql.Types.UnionTypeDef_key
type instance Glean.QueryOf Glean.Schema.Graphql.Types.UnionTypeDef_key = Glean.Schema.Query.Graphql.Types.UnionTypeDef_key

instance Glean.ToQuery Glean.Schema.Graphql.Types.UnionTypeDef_key where
  toQuery (Glean.Schema.Graphql.Types.UnionTypeDef_key x1 x2 x3) = Glean.Schema.Query.Graphql.Types.UnionTypeDef_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.UnionTypeDef_types_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.UnionTypeDef_directives_array_exact . Prelude.map Glean.toQuery) x3))

instance Glean.PredicateQuery Glean.Schema.Graphql.Types.UnionTypeDef where
  toQueryId = Glean.Schema.Query.Graphql.Types.UnionTypeDef_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Graphql.Types.UnionTypeDef_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.UnionTypeDef = Glean.Schema.Graphql.Types.UnionTypeDef
type instance Glean.QueryOf Glean.Schema.Graphql.Types.UnionTypeDef = Glean.Schema.Query.Graphql.Types.UnionTypeDef

instance Glean.ToQuery Glean.Schema.Graphql.Types.UnionTypeDef

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.InputObjectTypeDef_key = Glean.Schema.Graphql.Types.InputObjectTypeDef_key
type instance Glean.QueryOf Glean.Schema.Graphql.Types.InputObjectTypeDef_key = Glean.Schema.Query.Graphql.Types.InputObjectTypeDef_key

instance Glean.ToQuery Glean.Schema.Graphql.Types.InputObjectTypeDef_key where
  toQuery (Glean.Schema.Graphql.Types.InputObjectTypeDef_key x1 x2 x3) = Glean.Schema.Query.Graphql.Types.InputObjectTypeDef_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.InputObjectTypeDef_fields_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.InputObjectTypeDef_directives_array_exact . Prelude.map Glean.toQuery) x3))

instance Glean.PredicateQuery Glean.Schema.Graphql.Types.InputObjectTypeDef where
  toQueryId = Glean.Schema.Query.Graphql.Types.InputObjectTypeDef_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Graphql.Types.InputObjectTypeDef_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.InputObjectTypeDef = Glean.Schema.Graphql.Types.InputObjectTypeDef
type instance Glean.QueryOf Glean.Schema.Graphql.Types.InputObjectTypeDef = Glean.Schema.Query.Graphql.Types.InputObjectTypeDef

instance Glean.ToQuery Glean.Schema.Graphql.Types.InputObjectTypeDef

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.Directive_key = Glean.Schema.Graphql.Types.Directive_key
type instance Glean.QueryOf Glean.Schema.Graphql.Types.Directive_key = Glean.Schema.Query.Graphql.Types.Directive_key

instance Glean.ToQuery Glean.Schema.Graphql.Types.Directive_key where
  toQuery (Glean.Schema.Graphql.Types.Directive_key x1 x2) = Glean.Schema.Query.Graphql.Types.Directive_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.Directive_arguments_array_exact . Prelude.map Glean.toQuery) x2))

instance Glean.PredicateQuery Glean.Schema.Graphql.Types.Directive where
  toQueryId = Glean.Schema.Query.Graphql.Types.Directive_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Graphql.Types.Directive_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.Directive = Glean.Schema.Graphql.Types.Directive
type instance Glean.QueryOf Glean.Schema.Graphql.Types.Directive = Glean.Schema.Query.Graphql.Types.Directive

instance Glean.ToQuery Glean.Schema.Graphql.Types.Directive

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.ObjectTypeDef_key = Glean.Schema.Graphql.Types.ObjectTypeDef_key
type instance Glean.QueryOf Glean.Schema.Graphql.Types.ObjectTypeDef_key = Glean.Schema.Query.Graphql.Types.ObjectTypeDef_key

instance Glean.ToQuery Glean.Schema.Graphql.Types.ObjectTypeDef_key where
  toQuery (Glean.Schema.Graphql.Types.ObjectTypeDef_key x1 x2 x3 x4) = Glean.Schema.Query.Graphql.Types.ObjectTypeDef_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.ObjectTypeDef_interfaces_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.ObjectTypeDef_fields_array_exact . Prelude.map Glean.toQuery) x3)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.ObjectTypeDef_directives_array_exact . Prelude.map Glean.toQuery) x4))

instance Glean.PredicateQuery Glean.Schema.Graphql.Types.ObjectTypeDef where
  toQueryId = Glean.Schema.Query.Graphql.Types.ObjectTypeDef_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Graphql.Types.ObjectTypeDef_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.ObjectTypeDef = Glean.Schema.Graphql.Types.ObjectTypeDef
type instance Glean.QueryOf Glean.Schema.Graphql.Types.ObjectTypeDef = Glean.Schema.Query.Graphql.Types.ObjectTypeDef

instance Glean.ToQuery Glean.Schema.Graphql.Types.ObjectTypeDef

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.Field_key = Glean.Schema.Graphql.Types.Field_key
type instance Glean.QueryOf Glean.Schema.Graphql.Types.Field_key = Glean.Schema.Query.Graphql.Types.Field_key

instance Glean.ToQuery Glean.Schema.Graphql.Types.Field_key where
  toQuery (Glean.Schema.Graphql.Types.Field_key x1 x2 x3 x4 x5 x6) = Glean.Schema.Query.Graphql.Types.Field_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.Field_directives_array_exact . Prelude.map Glean.toQuery) x3)) (Prelude.Just (Glean.toQuery x4)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.Field_arguments_array_exact . Prelude.map Glean.toQuery) x5)) (Prelude.Just ((Prelude.maybe (Data.Default.def { Glean.Schema.Query.Graphql.Types.field_alias_nothing = Prelude.Just Data.Default.def}) (\x -> Data.Default.def { Glean.Schema.Query.Graphql.Types.field_alias_just = Prelude.Just (Glean.toQuery x)})) x6))

instance Glean.PredicateQuery Glean.Schema.Graphql.Types.Field where
  toQueryId = Glean.Schema.Query.Graphql.Types.Field_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Graphql.Types.Field_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.Field = Glean.Schema.Graphql.Types.Field
type instance Glean.QueryOf Glean.Schema.Graphql.Types.Field = Glean.Schema.Query.Graphql.Types.Field

instance Glean.ToQuery Glean.Schema.Graphql.Types.Field

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.FieldDef_key = Glean.Schema.Graphql.Types.FieldDef_key
type instance Glean.QueryOf Glean.Schema.Graphql.Types.FieldDef_key = Glean.Schema.Query.Graphql.Types.FieldDef_key

instance Glean.ToQuery Glean.Schema.Graphql.Types.FieldDef_key where
  toQuery (Glean.Schema.Graphql.Types.FieldDef_key x1 x2 x3 x4) = Glean.Schema.Query.Graphql.Types.FieldDef_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.FieldDef_argumentDefs_array_exact . Prelude.map Glean.toQuery) x3)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.FieldDef_directives_array_exact . Prelude.map Glean.toQuery) x4))

instance Glean.PredicateQuery Glean.Schema.Graphql.Types.FieldDef where
  toQueryId = Glean.Schema.Query.Graphql.Types.FieldDef_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Graphql.Types.FieldDef_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.FieldDef = Glean.Schema.Graphql.Types.FieldDef
type instance Glean.QueryOf Glean.Schema.Graphql.Types.FieldDef = Glean.Schema.Query.Graphql.Types.FieldDef

instance Glean.ToQuery Glean.Schema.Graphql.Types.FieldDef

instance Glean.PredicateQuery Glean.Schema.Graphql.Types.Value where
  toQueryId = Glean.Schema.Query.Graphql.Types.Value_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Graphql.Types.Value_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.Value = Glean.Schema.Graphql.Types.Value
type instance Glean.QueryOf Glean.Schema.Graphql.Types.Value = Glean.Schema.Query.Graphql.Types.Value

instance Glean.ToQuery Glean.Schema.Graphql.Types.Value

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.DirectiveDefLocation = Glean.Schema.Graphql.Types.DirectiveDefLocation
type instance Glean.QueryOf Glean.Schema.Graphql.Types.DirectiveDefLocation = Glean.Schema.Query.Graphql.Types.DirectiveDefLocation

instance Glean.ToQuery Glean.Schema.Graphql.Types.DirectiveDefLocation where
  toQuery Glean.Schema.Graphql.Types.DirectiveDefLocation_QUERY = Glean.Schema.Query.Graphql.Types.DirectiveDefLocation_QUERY
  toQuery Glean.Schema.Graphql.Types.DirectiveDefLocation_MUTATION = Glean.Schema.Query.Graphql.Types.DirectiveDefLocation_MUTATION
  toQuery Glean.Schema.Graphql.Types.DirectiveDefLocation_SUBSCRIPTION = Glean.Schema.Query.Graphql.Types.DirectiveDefLocation_SUBSCRIPTION
  toQuery Glean.Schema.Graphql.Types.DirectiveDefLocation_FIELD = Glean.Schema.Query.Graphql.Types.DirectiveDefLocation_FIELD
  toQuery Glean.Schema.Graphql.Types.DirectiveDefLocation_FRAGMENT_DEFINITION = Glean.Schema.Query.Graphql.Types.DirectiveDefLocation_FRAGMENT_DEFINITION
  toQuery Glean.Schema.Graphql.Types.DirectiveDefLocation_FRAGMENT_SPREAD = Glean.Schema.Query.Graphql.Types.DirectiveDefLocation_FRAGMENT_SPREAD
  toQuery Glean.Schema.Graphql.Types.DirectiveDefLocation_INLINE_FRAGMENT = Glean.Schema.Query.Graphql.Types.DirectiveDefLocation_INLINE_FRAGMENT
  toQuery Glean.Schema.Graphql.Types.DirectiveDefLocation_SCHEMA = Glean.Schema.Query.Graphql.Types.DirectiveDefLocation_SCHEMA
  toQuery Glean.Schema.Graphql.Types.DirectiveDefLocation_SCALAR = Glean.Schema.Query.Graphql.Types.DirectiveDefLocation_SCALAR
  toQuery Glean.Schema.Graphql.Types.DirectiveDefLocation_OBJECT = Glean.Schema.Query.Graphql.Types.DirectiveDefLocation_OBJECT
  toQuery Glean.Schema.Graphql.Types.DirectiveDefLocation_FIELD_DEFINITION = Glean.Schema.Query.Graphql.Types.DirectiveDefLocation_FIELD_DEFINITION
  toQuery Glean.Schema.Graphql.Types.DirectiveDefLocation_ARGUMENT_DEFINITION = Glean.Schema.Query.Graphql.Types.DirectiveDefLocation_ARGUMENT_DEFINITION
  toQuery Glean.Schema.Graphql.Types.DirectiveDefLocation_INTERFACE = Glean.Schema.Query.Graphql.Types.DirectiveDefLocation_INTERFACE
  toQuery Glean.Schema.Graphql.Types.DirectiveDefLocation_UNION = Glean.Schema.Query.Graphql.Types.DirectiveDefLocation_UNION
  toQuery Glean.Schema.Graphql.Types.DirectiveDefLocation_ENUM = Glean.Schema.Query.Graphql.Types.DirectiveDefLocation_ENUM
  toQuery Glean.Schema.Graphql.Types.DirectiveDefLocation_ENUM_VALUE = Glean.Schema.Query.Graphql.Types.DirectiveDefLocation_ENUM_VALUE
  toQuery Glean.Schema.Graphql.Types.DirectiveDefLocation_INPUT_OBJECT = Glean.Schema.Query.Graphql.Types.DirectiveDefLocation_INPUT_OBJECT
  toQuery Glean.Schema.Graphql.Types.DirectiveDefLocation_INPUT_FIELD_DEFINITION = Glean.Schema.Query.Graphql.Types.DirectiveDefLocation_INPUT_FIELD_DEFINITION

type instance Glean.QueryResult Glean.Schema.Query.Graphql.Types.SelectionSet = Glean.Schema.Graphql.Types.SelectionSet
type instance Glean.QueryOf Glean.Schema.Graphql.Types.SelectionSet = Glean.Schema.Query.Graphql.Types.SelectionSet

instance Glean.ToQuery Glean.Schema.Graphql.Types.SelectionSet where
  toQuery (Glean.Schema.Graphql.Types.SelectionSet x1 x2 x3) = Glean.Schema.Query.Graphql.Types.SelectionSet (Prelude.Just ((Glean.Schema.Query.Graphql.Types.SelectionSet_fields_array_exact . Prelude.map Glean.toQuery) x1)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.SelectionSet_inlineFragments_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just ((Glean.Schema.Query.Graphql.Types.SelectionSet_fragmentSpreads_array_exact . Prelude.map Glean.toQuery) x3))
