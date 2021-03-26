// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/thrift/query/builtin.thrift"

namespace cpp2 facebook.glean.schema.query.graphql
namespace hs Glean.Schema.Query
namespace php glean_schema_query_graphql
namespace py glean.schema.query.graphql
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.graphql
namespace rust glean_schema_query_graphql

hs_include "glean/schema/thrift/query/graphql_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "VariableDef": 1,
  "ScalarTypeDef": 1,
  "InterfaceTypeDef": 1,
  "InputValueDef": 1,
  "Query": 1,
  "DirectiveDef": 1,
  "Fragment": 1,
  "Argument": 1,
  "InlineFragment": 1,
  "EnumTypeDef": 1,
  "UnionTypeDef": 1,
  "InputObjectTypeDef": 1,
  "Directive": 1,
  "ObjectTypeDef": 1,
  "Field": 1,
  "FieldDef": 1,
  "Value": 1,
}


typedef glean.Id VariableDef_id

@glean.PredicateAnnotation{
  name="graphql.VariableDef";
  version=1;
}
union VariableDef {
  1: VariableDef_id id (hs.strict);
  2: VariableDef_key key;
  3: builtin.Unit get;
} (hs.prefix = "VariableDef_with_")

typedef glean.Id Value_id

@glean.PredicateAnnotation{
  name="graphql.Value";
  version=1;
}
union Value {
  1: Value_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "Value_with_")

typedef glean.Id UnionTypeDef_id

@glean.PredicateAnnotation{
  name="graphql.UnionTypeDef";
  version=1;
}
union UnionTypeDef {
  1: UnionTypeDef_id id (hs.strict);
  2: UnionTypeDef_key key;
  3: builtin.Unit get;
} (hs.prefix = "UnionTypeDef_with_")

typedef glean.Id ScalarTypeDef_id

@glean.PredicateAnnotation{
  name="graphql.ScalarTypeDef";
  version=1;
}
union ScalarTypeDef {
  1: ScalarTypeDef_id id (hs.strict);
  2: ScalarTypeDef_key key;
  3: builtin.Unit get;
} (hs.prefix = "ScalarTypeDef_with_")

typedef glean.Id Query_id

@glean.PredicateAnnotation{
  name="graphql.Query";
  version=1;
}
union Query {
  1: Query_id id (hs.strict);
  2: Query_key key;
  3: builtin.Unit get;
} (hs.prefix = "Query_with_")

typedef glean.Id ObjectTypeDef_id

@glean.PredicateAnnotation{
  name="graphql.ObjectTypeDef";
  version=1;
}
union ObjectTypeDef {
  1: ObjectTypeDef_id id (hs.strict);
  2: ObjectTypeDef_key key;
  3: builtin.Unit get;
} (hs.prefix = "ObjectTypeDef_with_")

typedef glean.Id InterfaceTypeDef_id

@glean.PredicateAnnotation{
  name="graphql.InterfaceTypeDef";
  version=1;
}
union InterfaceTypeDef {
  1: InterfaceTypeDef_id id (hs.strict);
  2: InterfaceTypeDef_key key;
  3: builtin.Unit get;
} (hs.prefix = "InterfaceTypeDef_with_")

typedef glean.Id InputValueDef_id

@glean.PredicateAnnotation{
  name="graphql.InputValueDef";
  version=1;
}
union InputValueDef {
  1: InputValueDef_id id (hs.strict);
  2: InputValueDef_key key;
  3: builtin.Unit get;
} (hs.prefix = "InputValueDef_with_")

typedef glean.Id InputObjectTypeDef_id

@glean.PredicateAnnotation{
  name="graphql.InputObjectTypeDef";
  version=1;
}
union InputObjectTypeDef {
  1: InputObjectTypeDef_id id (hs.strict);
  2: InputObjectTypeDef_key key;
  3: builtin.Unit get;
} (hs.prefix = "InputObjectTypeDef_with_")

typedef glean.Id InlineFragment_id

@glean.PredicateAnnotation{
  name="graphql.InlineFragment";
  version=1;
}
union InlineFragment {
  1: InlineFragment_id id (hs.strict);
  2: InlineFragment_key key;
  3: builtin.Unit get;
} (hs.prefix = "InlineFragment_with_")

typedef glean.Id Fragment_id

@glean.PredicateAnnotation{
  name="graphql.Fragment";
  version=1;
}
union Fragment {
  1: Fragment_id id (hs.strict);
  2: Fragment_key key;
  3: builtin.Unit get;
} (hs.prefix = "Fragment_with_")

typedef glean.Id FieldDef_id

@glean.PredicateAnnotation{
  name="graphql.FieldDef";
  version=1;
}
union FieldDef {
  1: FieldDef_id id (hs.strict);
  2: FieldDef_key key;
  3: builtin.Unit get;
} (hs.prefix = "FieldDef_with_")

typedef glean.Id Field_id

@glean.PredicateAnnotation{
  name="graphql.Field";
  version=1;
}
union Field {
  1: Field_id id (hs.strict);
  2: Field_key key;
  3: builtin.Unit get;
} (hs.prefix = "Field_with_")

typedef glean.Id EnumTypeDef_id

@glean.PredicateAnnotation{
  name="graphql.EnumTypeDef";
  version=1;
}
union EnumTypeDef {
  1: EnumTypeDef_id id (hs.strict);
  2: EnumTypeDef_key key;
  3: builtin.Unit get;
} (hs.prefix = "EnumTypeDef_with_")

typedef glean.Id DirectiveDef_id

@glean.PredicateAnnotation{
  name="graphql.DirectiveDef";
  version=1;
}
union DirectiveDef {
  1: DirectiveDef_id id (hs.strict);
  2: DirectiveDef_key key;
  3: builtin.Unit get;
} (hs.prefix = "DirectiveDef_with_")

typedef glean.Id Directive_id

@glean.PredicateAnnotation{
  name="graphql.Directive";
  version=1;
}
union Directive {
  1: Directive_id id (hs.strict);
  2: Directive_key key;
  3: builtin.Unit get;
} (hs.prefix = "Directive_with_")

typedef glean.Id Argument_id

@glean.PredicateAnnotation{
  name="graphql.Argument";
  version=1;
}
union Argument {
  1: Argument_id id (hs.strict);
  2: Argument_key key;
  3: builtin.Unit get;
} (hs.prefix = "Argument_with_")

union VariableDef_directives_array {
  1: Directive every;
  2: list<Directive> exact;
}

struct VariableDef_defaultValue {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Value just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct VariableDef_key {
  1: optional Value name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Value type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional VariableDef_directives_array directives (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional VariableDef_defaultValue defaultValue (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union UnionTypeDef_types_array {
  1: Value every;
  2: list<Value> exact;
}

union UnionTypeDef_directives_array {
  1: Directive every;
  2: list<Directive> exact;
}

struct UnionTypeDef_key {
  1: optional Value name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional UnionTypeDef_types_array types (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional UnionTypeDef_directives_array directives (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union SelectionSet_fields_array {
  1: Field every;
  2: list<Field> exact;
}

union SelectionSet_inlineFragments_array {
  1: InlineFragment every;
  2: list<InlineFragment> exact;
}

union SelectionSet_fragmentSpreads_array {
  1: Value every;
  2: list<Value> exact;
}

struct SelectionSet {
  1: optional SelectionSet_fields_array fields (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional SelectionSet_inlineFragments_array inlineFragments (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional SelectionSet_fragmentSpreads_array fragmentSpreads (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union ScalarTypeDef_directives_array {
  1: Directive every;
  2: list<Directive> exact;
}

struct ScalarTypeDef_key {
  1: optional Value name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ScalarTypeDef_directives_array directives (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union Query_directives_array {
  1: Directive every;
  2: list<Directive> exact;
}

union Query_variableDefs_array {
  1: VariableDef every;
  2: list<VariableDef> exact;
}

struct Query_key {
  1: optional Value name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Query_directives_array directives (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional Query_variableDefs_array variableDefs (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional SelectionSet selectionSet (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union ObjectTypeDef_interfaces_array {
  1: Value every;
  2: list<Value> exact;
}

union ObjectTypeDef_fields_array {
  1: FieldDef every;
  2: list<FieldDef> exact;
}

union ObjectTypeDef_directives_array {
  1: Directive every;
  2: list<Directive> exact;
}

struct ObjectTypeDef_key {
  1: optional Value name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ObjectTypeDef_interfaces_array interfaces (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional ObjectTypeDef_fields_array fields (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional ObjectTypeDef_directives_array directives (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union InterfaceTypeDef_fields_array {
  1: FieldDef every;
  2: list<FieldDef> exact;
}

union InterfaceTypeDef_directives_array {
  1: Directive every;
  2: list<Directive> exact;
}

struct InterfaceTypeDef_key {
  1: optional Value name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional InterfaceTypeDef_fields_array fields (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional InterfaceTypeDef_directives_array directives (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union InputValueDef_directives_array {
  1: Directive every;
  2: list<Directive> exact;
}

struct InputValueDef_defaultValue {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Value just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct InputValueDef_key {
  1: optional Value name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Value type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional InputValueDef_directives_array directives (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional InputValueDef_defaultValue defaultValue (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union InputObjectTypeDef_fields_array {
  1: InputValueDef every;
  2: list<InputValueDef> exact;
}

union InputObjectTypeDef_directives_array {
  1: Directive every;
  2: list<Directive> exact;
}

struct InputObjectTypeDef_key {
  1: optional Value name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional InputObjectTypeDef_fields_array fields (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional InputObjectTypeDef_directives_array directives (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union InlineFragment_directives_array {
  1: Directive every;
  2: list<Directive> exact;
}

struct InlineFragment_typeCondition {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Value just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct InlineFragment_key {
  1: optional Value inferredTypeCondition (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional InlineFragment_directives_array directives (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional SelectionSet selectionSet (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional InlineFragment_typeCondition typeCondition (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union Fragment_variableDefs_array {
  1: VariableDef every;
  2: list<VariableDef> exact;
}

union Fragment_directives_array {
  1: Directive every;
  2: list<Directive> exact;
}

struct Fragment_key {
  1: optional Value name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Value typeCondition (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional Fragment_variableDefs_array variableDefs (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional Fragment_directives_array directives (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional SelectionSet selectionSet (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union FieldDef_argumentDefs_array {
  1: InputValueDef every;
  2: list<InputValueDef> exact;
}

union FieldDef_directives_array {
  1: Directive every;
  2: list<Directive> exact;
}

struct FieldDef_key {
  1: optional Value name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Value type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional FieldDef_argumentDefs_array argumentDefs (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional FieldDef_directives_array directives (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union Field_directives_array {
  1: Directive every;
  2: list<Directive> exact;
}

union Field_arguments_array {
  1: Argument every;
  2: list<Argument> exact;
}

struct Field_alias {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Value just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct Field_key {
  1: optional Value type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Value name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional Field_directives_array directives (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional SelectionSet selectionSet (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional Field_arguments_array arguments (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: optional Field_alias alias (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union EnumTypeDef_values_array {
  1: Value every;
  2: list<Value> exact;
}

union EnumTypeDef_directives_array {
  1: Directive every;
  2: list<Directive> exact;
}

struct EnumTypeDef_key {
  1: optional Value name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional EnumTypeDef_values_array values (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional EnumTypeDef_directives_array directives (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

enum DirectiveDefLocation {
  QUERY = 0,
  MUTATION = 1,
  SUBSCRIPTION = 2,
  FIELD = 3,
  FRAGMENT_DEFINITION = 4,
  FRAGMENT_SPREAD = 5,
  INLINE_FRAGMENT = 6,
  SCHEMA = 7,
  SCALAR = 8,
  OBJECT = 9,
  FIELD_DEFINITION = 10,
  ARGUMENT_DEFINITION = 11,
  INTERFACE = 12,
  UNION = 13,
  ENUM = 14,
  ENUM_VALUE = 15,
  INPUT_OBJECT = 16,
  INPUT_FIELD_DEFINITION = 17
} (hs.nounknown)

union DirectiveDef_argumentDefs_array {
  1: InputValueDef every;
  2: list<InputValueDef> exact;
}

union DirectiveDef_locations_array {
  1: DirectiveDefLocation every;
  2: list<DirectiveDefLocation> exact;
}

struct DirectiveDef_key {
  1: optional Value name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional DirectiveDef_argumentDefs_array argumentDefs (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional DirectiveDef_locations_array locations;
}

union Directive_arguments_array {
  1: Argument every;
  2: list<Argument> exact;
}

struct Directive_key {
  1: optional Value name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Directive_arguments_array arguments (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Argument_key {
  1: optional Value name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Value value (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}
