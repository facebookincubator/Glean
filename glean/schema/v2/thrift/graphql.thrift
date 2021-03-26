// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/v2/thrift/builtin.thrift"

namespace cpp2 facebook.glean.schema.graphql
namespace hs Glean.Schema
namespace php glean_schema_graphql
namespace py glean.schema.graphql
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.graphql
namespace rust glean_schema_graphql

hs_include "glean/schema/v2/thrift/graphql_include.hs"
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
struct VariableDef {
  1: VariableDef_id id (hs.strict);
  2: optional VariableDef_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Value_id

@glean.PredicateAnnotation{
  name="graphql.Value";
  version=1;
}
struct Value {
  1: Value_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id UnionTypeDef_id

@glean.PredicateAnnotation{
  name="graphql.UnionTypeDef";
  version=1;
}
struct UnionTypeDef {
  1: UnionTypeDef_id id (hs.strict);
  2: optional UnionTypeDef_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ScalarTypeDef_id

@glean.PredicateAnnotation{
  name="graphql.ScalarTypeDef";
  version=1;
}
struct ScalarTypeDef {
  1: ScalarTypeDef_id id (hs.strict);
  2: optional ScalarTypeDef_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Query_id

@glean.PredicateAnnotation{
  name="graphql.Query";
  version=1;
}
struct Query {
  1: Query_id id (hs.strict);
  2: optional Query_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ObjectTypeDef_id

@glean.PredicateAnnotation{
  name="graphql.ObjectTypeDef";
  version=1;
}
struct ObjectTypeDef {
  1: ObjectTypeDef_id id (hs.strict);
  2: optional ObjectTypeDef_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id InterfaceTypeDef_id

@glean.PredicateAnnotation{
  name="graphql.InterfaceTypeDef";
  version=1;
}
struct InterfaceTypeDef {
  1: InterfaceTypeDef_id id (hs.strict);
  2: optional InterfaceTypeDef_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id InputValueDef_id

@glean.PredicateAnnotation{
  name="graphql.InputValueDef";
  version=1;
}
struct InputValueDef {
  1: InputValueDef_id id (hs.strict);
  2: optional InputValueDef_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id InputObjectTypeDef_id

@glean.PredicateAnnotation{
  name="graphql.InputObjectTypeDef";
  version=1;
}
struct InputObjectTypeDef {
  1: InputObjectTypeDef_id id (hs.strict);
  2: optional InputObjectTypeDef_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id InlineFragment_id

@glean.PredicateAnnotation{
  name="graphql.InlineFragment";
  version=1;
}
struct InlineFragment {
  1: InlineFragment_id id (hs.strict);
  2: optional InlineFragment_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Fragment_id

@glean.PredicateAnnotation{
  name="graphql.Fragment";
  version=1;
}
struct Fragment {
  1: Fragment_id id (hs.strict);
  2: optional Fragment_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FieldDef_id

@glean.PredicateAnnotation{
  name="graphql.FieldDef";
  version=1;
}
struct FieldDef {
  1: FieldDef_id id (hs.strict);
  2: optional FieldDef_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Field_id

@glean.PredicateAnnotation{
  name="graphql.Field";
  version=1;
}
struct Field {
  1: Field_id id (hs.strict);
  2: optional Field_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id EnumTypeDef_id

@glean.PredicateAnnotation{
  name="graphql.EnumTypeDef";
  version=1;
}
struct EnumTypeDef {
  1: EnumTypeDef_id id (hs.strict);
  2: optional EnumTypeDef_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DirectiveDef_id

@glean.PredicateAnnotation{
  name="graphql.DirectiveDef";
  version=1;
}
struct DirectiveDef {
  1: DirectiveDef_id id (hs.strict);
  2: optional DirectiveDef_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Directive_id

@glean.PredicateAnnotation{
  name="graphql.Directive";
  version=1;
}
struct Directive {
  1: Directive_id id (hs.strict);
  2: optional Directive_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Argument_id

@glean.PredicateAnnotation{
  name="graphql.Argument";
  version=1;
}
struct Argument {
  1: Argument_id id (hs.strict);
  2: optional Argument_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct VariableDef_key {
  1: Value name;
  2: Value type;
  3: list<Directive> directives;
  4: optional Value defaultValue;
}

struct UnionTypeDef_key {
  1: Value name;
  2: list<Value> types;
  3: list<Directive> directives;
}

struct SelectionSet {
  1: list<Field> fields;
  2: list<InlineFragment> inlineFragments;
  3: list<Value> fragmentSpreads;
}

struct ScalarTypeDef_key {
  1: Value name;
  2: list<Directive> directives;
}

struct Query_key {
  1: Value name;
  2: list<Directive> directives;
  3: list<VariableDef> variableDefs;
  4: SelectionSet selectionSet;
}

struct ObjectTypeDef_key {
  1: Value name;
  2: list<Value> interfaces;
  3: list<FieldDef> fields;
  4: list<Directive> directives;
}

struct InterfaceTypeDef_key {
  1: Value name;
  2: list<FieldDef> fields;
  3: list<Directive> directives;
}

struct InputValueDef_key {
  1: Value name;
  2: Value type;
  3: list<Directive> directives;
  4: optional Value defaultValue;
}

struct InputObjectTypeDef_key {
  1: Value name;
  2: list<InputValueDef> fields;
  3: list<Directive> directives;
}

struct InlineFragment_key {
  1: Value inferredTypeCondition;
  2: list<Directive> directives;
  3: SelectionSet selectionSet;
  4: optional Value typeCondition;
}

struct Fragment_key {
  1: Value name;
  2: Value typeCondition;
  3: list<VariableDef> variableDefs;
  4: list<Directive> directives;
  5: SelectionSet selectionSet;
}

struct FieldDef_key {
  1: Value name;
  2: Value type;
  3: list<InputValueDef> argumentDefs;
  4: list<Directive> directives;
}

struct Field_key {
  1: Value type;
  2: Value name;
  3: list<Directive> directives;
  4: SelectionSet selectionSet;
  5: list<Argument> arguments;
  6: optional Value alias;
}

struct EnumTypeDef_key {
  1: Value name;
  2: list<Value> values;
  3: list<Directive> directives;
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

struct DirectiveDef_key {
  1: Value name;
  2: list<InputValueDef> argumentDefs;
  3: list<DirectiveDefLocation> locations;
}

struct Directive_key {
  1: Value name;
  2: list<Argument> arguments;
}

struct Argument_key {
  1: Value name;
  2: Value value;
}
