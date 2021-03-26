// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/thrift/query/builtin.thrift"
include "glean/schema/thrift/query/cxx1.thrift"

namespace cpp2 facebook.glean.schema.query.code.cxx
namespace hs Glean.Schema.Query
namespace php glean_schema_query_code_cxx
namespace py glean.schema.query.code_cxx
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.code_cxx
namespace rust glean_schema_query_code_cxx

hs_include "glean/schema/thrift/query/code_cxx_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "DeclToDef": 3,
}


typedef glean.Id DeclToDef_id

@glean.PredicateAnnotation{
  name="code.cxx.DeclToDef";
  version=3;
}
union DeclToDef {
  1: DeclToDef_id id (hs.strict);
  2: DeclToDef_key key;
  3: builtin.Unit get;
} (hs.prefix = "DeclToDef_with_")

struct Definition {
  1: optional cxx1.RecordDefinition record_ (java.swift.name = "record_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional cxx1.FunctionDefinition function_ (java.swift.name = "function_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional cxx1.EnumDefinition enum_ (java.swift.name = "enum_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional cxx1.ObjcMethodDefinition objcMethod (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional cxx1.ObjcContainerDefinition objcContainer (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: optional cxx1.VariableDeclaration variable (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  7: optional cxx1.NamespaceDefinition namespace_ (java.swift.name = "namespace_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  8: bool any = false;
}

struct Entity {
  1: optional cxx1.Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Definition defn (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional cxx1.Enumerator enumerator (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: bool any = false;
}

struct DeclToDef_key {
  1: optional cxx1.Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Definition defn (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}
