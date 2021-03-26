// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/thrift/query/builtin.thrift"
include "glean/schema/thrift/query/hs.thrift"

namespace cpp2 facebook.glean.schema.query.code.hs
namespace hs Glean.Schema.Query
namespace php glean_schema_query_code_hs
namespace py glean.schema.query.code_hs
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.code_hs
namespace rust glean_schema_query_code_hs

hs_include "glean/schema/thrift/query/code_hs_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
}


struct Entity {
  1: optional hs.FunctionDefinition function_ (java.swift.name = "function_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional hs.Class class_ (java.swift.name = "class_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}
