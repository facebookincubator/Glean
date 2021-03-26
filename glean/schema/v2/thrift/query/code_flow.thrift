// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/v2/thrift/query/builtin.thrift"
include "glean/schema/v2/thrift/query/flow.thrift"

namespace cpp2 facebook.glean.schema.query.code.flow
namespace hs Glean.Schema.Query
namespace php glean_schema_query_code_flow
namespace py glean.schema.query.code_flow
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.code_flow
namespace rust glean_schema_query_code_flow

hs_include "glean/schema/v2/thrift/query/code_flow_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
}


struct Entity {
  1: optional flow.SomeDeclaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional flow.Module module_ (java.swift.name = "module_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}
