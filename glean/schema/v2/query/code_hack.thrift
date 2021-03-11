// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/v2/query/builtin.thrift"
include "glean/schema/v2/query/hack.thrift"

namespace cpp2 facebook.glean.schema.query.code.hack
namespace hs Glean.Schema.Query
namespace php glean_schema_query_code_hack
namespace py glean.schema.query.code_hack
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.code_hack
namespace rust glean_schema_query_code_hack

hs_include "glean/schema/v2/query/code_hack_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
}


struct Entity {
  1: optional hack.Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: bool any = false;
}
