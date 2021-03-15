// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/v2/query/builtin.thrift"
include "glean/schema/v2/query/code_cxx.thrift"
include "glean/schema/v2/query/code_flow.thrift"
include "glean/schema/v2/query/code_hack.thrift"
include "glean/schema/v2/query/code_hs.thrift"
include "glean/schema/v2/query/code_java.thrift"
include "glean/schema/v2/query/code_python.thrift"
include "glean/schema/v2/query/pp1.thrift"

namespace cpp2 facebook.glean.schema.query.code
namespace hs Glean.Schema.Query
namespace php glean_schema_query_code
namespace py glean.schema.query.code
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.code
namespace rust glean_schema_query_code

hs_include "glean/schema/v2/query/code_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
}


struct Entity {
  1: optional code_cxx.Entity cxx (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional pp1.Define pp (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional code_java.Entity java (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional code_hs.Entity hs (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional code_python.Entity python (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: optional code_hack.Entity hack (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  7: optional code_flow.Entity flow (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  8: bool any = false;
}
