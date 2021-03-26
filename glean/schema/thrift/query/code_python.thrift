// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/thrift/query/builtin.thrift"
include "glean/schema/thrift/query/python.thrift"

namespace cpp2 facebook.glean.schema.query.code.python
namespace hs Glean.Schema.Query
namespace php glean_schema_query_code_python
namespace py glean.schema.query.code_python
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.code_python
namespace rust glean_schema_query_code_python

hs_include "glean/schema/thrift/query/code_python_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
}


struct Entity {
  1: optional python.Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: bool any = false;
}
