// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/thrift/query/builtin.thrift"
include "glean/schema/thrift/query/java.thrift"

namespace cpp2 facebook.glean.schema.query.code.java
namespace hs Glean.Schema.Query
namespace php glean_schema_query_code_java
namespace py glean.schema.query.code_java
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.code_java
namespace rust glean_schema_query_code_java

hs_include "glean/schema/thrift/query/code_java_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
}


struct Entity {
  1: optional java.ClassDeclaration class_ (java.swift.name = "class_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: bool any = false;
}
