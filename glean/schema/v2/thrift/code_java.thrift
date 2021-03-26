// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/v2/thrift/builtin.thrift"
include "glean/schema/v2/thrift/java.thrift"

namespace cpp2 facebook.glean.schema.code.java
namespace hs Glean.Schema
namespace php glean_schema_code_java
namespace py glean.schema.code_java
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.code_java
namespace rust glean_schema_code_java

hs_include "glean/schema/v2/thrift/code_java_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
}


union Entity {
  1: java.ClassDeclaration class_ (java.swift.name = "class_");
} (hs.nonempty)
