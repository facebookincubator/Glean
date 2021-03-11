// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/builtin.thrift"
include "glean/schema/java.thrift"

namespace cpp2 facebook.glean.schema.code.java
namespace hs Glean.Schema
namespace php glean_schema_code_java
namespace py glean.schema.code_java
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.code_java
namespace rust glean_schema_code_java

hs_include "glean/schema/code_java_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
}


union Entity {
  1: java.ClassDeclaration class_ (java.swift.name = "class_");
} (hs.nonempty)

union Entity_2 {
  1: java.ClassDeclaration_3 class_ (java.swift.name = "class_");
} (hs.nonempty)
