// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/builtin.thrift"
include "glean/schema/hs.thrift"

namespace cpp2 facebook.glean.schema.code.hs
namespace hs Glean.Schema
namespace php glean_schema_code_hs
namespace py glean.schema.code_hs
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.code_hs
namespace rust glean_schema_code_hs

hs_include "glean/schema/code_hs_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
}


union Entity {
  1: hs.FunctionDefinition function_ (java.swift.name = "function_");
  2: hs.Class class_ (java.swift.name = "class_");
} (hs.nonempty)
