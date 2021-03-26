// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/v2/thrift/builtin.thrift"
include "glean/schema/v2/thrift/flow.thrift"

namespace cpp2 facebook.glean.schema.code.flow
namespace hs Glean.Schema
namespace php glean_schema_code_flow
namespace py glean.schema.code_flow
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.code_flow
namespace rust glean_schema_code_flow

hs_include "glean/schema/v2/thrift/code_flow_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
}


union Entity {
  1: flow.SomeDeclaration decl;
  2: flow.Module module_ (java.swift.name = "module_");
} (hs.nonempty)
