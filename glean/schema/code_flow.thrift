// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/builtin.thrift"
include "glean/schema/flow.thrift"

namespace cpp2 facebook.glean.schema.code.flow
namespace hs Glean.Schema
namespace php glean_schema_code_flow
namespace py glean.schema.code_flow
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.code_flow
namespace rust glean_schema_code_flow

hs_include "glean/schema/code_flow_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
}


union Entity {
  1: flow.SomeDeclaration decl;
} (hs.nonempty)
