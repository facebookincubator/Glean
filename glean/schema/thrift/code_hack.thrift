// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/thrift/builtin.thrift"
include "glean/schema/thrift/hack.thrift"

namespace cpp2 facebook.glean.schema.code.hack
namespace hs Glean.Schema
namespace php glean_schema_code_hack
namespace py glean.schema.code_hack
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.code_hack
namespace rust glean_schema_code_hack

hs_include "glean/schema/thrift/code_hack_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
}


union Entity {
  1: hack.Declaration decl;
} (hs.nonempty)
