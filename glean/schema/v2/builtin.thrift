// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"

namespace cpp2 facebook.glean.schema.builtin
namespace hs Glean.Schema
namespace php glean_schema_builtin
namespace py glean.schema.builtin
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.builtin
namespace rust glean_schema_builtin

hs_include "glean/schema/v2/builtin_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
}
// Schema version
const i64 version = 2


struct Unit {}
