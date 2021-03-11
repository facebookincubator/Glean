// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"

namespace cpp2 facebook.glean.schema.query.builtin
namespace hs Glean.Schema.Query
namespace php glean_schema_query_builtin
namespace py glean.schema.query.builtin
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.builtin
namespace rust glean_schema_query_builtin

hs_include "glean/schema/v2/query/builtin_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
}
// Schema version
const i64 version = 2


struct Unit {}
