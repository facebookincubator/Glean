// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/query/builtin.thrift"

namespace cpp2 facebook.glean.schema.query.sys
namespace hs Glean.Schema.Query
namespace php glean_schema_query_sys
namespace py glean.schema.query.sys
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.sys
namespace rust glean_schema_query_sys

hs_include "glean/schema/query/sys_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "Blob": 1,
}


typedef glean.Id Blob_id

@glean.PredicateAnnotation{
  name="sys.Blob";
  version=1;
}
union Blob {
  1: Blob_id id (hs.strict);
  2: binary key;
  3: builtin.Unit get;
} (hs.prefix = "Blob_with_")
