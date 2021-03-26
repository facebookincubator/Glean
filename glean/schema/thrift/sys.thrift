// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/thrift/builtin.thrift"

namespace cpp2 facebook.glean.schema.sys
namespace hs Glean.Schema
namespace php glean_schema_sys
namespace py glean.schema.sys
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.sys
namespace rust glean_schema_sys

hs_include "glean/schema/thrift/sys_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "Blob": 1,
}


typedef glean.Id Blob_id

@glean.PredicateAnnotation{
  name="sys.Blob";
  version=1;
}
struct Blob {
  1: Blob_id id (hs.strict);
  2: optional binary key;
}
