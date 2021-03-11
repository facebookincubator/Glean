// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/builtin.thrift"
include "glean/schema/python.thrift"

namespace cpp2 facebook.glean.schema.code.python
namespace hs Glean.Schema
namespace php glean_schema_code_python
namespace py glean.schema.code_python
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.code_python
namespace rust glean_schema_code_python

hs_include "glean/schema/code_python_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
}


union Entity {
  1: python.Declaration decl;
} (hs.nonempty)
