// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/query/builtin.thrift"
include "glean/schema/query/cxx1.thrift"
include "glean/schema/query/docmarkup.thrift"

namespace cpp2 facebook.glean.schema.query.lionhead.lionizer
namespace hs Glean.Schema.Query
namespace php glean_schema_query_lionhead_lionizer
namespace py glean.schema.query.lionhead_lionizer
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.lionhead_lionizer
namespace rust glean_schema_query_lionhead_lionizer

hs_include "glean/schema/query/lionhead_lionizer_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "FindFunctionWithDef": 1,
  "FindFunction": 1,
}


typedef glean.Id FindFunctionWithDef_id

@glean.PredicateAnnotation{
  name="lionhead.lionizer.FindFunctionWithDef";
  version=1;
}
union FindFunctionWithDef {
  1: FindFunctionWithDef_id id (hs.strict);
  2: FindFunctionWithDef_key key;
  3: builtin.Unit get;
} (hs.prefix = "FindFunctionWithDef_with_")

typedef glean.Id FindFunction_id

@glean.PredicateAnnotation{
  name="lionhead.lionizer.FindFunction";
  version=1;
}
union FindFunction {
  1: FindFunction_id id (hs.strict);
  2: FindFunction_key key;
  3: builtin.Unit get;
} (hs.prefix = "FindFunction_with_")

struct FindFunctionWithDef_key {
  1: optional docmarkup.DocAttrKey key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional docmarkup.DocAttrValue value (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional cxx1.FunctionDeclaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional cxx1.FunctionDefinition definition (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FindFunction_key {
  1: optional docmarkup.DocAttrKey key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional docmarkup.DocAttrValue value (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional cxx1.FunctionDeclaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}
