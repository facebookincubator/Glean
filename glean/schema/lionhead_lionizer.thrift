// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/builtin.thrift"
include "glean/schema/cxx1.thrift"
include "glean/schema/docmarkup.thrift"

namespace cpp2 facebook.glean.schema.lionhead.lionizer
namespace hs Glean.Schema
namespace php glean_schema_lionhead_lionizer
namespace py glean.schema.lionhead_lionizer
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.lionhead_lionizer
namespace rust glean_schema_lionhead_lionizer

hs_include "glean/schema/lionhead_lionizer_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "FindFunctionWithDef": 1,
  "FindFunction": 1,
}


typedef glean.Id FindFunctionWithDef_id

@glean.PredicateAnnotation{
  name="lionhead.lionizer.FindFunctionWithDef";
  version=1;
}
struct FindFunctionWithDef {
  1: FindFunctionWithDef_id id (hs.strict);
  2: optional FindFunctionWithDef_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FindFunction_id

@glean.PredicateAnnotation{
  name="lionhead.lionizer.FindFunction";
  version=1;
}
struct FindFunction {
  1: FindFunction_id id (hs.strict);
  2: optional FindFunction_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FindFunctionWithDef_key {
  1: docmarkup.DocAttrKey key;
  2: docmarkup.DocAttrValue value;
  3: cxx1.FunctionDeclaration declaration;
  4: cxx1.FunctionDefinition definition;
}

struct FindFunction_key {
  1: docmarkup.DocAttrKey key;
  2: docmarkup.DocAttrValue value;
  3: cxx1.FunctionDeclaration declaration;
}
