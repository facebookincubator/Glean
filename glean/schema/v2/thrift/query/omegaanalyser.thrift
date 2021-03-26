// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/v2/thrift/query/builtin.thrift"

namespace cpp2 facebook.glean.schema.query.omegaanalyser
namespace hs Glean.Schema.Query
namespace php glean_schema_query_omegaanalyser
namespace py glean.schema.query.omegaanalyser
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.omegaanalyser
namespace rust glean_schema_query_omegaanalyser

hs_include "glean/schema/v2/thrift/query/omegaanalyser_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "OmegaPolicy": 2,
  "Class_": 1,
  "DependencyList": 1,
  "Function_": 1,
  "OmegaEndpoint": 1,
  "Method": 1,
}


typedef glean.Id OmegaPolicy_id

@glean.PredicateAnnotation{
  name="omegaanalyser.OmegaPolicy";
  version=2;
}
union OmegaPolicy {
  1: OmegaPolicy_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "OmegaPolicy_with_")

typedef glean.Id OmegaEndpoint_id

@glean.PredicateAnnotation{
  name="omegaanalyser.OmegaEndpoint";
  version=1;
}
union OmegaEndpoint {
  1: OmegaEndpoint_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "OmegaEndpoint_with_")

typedef glean.Id Method_id

@glean.PredicateAnnotation{
  name="omegaanalyser.Method";
  version=1;
}
union Method {
  1: Method_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "Method_with_")

typedef glean.Id Function__id

@glean.PredicateAnnotation{
  name="omegaanalyser.Function_";
  version=1;
}
union Function_ {
  1: Function__id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "Function__with_")

typedef glean.Id DependencyList_id

@glean.PredicateAnnotation{
  name="omegaanalyser.DependencyList";
  version=1;
}
union DependencyList {
  1: DependencyList_id id (hs.strict);
  2: DependencyList_key key;
  3: builtin.Unit get;
} (hs.prefix = "DependencyList_with_")

typedef glean.Id Class__id

@glean.PredicateAnnotation{
  name="omegaanalyser.Class_";
  version=1;
}
union Class_ {
  1: Class__id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "Class__with_")

struct Node {
  1: optional Class_ class_ (java.swift.name = "class_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Method method (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional Function_ function_ (java.swift.name = "function_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: bool any = false;
}

union DependencyList_endpoints_array {
  1: OmegaEndpoint every;
  2: list<OmegaEndpoint> exact;
}

struct DependencyList_key {
  1: optional Node node (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional DependencyList_endpoints_array endpoints (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}
