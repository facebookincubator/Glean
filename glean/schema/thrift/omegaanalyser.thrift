// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/thrift/builtin.thrift"

namespace cpp2 facebook.glean.schema.omegaanalyser
namespace hs Glean.Schema
namespace php glean_schema_omegaanalyser
namespace py glean.schema.omegaanalyser
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.omegaanalyser
namespace rust glean_schema_omegaanalyser

hs_include "glean/schema/thrift/omegaanalyser_include.hs"
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
struct OmegaPolicy {
  1: OmegaPolicy_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id OmegaEndpoint_id

@glean.PredicateAnnotation{
  name="omegaanalyser.OmegaEndpoint";
  version=1;
}
struct OmegaEndpoint {
  1: OmegaEndpoint_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id Method_id

@glean.PredicateAnnotation{
  name="omegaanalyser.Method";
  version=1;
}
struct Method {
  1: Method_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id Function__id

@glean.PredicateAnnotation{
  name="omegaanalyser.Function_";
  version=1;
}
struct Function_ {
  1: Function__id id (hs.strict);
  2: optional string key;
}

typedef glean.Id DependencyList_id

@glean.PredicateAnnotation{
  name="omegaanalyser.DependencyList";
  version=1;
}
struct DependencyList {
  1: DependencyList_id id (hs.strict);
  2: optional DependencyList_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Class__id

@glean.PredicateAnnotation{
  name="omegaanalyser.Class_";
  version=1;
}
struct Class_ {
  1: Class__id id (hs.strict);
  2: optional string key;
}

union Node {
  1: Class_ class_ (java.swift.name = "class_");
  2: Method method;
  3: Function_ function_ (java.swift.name = "function_");
} (hs.nonempty)

struct DependencyList_key {
  1: Node node;
  2: list<OmegaEndpoint> endpoints;
}
