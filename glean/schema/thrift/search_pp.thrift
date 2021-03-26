// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/thrift/builtin.thrift"
include "glean/schema/thrift/pp1.thrift"

namespace cpp2 facebook.glean.schema.search.pp
namespace hs Glean.Schema
namespace php glean_schema_search_pp
namespace py glean.schema.search_pp
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.search_pp
namespace rust glean_schema_search_pp

hs_include "glean/schema/thrift/search_pp_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "SearchByName": 2,
  "SearchByName_1": 1,
}


typedef glean.Id SearchByName_id

@glean.PredicateAnnotation{
  name="search.pp.SearchByName";
  version=2;
}
struct SearchByName {
  1: SearchByName_id id (hs.strict);
  2: optional SearchByName_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id SearchByName_1_id

@glean.PredicateAnnotation{
  name="search.pp.SearchByName";
  version=1;
}
struct SearchByName_1 {
  1: SearchByName_1_id id (hs.strict);
  2: optional SearchByName_1_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct SearchByName_key {
  1: pp1.Macro macro;
  2: pp1.Define entity;
}

struct SearchByName_1_key {
  1: string name;
  2: pp1.Define entity;
}
