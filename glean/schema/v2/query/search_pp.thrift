// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/v2/query/builtin.thrift"
include "glean/schema/v2/query/pp1.thrift"

namespace cpp2 facebook.glean.schema.query.search.pp
namespace hs Glean.Schema.Query
namespace php glean_schema_query_search_pp
namespace py glean.schema.query.search_pp
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.search_pp
namespace rust glean_schema_query_search_pp

hs_include "glean/schema/v2/query/search_pp_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "SearchByName": 2,
  "SearchByName_1": 1,
}


typedef glean.Id SearchByName_id

@glean.PredicateAnnotation{
  name="search.pp.SearchByName";
  version=2;
}
union SearchByName {
  1: SearchByName_id id (hs.strict);
  2: SearchByName_key key;
  3: builtin.Unit get;
} (hs.prefix = "SearchByName_with_")

typedef glean.Id SearchByName_1_id

@glean.PredicateAnnotation{
  name="search.pp.SearchByName";
  version=1;
}
union SearchByName_1 {
  1: SearchByName_1_id id (hs.strict);
  2: SearchByName_1_key key;
  3: builtin.Unit get;
} (hs.prefix = "SearchByName_1_with_")

struct SearchByName_key {
  1: optional pp1.Macro macro (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional pp1.Define entity (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct SearchByName_1_key {
  1: optional string name;
  2: optional pp1.Define entity (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}
