// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/thrift/query/builtin.thrift"
include "glean/schema/thrift/query/hack.thrift"

namespace cpp2 facebook.glean.schema.query.search.hack
namespace hs Glean.Schema.Query
namespace php glean_schema_query_search_hack
namespace py glean.schema.query.search_hack
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.search_hack
namespace rust glean_schema_query_search_hack

hs_include "glean/schema/thrift/query/search_hack_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "SearchInContext": 5,
  "SearchInNamespace": 5,
  "SearchByName": 5,
  "SearchInContainer": 5,
  "SearchInEnum": 5,
  "SearchInContainerOrEnum": 5,
}


typedef glean.Id SearchInNamespace_id

@glean.PredicateAnnotation{
  name="search.hack.SearchInNamespace";
  version=5;
}
union SearchInNamespace {
  1: SearchInNamespace_id id (hs.strict);
  2: SearchInNamespace_key key;
  3: builtin.Unit get;
} (hs.prefix = "SearchInNamespace_with_")

typedef glean.Id SearchInEnum_id

@glean.PredicateAnnotation{
  name="search.hack.SearchInEnum";
  version=5;
}
union SearchInEnum {
  1: SearchInEnum_id id (hs.strict);
  2: SearchInEnum_key key;
  3: builtin.Unit get;
} (hs.prefix = "SearchInEnum_with_")

typedef glean.Id SearchInContext_id

@glean.PredicateAnnotation{
  name="search.hack.SearchInContext";
  version=5;
}
union SearchInContext {
  1: SearchInContext_id id (hs.strict);
  2: SearchInContext_key key;
  3: builtin.Unit get;
} (hs.prefix = "SearchInContext_with_")

typedef glean.Id SearchInContainerOrEnum_id

@glean.PredicateAnnotation{
  name="search.hack.SearchInContainerOrEnum";
  version=5;
}
union SearchInContainerOrEnum {
  1: SearchInContainerOrEnum_id id (hs.strict);
  2: SearchInContainerOrEnum_key key;
  3: builtin.Unit get;
} (hs.prefix = "SearchInContainerOrEnum_with_")

typedef glean.Id SearchInContainer_id

@glean.PredicateAnnotation{
  name="search.hack.SearchInContainer";
  version=5;
}
union SearchInContainer {
  1: SearchInContainer_id id (hs.strict);
  2: SearchInContainer_key key;
  3: builtin.Unit get;
} (hs.prefix = "SearchInContainer_with_")

typedef glean.Id SearchByName_id

@glean.PredicateAnnotation{
  name="search.hack.SearchByName";
  version=5;
}
union SearchByName {
  1: SearchByName_id id (hs.strict);
  2: SearchByName_key key;
  3: builtin.Unit get;
} (hs.prefix = "SearchByName_with_")

struct SearchInNamespace_namespace_ {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional hack.NamespaceQName just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct SearchInNamespace_key {
  1: optional hack.Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional SearchInNamespace_namespace_ namespace_ (java.swift.name = "namespace_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional hack.Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct SearchInEnum_enumNamespace {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional hack.NamespaceQName just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct SearchInEnum_key {
  1: optional hack.Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional hack.Name enumName (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional SearchInEnum_enumNamespace enumNamespace (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional hack.Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct SearchInContext_contextNamespace {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional hack.NamespaceQName just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct SearchInContext_key {
  1: optional hack.Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional hack.Name contextName (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional SearchInContext_contextNamespace contextNamespace (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional hack.Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct SearchInContainerOrEnum_contextNamespace {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional hack.NamespaceQName just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct SearchInContainerOrEnum_key {
  1: optional hack.Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional hack.Name contextName (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional SearchInContainerOrEnum_contextNamespace contextNamespace (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional hack.Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct SearchInContainer_containerNamespace {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional hack.NamespaceQName just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct SearchInContainer_key {
  1: optional hack.Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional hack.Name containerName (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional SearchInContainer_containerNamespace containerNamespace (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional hack.Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct SearchByName_key {
  1: optional hack.Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional hack.Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}
