// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/builtin.thrift"
include "glean/schema/hack.thrift"

namespace cpp2 facebook.glean.schema.search.hack
namespace hs Glean.Schema
namespace php glean_schema_search_hack
namespace py glean.schema.search_hack
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.search_hack
namespace rust glean_schema_search_hack

hs_include "glean/schema/search_hack_include.hs"
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
struct SearchInNamespace {
  1: SearchInNamespace_id id (hs.strict);
  2: optional SearchInNamespace_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id SearchInEnum_id

@glean.PredicateAnnotation{
  name="search.hack.SearchInEnum";
  version=5;
}
struct SearchInEnum {
  1: SearchInEnum_id id (hs.strict);
  2: optional SearchInEnum_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id SearchInContext_id

@glean.PredicateAnnotation{
  name="search.hack.SearchInContext";
  version=5;
}
struct SearchInContext {
  1: SearchInContext_id id (hs.strict);
  2: optional SearchInContext_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id SearchInContainerOrEnum_id

@glean.PredicateAnnotation{
  name="search.hack.SearchInContainerOrEnum";
  version=5;
}
struct SearchInContainerOrEnum {
  1: SearchInContainerOrEnum_id id (hs.strict);
  2: optional SearchInContainerOrEnum_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id SearchInContainer_id

@glean.PredicateAnnotation{
  name="search.hack.SearchInContainer";
  version=5;
}
struct SearchInContainer {
  1: SearchInContainer_id id (hs.strict);
  2: optional SearchInContainer_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id SearchByName_id

@glean.PredicateAnnotation{
  name="search.hack.SearchByName";
  version=5;
}
struct SearchByName {
  1: SearchByName_id id (hs.strict);
  2: optional SearchByName_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct SearchInNamespace_key {
  1: hack.Name name;
  2: optional hack.NamespaceQName namespace_ (java.swift.name = "namespace_");
  3: hack.Declaration decl;
}

struct SearchInEnum_key {
  1: hack.Name name;
  2: hack.Name enumName;
  3: optional hack.NamespaceQName enumNamespace;
  4: hack.Declaration decl;
}

struct SearchInContext_key {
  1: hack.Name name;
  2: hack.Name contextName;
  3: optional hack.NamespaceQName contextNamespace;
  4: hack.Declaration decl;
}

struct SearchInContainerOrEnum_key {
  1: hack.Name name;
  2: hack.Name contextName;
  3: optional hack.NamespaceQName contextNamespace;
  4: hack.Declaration decl;
}

struct SearchInContainer_key {
  1: hack.Name name;
  2: hack.Name containerName;
  3: optional hack.NamespaceQName containerNamespace;
  4: hack.Declaration decl;
}

struct SearchByName_key {
  1: hack.Name name;
  2: hack.Declaration decl;
}
