// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/query/builtin.thrift"
include "glean/schema/query/code_cxx.thrift"
include "glean/schema/query/cxx1.thrift"

namespace cpp2 facebook.glean.schema.query.search.cxx
namespace hs Glean.Schema.Query
namespace php glean_schema_query_search_cxx
namespace py glean.schema.query.search_cxx
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.search_cxx
namespace rust glean_schema_query_search_cxx

hs_include "glean/schema/query/search_cxx_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "EntityUses": 4,
  "SearchByNameAndScope": 4,
  "GlobalDeclarationWithName": 1,
  "DeclIsDefn": 4,
  "SearchBySelector": 4,
}


typedef glean.Id SearchBySelector_id

@glean.PredicateAnnotation{
  name="search.cxx.SearchBySelector";
  version=4;
}
union SearchBySelector {
  1: SearchBySelector_id id (hs.strict);
  2: SearchBySelector_key key;
  3: builtin.Unit get;
} (hs.prefix = "SearchBySelector_with_")

typedef glean.Id SearchByNameAndScope_id

@glean.PredicateAnnotation{
  name="search.cxx.SearchByNameAndScope";
  version=4;
}
union SearchByNameAndScope {
  1: SearchByNameAndScope_id id (hs.strict);
  2: SearchByNameAndScope_key key;
  3: builtin.Unit get;
} (hs.prefix = "SearchByNameAndScope_with_")

typedef glean.Id GlobalDeclarationWithName_id

@glean.PredicateAnnotation{
  name="search.cxx.GlobalDeclarationWithName";
  version=1;
}
union GlobalDeclarationWithName {
  1: GlobalDeclarationWithName_id id (hs.strict);
  2: GlobalDeclarationWithName_key key;
  3: builtin.Unit get;
} (hs.prefix = "GlobalDeclarationWithName_with_")

typedef glean.Id EntityUses_id

@glean.PredicateAnnotation{
  name="search.cxx.EntityUses";
  version=4;
}
union EntityUses {
  1: EntityUses_id id (hs.strict);
  2: EntityUses_key key;
  3: builtin.Unit get;
} (hs.prefix = "EntityUses_with_")

typedef glean.Id DeclIsDefn_id

@glean.PredicateAnnotation{
  name="search.cxx.DeclIsDefn";
  version=4;
}
union DeclIsDefn {
  1: DeclIsDefn_id id (hs.strict);
  2: DeclIsDefn_key key;
  3: builtin.Unit get;
} (hs.prefix = "DeclIsDefn_with_")

struct SearchBySelector_key {
  1: optional cxx1.ObjcSelector selector (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional code_cxx.Entity entity (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct SearchByNameAndScope_key {
  1: optional cxx1.Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional cxx1.Scope scope (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional code_cxx.Entity entity (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct GlobalDeclarationWithName_key {
  1: optional cxx1.Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional cxx1.Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct EntityUses_key {
  1: optional code_cxx.Entity entity (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional cxx1.TargetUses uses (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct DeclIsDefn_key {
  1: optional cxx1.Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional code_cxx.Definition defn (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}
