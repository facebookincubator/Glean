// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/thrift/builtin.thrift"
include "glean/schema/thrift/code_cxx.thrift"
include "glean/schema/thrift/cxx1.thrift"

namespace cpp2 facebook.glean.schema.search.cxx
namespace hs Glean.Schema
namespace php glean_schema_search_cxx
namespace py glean.schema.search_cxx
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.search_cxx
namespace rust glean_schema_search_cxx

hs_include "glean/schema/thrift/search_cxx_include.hs"
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
struct SearchBySelector {
  1: SearchBySelector_id id (hs.strict);
  2: optional SearchBySelector_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id SearchByNameAndScope_id

@glean.PredicateAnnotation{
  name="search.cxx.SearchByNameAndScope";
  version=4;
}
struct SearchByNameAndScope {
  1: SearchByNameAndScope_id id (hs.strict);
  2: optional SearchByNameAndScope_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id GlobalDeclarationWithName_id

@glean.PredicateAnnotation{
  name="search.cxx.GlobalDeclarationWithName";
  version=1;
}
struct GlobalDeclarationWithName {
  1: GlobalDeclarationWithName_id id (hs.strict);
  2: optional GlobalDeclarationWithName_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id EntityUses_id

@glean.PredicateAnnotation{
  name="search.cxx.EntityUses";
  version=4;
}
struct EntityUses {
  1: EntityUses_id id (hs.strict);
  2: optional EntityUses_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclIsDefn_id

@glean.PredicateAnnotation{
  name="search.cxx.DeclIsDefn";
  version=4;
}
struct DeclIsDefn {
  1: DeclIsDefn_id id (hs.strict);
  2: optional DeclIsDefn_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct SearchBySelector_key {
  1: cxx1.ObjcSelector selector;
  2: code_cxx.Entity entity;
}

struct SearchByNameAndScope_key {
  1: cxx1.Name name;
  2: cxx1.Scope scope;
  3: code_cxx.Entity entity;
}

struct GlobalDeclarationWithName_key {
  1: cxx1.Name name;
  2: cxx1.Declaration decl;
}

struct EntityUses_key {
  1: code_cxx.Entity entity;
  2: cxx1.TargetUses uses;
}

struct DeclIsDefn_key {
  1: cxx1.Declaration decl;
  2: code_cxx.Definition defn;
}
