// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/builtin.thrift"
include "glean/schema/cxx1.thrift"

namespace cpp2 facebook.glean.schema.code.cxx
namespace hs Glean.Schema
namespace php glean_schema_code_cxx
namespace py glean.schema.code_cxx
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.code_cxx
namespace rust glean_schema_code_cxx

hs_include "glean/schema/code_cxx_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "DeclToDef": 3,
}


typedef glean.Id DeclToDef_id

@glean.PredicateAnnotation{
  name="code.cxx.DeclToDef";
  version=3;
}
struct DeclToDef {
  1: DeclToDef_id id (hs.strict);
  2: optional DeclToDef_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union Definition {
  1: cxx1.RecordDefinition record_ (java.swift.name = "record_");
  2: cxx1.FunctionDefinition function_ (java.swift.name = "function_");
  3: cxx1.EnumDefinition enum_ (java.swift.name = "enum_");
  4: cxx1.ObjcMethodDefinition objcMethod;
  5: cxx1.ObjcContainerDefinition objcContainer;
  6: cxx1.VariableDeclaration variable;
  7: cxx1.NamespaceDefinition namespace_ (java.swift.name = "namespace_");
} (hs.nonempty)

union Entity {
  1: cxx1.Declaration decl;
  2: Definition defn;
  3: cxx1.Enumerator enumerator;
} (hs.nonempty)

struct DeclToDef_key {
  1: cxx1.Declaration decl;
  2: Definition defn;
}
