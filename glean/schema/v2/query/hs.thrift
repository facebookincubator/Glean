// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/v2/query/builtin.thrift"
include "glean/schema/v2/query/src.thrift"

namespace cpp2 facebook.glean.schema.query.hs
namespace hs Glean.Schema.Query
namespace php glean_schema_query_hs
namespace py glean.schema.query.hs
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.hs
namespace rust glean_schema_query_hs

hs_include "glean/schema/v2/query/hs_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "Class": 1,
  "FunctionName": 1,
  "Type": 1,
  "PackageId": 1,
  "Module": 1,
  "DefinitionName": 1,
  "ModuleDefinitions": 1,
  "FileXRefMap": 2,
  "Definition": 2,
  "Definition_1": 1,
  "ModuleName": 1,
  "ClassInstance": 1,
  "FunctionDefinition": 1,
  "SourceModule": 1,
  "ClassName": 1,
  "XRef": 2,
}


typedef glean.Id XRef_id

@glean.PredicateAnnotation{
  name="hs.XRef";
  version=2;
}
union XRef {
  1: XRef_id id (hs.strict);
  2: XRef_key key;
  3: builtin.Unit get;
} (hs.prefix = "XRef_with_")

typedef glean.Id Type_id

@glean.PredicateAnnotation{
  name="hs.Type";
  version=1;
}
union Type_ {
  1: Type_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "Type__with_")

typedef glean.Id SourceModule_id

@glean.PredicateAnnotation{
  name="hs.SourceModule";
  version=1;
}
union SourceModule {
  1: SourceModule_id id (hs.strict);
  2: SourceModule_key key;
  3: builtin.Unit get;
} (hs.prefix = "SourceModule_with_")

typedef glean.Id PackageId_id

@glean.PredicateAnnotation{
  name="hs.PackageId";
  version=1;
}
union PackageId {
  1: PackageId_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "PackageId_with_")

typedef glean.Id ModuleName_id

@glean.PredicateAnnotation{
  name="hs.ModuleName";
  version=1;
}
union ModuleName {
  1: ModuleName_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "ModuleName_with_")

typedef glean.Id ModuleDefinitions_id

@glean.PredicateAnnotation{
  name="hs.ModuleDefinitions";
  version=1;
}
union ModuleDefinitions {
  1: ModuleDefinitions_id id (hs.strict);
  2: ModuleDefinitions_key key;
  3: builtin.Unit get;
} (hs.prefix = "ModuleDefinitions_with_")

typedef glean.Id Module_id

@glean.PredicateAnnotation{
  name="hs.Module";
  version=1;
}
union Module {
  1: Module_id id (hs.strict);
  2: Module_key key;
  3: builtin.Unit get;
} (hs.prefix = "Module_with_")

typedef glean.Id FunctionName_id

@glean.PredicateAnnotation{
  name="hs.FunctionName";
  version=1;
}
union FunctionName {
  1: FunctionName_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "FunctionName_with_")

typedef glean.Id FunctionDefinition_id

@glean.PredicateAnnotation{
  name="hs.FunctionDefinition";
  version=1;
}
union FunctionDefinition {
  1: FunctionDefinition_id id (hs.strict);
  2: FunctionDefinition_key key;
  3: builtin.Unit get;
} (hs.prefix = "FunctionDefinition_with_")

typedef glean.Id FileXRefMap_id

@glean.PredicateAnnotation{
  name="hs.FileXRefMap";
  version=2;
}
union FileXRefMap {
  1: FileXRefMap_id id (hs.strict);
  2: FileXRefMap_key key;
  3: builtin.Unit get;
} (hs.prefix = "FileXRefMap_with_")

typedef glean.Id DefinitionName_id

@glean.PredicateAnnotation{
  name="hs.DefinitionName";
  version=1;
}
union DefinitionName {
  1: DefinitionName_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "DefinitionName_with_")

typedef glean.Id Definition_id

@glean.PredicateAnnotation{
  name="hs.Definition";
  version=2;
}
union Definition {
  1: Definition_id id (hs.strict);
  2: Definition_key key;
  3: builtin.Unit get;
} (hs.prefix = "Definition_with_")

typedef glean.Id Definition_1_id

@glean.PredicateAnnotation{
  name="hs.Definition";
  version=1;
}
union Definition_1 {
  1: Definition_1_id id (hs.strict);
  2: Definition_1_key key;
  3: builtin.Unit get;
} (hs.prefix = "Definition_1_with_")

typedef glean.Id ClassName_id

@glean.PredicateAnnotation{
  name="hs.ClassName";
  version=1;
}
union ClassName {
  1: ClassName_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "ClassName_with_")

typedef glean.Id ClassInstance_id

@glean.PredicateAnnotation{
  name="hs.ClassInstance";
  version=1;
}
union ClassInstance {
  1: ClassInstance_id id (hs.strict);
  2: ClassInstance_key key;
  3: builtin.Unit get;
} (hs.prefix = "ClassInstance_with_")

typedef glean.Id Class_id

@glean.PredicateAnnotation{
  name="hs.Class";
  version=1;
}
union Class {
  1: Class_id id (hs.strict);
  2: Class_key key;
  3: builtin.Unit get;
} (hs.prefix = "Class_with_")

struct XRefTarget {
  1: optional DefinitionName definition (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ClassName typeclass (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional ModuleName hs_module (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: bool any = false;
}

union XReference_spans_array {
  1: src.ByteSpan every;
  2: list<src.ByteSpan> exact;
}

struct XReference {
  1: optional XRefTarget target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional XReference_spans_array spans (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct XRef_key {
  1: optional src.FileLocation loc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional XRefTarget ref (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct SourceModule_key {
  1: optional ModuleName moduleName (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.File source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union ModuleDefinitions_functionDefinitions_array {
  1: FunctionDefinition every;
  2: list<FunctionDefinition> exact;
}

struct ModuleDefinitions_key {
  1: optional Module module (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ModuleDefinitions_functionDefinitions_array functionDefinitions (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Module_key {
  1: optional PackageId packageId (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ModuleName moduleName (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.File source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FunctionDefinition_key {
  1: optional FunctionName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.Range source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union FileXRefMap_refs_array {
  1: XReference every;
  2: list<XReference> exact;
}

struct FileXRefMap_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional FileXRefMap_refs_array refs (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Definition_key {
  1: optional DefinitionName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.FileLocation source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Definition_1_key {
  1: optional DefinitionName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.Range source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct ClassInstance_key {
  1: optional ClassName typeclass (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_ instance (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.Range source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Class_key {
  1: optional ClassName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.Range source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}
