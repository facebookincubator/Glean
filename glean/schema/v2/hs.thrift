// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/v2/builtin.thrift"
include "glean/schema/v2/src.thrift"

namespace cpp2 facebook.glean.schema.hs
namespace hs Glean.Schema
namespace php glean_schema_hs
namespace py glean.schema.hs
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.hs
namespace rust glean_schema_hs

hs_include "glean/schema/v2/hs_include.hs"
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
struct XRef {
  1: XRef_id id (hs.strict);
  2: optional XRef_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Type_id

@glean.PredicateAnnotation{
  name="hs.Type";
  version=1;
}
struct Type {
  1: Type_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id SourceModule_id

@glean.PredicateAnnotation{
  name="hs.SourceModule";
  version=1;
}
struct SourceModule {
  1: SourceModule_id id (hs.strict);
  2: optional SourceModule_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id PackageId_id

@glean.PredicateAnnotation{
  name="hs.PackageId";
  version=1;
}
struct PackageId {
  1: PackageId_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id ModuleName_id

@glean.PredicateAnnotation{
  name="hs.ModuleName";
  version=1;
}
struct ModuleName {
  1: ModuleName_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id ModuleDefinitions_id

@glean.PredicateAnnotation{
  name="hs.ModuleDefinitions";
  version=1;
}
struct ModuleDefinitions {
  1: ModuleDefinitions_id id (hs.strict);
  2: optional ModuleDefinitions_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Module_id

@glean.PredicateAnnotation{
  name="hs.Module";
  version=1;
}
struct Module {
  1: Module_id id (hs.strict);
  2: optional Module_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FunctionName_id

@glean.PredicateAnnotation{
  name="hs.FunctionName";
  version=1;
}
struct FunctionName {
  1: FunctionName_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id FunctionDefinition_id

@glean.PredicateAnnotation{
  name="hs.FunctionDefinition";
  version=1;
}
struct FunctionDefinition {
  1: FunctionDefinition_id id (hs.strict);
  2: optional FunctionDefinition_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FileXRefMap_id

@glean.PredicateAnnotation{
  name="hs.FileXRefMap";
  version=2;
}
struct FileXRefMap {
  1: FileXRefMap_id id (hs.strict);
  2: optional FileXRefMap_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DefinitionName_id

@glean.PredicateAnnotation{
  name="hs.DefinitionName";
  version=1;
}
struct DefinitionName {
  1: DefinitionName_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id Definition_id

@glean.PredicateAnnotation{
  name="hs.Definition";
  version=2;
}
struct Definition {
  1: Definition_id id (hs.strict);
  2: optional Definition_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Definition_1_id

@glean.PredicateAnnotation{
  name="hs.Definition";
  version=1;
}
struct Definition_1 {
  1: Definition_1_id id (hs.strict);
  2: optional Definition_1_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ClassName_id

@glean.PredicateAnnotation{
  name="hs.ClassName";
  version=1;
}
struct ClassName {
  1: ClassName_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id ClassInstance_id

@glean.PredicateAnnotation{
  name="hs.ClassInstance";
  version=1;
}
struct ClassInstance {
  1: ClassInstance_id id (hs.strict);
  2: optional ClassInstance_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Class_id

@glean.PredicateAnnotation{
  name="hs.Class";
  version=1;
}
struct Class {
  1: Class_id id (hs.strict);
  2: optional Class_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union XRefTarget {
  1: DefinitionName definition;
  2: ClassName typeclass;
  3: ModuleName hs_module;
} (hs.nonempty)

struct XReference {
  1: XRefTarget target;
  2: list<src.ByteSpan> spans;
}

struct XRef_key {
  1: src.FileLocation loc;
  2: XRefTarget ref;
}

struct SourceModule_key {
  1: ModuleName moduleName;
  2: src.File source;
}

struct ModuleDefinitions_key {
  1: Module module;
  2: list<FunctionDefinition> functionDefinitions;
}

struct Module_key {
  1: PackageId packageId;
  2: ModuleName moduleName;
  3: src.File source;
}

struct FunctionDefinition_key {
  1: FunctionName name;
  2: src.Range source;
}

struct FileXRefMap_key {
  1: src.File file;
  2: list<XReference> refs;
}

struct Definition_key {
  1: DefinitionName name;
  2: src.FileLocation source;
}

struct Definition_1_key {
  1: DefinitionName name;
  2: src.Range source;
}

struct ClassInstance_key {
  1: ClassName typeclass;
  2: Type instance;
  3: src.Range source;
}

struct Class_key {
  1: ClassName name;
  2: src.Range source;
}
