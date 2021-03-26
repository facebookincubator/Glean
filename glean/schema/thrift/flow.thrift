// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/thrift/builtin.thrift"
include "glean/schema/thrift/src.thrift"

namespace cpp2 facebook.glean.schema.flow
namespace hs Glean.Schema
namespace php glean_schema_flow
namespace py glean.schema.flow
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.flow
namespace rust glean_schema_flow

hs_include "glean/schema/thrift/flow_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "ImportDeclaration": 3,
  "Documentation": 3,
  "Module": 3,
  "StringToFileModule": 3,
  "FileXRef": 3,
  "Export": 3,
  "FileDeclaration": 3,
  "ModuleTypeExport": 3,
  "LocalDeclarationReference": 3,
  "DeclarationInfo": 3,
  "SourceOfTypeExport": 3,
  "Declaration": 3,
  "TypeImportDeclaration": 3,
  "Range": 3,
  "Name": 3,
  "TypeExport": 3,
  "MemberDeclarationInfo": 3,
  "TypeDeclarationInfo": 3,
  "ModuleExport": 3,
  "TypeDeclarationReference": 3,
  "MemberDeclarationReference": 3,
  "TypeDeclaration": 3,
  "MemberDeclaration": 3,
  "SourceOfExport": 3,
  "Type": 3,
  "FileOfStringModule": 3,
}


typedef glean.Id TypeImportDeclaration_id

@glean.PredicateAnnotation{
  name="flow.TypeImportDeclaration";
  version=3;
}
struct TypeImportDeclaration {
  1: TypeImportDeclaration_id id (hs.strict);
  2: optional TypeImportDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TypeExport_id

@glean.PredicateAnnotation{
  name="flow.TypeExport";
  version=3;
}
struct TypeExport {
  1: TypeExport_id id (hs.strict);
  2: optional TypeExport_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TypeDeclarationReference_id

@glean.PredicateAnnotation{
  name="flow.TypeDeclarationReference";
  version=3;
}
struct TypeDeclarationReference {
  1: TypeDeclarationReference_id id (hs.strict);
  2: optional TypeDeclarationReference_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TypeDeclarationInfo_id

@glean.PredicateAnnotation{
  name="flow.TypeDeclarationInfo";
  version=3;
}
struct TypeDeclarationInfo {
  1: TypeDeclarationInfo_id id (hs.strict);
  2: optional TypeDeclarationInfo_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TypeDeclaration_id

@glean.PredicateAnnotation{
  name="flow.TypeDeclaration";
  version=3;
}
struct TypeDeclaration {
  1: TypeDeclaration_id id (hs.strict);
  2: optional TypeDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Type_id

@glean.PredicateAnnotation{
  name="flow.Type";
  version=3;
}
struct Type {
  1: Type_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id StringToFileModule_id

@glean.PredicateAnnotation{
  name="flow.StringToFileModule";
  version=3;
}
struct StringToFileModule {
  1: StringToFileModule_id id (hs.strict);
  2: optional StringToFileModule_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id SourceOfTypeExport_id

@glean.PredicateAnnotation{
  name="flow.SourceOfTypeExport";
  version=3;
}
struct SourceOfTypeExport {
  1: SourceOfTypeExport_id id (hs.strict);
  2: optional SourceOfTypeExport_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id SourceOfExport_id

@glean.PredicateAnnotation{
  name="flow.SourceOfExport";
  version=3;
}
struct SourceOfExport {
  1: SourceOfExport_id id (hs.strict);
  2: optional SourceOfExport_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Range_id

@glean.PredicateAnnotation{
  name="flow.Range";
  version=3;
}
struct Range {
  1: Range_id id (hs.strict);
  2: optional Range_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Name_id

@glean.PredicateAnnotation{
  name="flow.Name";
  version=3;
}
struct Name {
  1: Name_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id ModuleTypeExport_id

@glean.PredicateAnnotation{
  name="flow.ModuleTypeExport";
  version=3;
}
struct ModuleTypeExport {
  1: ModuleTypeExport_id id (hs.strict);
  2: optional ModuleTypeExport_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ModuleExport_id

@glean.PredicateAnnotation{
  name="flow.ModuleExport";
  version=3;
}
struct ModuleExport {
  1: ModuleExport_id id (hs.strict);
  2: optional ModuleExport_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Module_id

@glean.PredicateAnnotation{
  name="flow.Module";
  version=3;
}
struct Module {
  1: Module_id id (hs.strict);
  2: optional Module_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id MemberDeclarationReference_id

@glean.PredicateAnnotation{
  name="flow.MemberDeclarationReference";
  version=3;
}
struct MemberDeclarationReference {
  1: MemberDeclarationReference_id id (hs.strict);
  2: optional MemberDeclarationReference_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id MemberDeclarationInfo_id

@glean.PredicateAnnotation{
  name="flow.MemberDeclarationInfo";
  version=3;
}
struct MemberDeclarationInfo {
  1: MemberDeclarationInfo_id id (hs.strict);
  2: optional MemberDeclarationInfo_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id MemberDeclaration_id

@glean.PredicateAnnotation{
  name="flow.MemberDeclaration";
  version=3;
}
struct MemberDeclaration {
  1: MemberDeclaration_id id (hs.strict);
  2: optional MemberDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id LocalDeclarationReference_id

@glean.PredicateAnnotation{
  name="flow.LocalDeclarationReference";
  version=3;
}
struct LocalDeclarationReference {
  1: LocalDeclarationReference_id id (hs.strict);
  2: optional LocalDeclarationReference_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ImportDeclaration_id

@glean.PredicateAnnotation{
  name="flow.ImportDeclaration";
  version=3;
}
struct ImportDeclaration {
  1: ImportDeclaration_id id (hs.strict);
  2: optional ImportDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FileXRef_id

@glean.PredicateAnnotation{
  name="flow.FileXRef";
  version=3;
}
struct FileXRef {
  1: FileXRef_id id (hs.strict);
  2: optional FileXRef_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FileOfStringModule_id

@glean.PredicateAnnotation{
  name="flow.FileOfStringModule";
  version=3;
}
struct FileOfStringModule {
  1: FileOfStringModule_id id (hs.strict);
  2: optional FileOfStringModule_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FileDeclaration_id

@glean.PredicateAnnotation{
  name="flow.FileDeclaration";
  version=3;
}
struct FileDeclaration {
  1: FileDeclaration_id id (hs.strict);
  2: optional FileDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Export_id

@glean.PredicateAnnotation{
  name="flow.Export";
  version=3;
}
struct Export {
  1: Export_id id (hs.strict);
  2: optional Export_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Documentation_id

@glean.PredicateAnnotation{
  name="flow.Documentation";
  version=3;
}
struct Documentation {
  1: Documentation_id id (hs.strict);
  2: optional Range key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclarationInfo_id

@glean.PredicateAnnotation{
  name="flow.DeclarationInfo";
  version=3;
}
struct DeclarationInfo {
  1: DeclarationInfo_id id (hs.strict);
  2: optional DeclarationInfo_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Declaration_id

@glean.PredicateAnnotation{
  name="flow.Declaration";
  version=3;
}
struct Declaration {
  1: Declaration_id id (hs.strict);
  2: optional Declaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union XRef {
  1: LocalDeclarationReference localRef;
  2: MemberDeclarationReference memberRef;
  3: TypeDeclarationReference typeRef;
} (hs.nonempty)

union TypeImportDeclaration_import_ {
  1: ModuleTypeExport type (py3.name = "type_");
  2: ModuleExport typeof_ (java.swift.name = "typeof_");
  3: Module moduleTypeof;
} (hs.nonempty)

struct TypeImportDeclaration_key {
  1: TypeDeclaration typeDeclaration;
  2: TypeImportDeclaration_import_ import_ (java.swift.name = "import_");
}

union TypeExport_key {
  1: Name named;
  2: Module star;
} (hs.nonempty)

struct TypeDeclarationReference_key {
  1: TypeDeclaration typeDeclaration;
  2: Range loc;
}

struct TypeDeclarationInfo_key {
  1: TypeDeclaration typeDeclaration;
  2: Type type;
  3: optional Documentation documentation;
}

struct TypeDeclaration_key {
  1: Name name;
  2: Range loc;
}

struct StringToFileModule_key {
  1: string string_ (java.swift.name = "string_");
  2: src.File file;
}

union SourceOfTypeExport_source {
  1: TypeDeclaration typeDeclaration;
  2: ModuleTypeExport moduleTypeExport;
  3: Module moduleNamespace;
} (hs.nonempty)

struct SourceOfTypeExport_key {
  1: ModuleTypeExport moduleTypeExport;
  2: SourceOfTypeExport_source source;
}

union SourceOfExport_source {
  1: Declaration declaration;
  2: MemberDeclaration memberDeclaration;
  3: ModuleExport moduleExport;
  4: Module moduleNamespace;
} (hs.nonempty)

struct SourceOfExport_key {
  1: ModuleExport moduleExport;
  2: SourceOfExport_source source;
}

union SomeDeclaration {
  1: Declaration localDecl;
  2: MemberDeclaration memberDecl;
  3: TypeDeclaration typeDecl;
} (hs.nonempty)

struct Range_key {
  1: Module module;
  2: src.ByteSpan span;
}

struct ModuleTypeExport_key {
  1: Module module;
  2: TypeExport typeExport;
}

struct ModuleExport_key {
  1: Module module;
  2: Export export_ (java.swift.name = "export_");
}

union Module_key {
  1: src.File file;
  2: builtin.Unit builtin;
  3: string lib;
  4: builtin.Unit noSource;
  5: string string_ (java.swift.name = "string_");
} (hs.nonempty)

struct MemberDeclarationReference_key {
  1: MemberDeclaration memberDeclaration;
  2: Range loc;
}

struct MemberDeclarationInfo_key {
  1: MemberDeclaration memberDeclaration;
  2: Type type;
  3: optional Documentation documentation;
}

struct MemberDeclaration_key {
  1: Name name;
  2: Range loc;
}

struct LocalDeclarationReference_key {
  1: Declaration declaration;
  2: Range loc;
}

union ImportDeclaration_import_ {
  1: ModuleExport moduleExport;
  2: Module moduleNamespace;
} (hs.nonempty)

struct ImportDeclaration_key {
  1: Declaration declaration;
  2: ImportDeclaration_import_ import_ (java.swift.name = "import_");
}

struct FileXRef_key {
  1: src.File file;
  2: XRef ref;
}

struct FileOfStringModule_key {
  1: src.File file;
  2: string string_ (java.swift.name = "string_");
}

struct FileDeclaration_key {
  1: src.File file;
  2: SomeDeclaration declaration;
}

union Export_key {
  1: builtin.Unit commonJS;
  2: Name commonJSMember;
  3: Name named;
  4: builtin.Unit default_ (java.swift.name = "default_");
  5: Module star;
} (hs.nonempty)

struct DeclarationInfo_key {
  1: Declaration declaration;
  2: Type type;
  3: optional Documentation documentation;
}

struct Declaration_key {
  1: Name name;
  2: Range loc;
}
