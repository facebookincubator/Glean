// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/v2/thrift/query/builtin.thrift"
include "glean/schema/v2/thrift/query/src.thrift"

namespace cpp2 facebook.glean.schema.query.flow
namespace hs Glean.Schema.Query
namespace php glean_schema_query_flow
namespace py glean.schema.query.flow
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.flow
namespace rust glean_schema_query_flow

hs_include "glean/schema/v2/thrift/query/flow_include.hs"
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
union TypeImportDeclaration {
  1: TypeImportDeclaration_id id (hs.strict);
  2: TypeImportDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "TypeImportDeclaration_with_")

typedef glean.Id TypeExport_id

@glean.PredicateAnnotation{
  name="flow.TypeExport";
  version=3;
}
union TypeExport {
  1: TypeExport_id id (hs.strict);
  2: TypeExport_key key;
  3: builtin.Unit get;
} (hs.prefix = "TypeExport_with_")

typedef glean.Id TypeDeclarationReference_id

@glean.PredicateAnnotation{
  name="flow.TypeDeclarationReference";
  version=3;
}
union TypeDeclarationReference {
  1: TypeDeclarationReference_id id (hs.strict);
  2: TypeDeclarationReference_key key;
  3: builtin.Unit get;
} (hs.prefix = "TypeDeclarationReference_with_")

typedef glean.Id TypeDeclarationInfo_id

@glean.PredicateAnnotation{
  name="flow.TypeDeclarationInfo";
  version=3;
}
union TypeDeclarationInfo {
  1: TypeDeclarationInfo_id id (hs.strict);
  2: TypeDeclarationInfo_key key;
  3: builtin.Unit get;
} (hs.prefix = "TypeDeclarationInfo_with_")

typedef glean.Id TypeDeclaration_id

@glean.PredicateAnnotation{
  name="flow.TypeDeclaration";
  version=3;
}
union TypeDeclaration {
  1: TypeDeclaration_id id (hs.strict);
  2: TypeDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "TypeDeclaration_with_")

typedef glean.Id Type_id

@glean.PredicateAnnotation{
  name="flow.Type";
  version=3;
}
union Type_ {
  1: Type_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "Type__with_")

typedef glean.Id StringToFileModule_id

@glean.PredicateAnnotation{
  name="flow.StringToFileModule";
  version=3;
}
union StringToFileModule {
  1: StringToFileModule_id id (hs.strict);
  2: StringToFileModule_key key;
  3: builtin.Unit get;
} (hs.prefix = "StringToFileModule_with_")

typedef glean.Id SourceOfTypeExport_id

@glean.PredicateAnnotation{
  name="flow.SourceOfTypeExport";
  version=3;
}
union SourceOfTypeExport {
  1: SourceOfTypeExport_id id (hs.strict);
  2: SourceOfTypeExport_key key;
  3: builtin.Unit get;
} (hs.prefix = "SourceOfTypeExport_with_")

typedef glean.Id SourceOfExport_id

@glean.PredicateAnnotation{
  name="flow.SourceOfExport";
  version=3;
}
union SourceOfExport {
  1: SourceOfExport_id id (hs.strict);
  2: SourceOfExport_key key;
  3: builtin.Unit get;
} (hs.prefix = "SourceOfExport_with_")

typedef glean.Id Range_id

@glean.PredicateAnnotation{
  name="flow.Range";
  version=3;
}
union Range {
  1: Range_id id (hs.strict);
  2: Range_key key;
  3: builtin.Unit get;
} (hs.prefix = "Range_with_")

typedef glean.Id Name_id

@glean.PredicateAnnotation{
  name="flow.Name";
  version=3;
}
union Name {
  1: Name_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "Name_with_")

typedef glean.Id ModuleTypeExport_id

@glean.PredicateAnnotation{
  name="flow.ModuleTypeExport";
  version=3;
}
union ModuleTypeExport {
  1: ModuleTypeExport_id id (hs.strict);
  2: ModuleTypeExport_key key;
  3: builtin.Unit get;
} (hs.prefix = "ModuleTypeExport_with_")

typedef glean.Id ModuleExport_id

@glean.PredicateAnnotation{
  name="flow.ModuleExport";
  version=3;
}
union ModuleExport {
  1: ModuleExport_id id (hs.strict);
  2: ModuleExport_key key;
  3: builtin.Unit get;
} (hs.prefix = "ModuleExport_with_")

typedef glean.Id Module_id

@glean.PredicateAnnotation{
  name="flow.Module";
  version=3;
}
union Module {
  1: Module_id id (hs.strict);
  2: Module_key key;
  3: builtin.Unit get;
} (hs.prefix = "Module_with_")

typedef glean.Id MemberDeclarationReference_id

@glean.PredicateAnnotation{
  name="flow.MemberDeclarationReference";
  version=3;
}
union MemberDeclarationReference {
  1: MemberDeclarationReference_id id (hs.strict);
  2: MemberDeclarationReference_key key;
  3: builtin.Unit get;
} (hs.prefix = "MemberDeclarationReference_with_")

typedef glean.Id MemberDeclarationInfo_id

@glean.PredicateAnnotation{
  name="flow.MemberDeclarationInfo";
  version=3;
}
union MemberDeclarationInfo {
  1: MemberDeclarationInfo_id id (hs.strict);
  2: MemberDeclarationInfo_key key;
  3: builtin.Unit get;
} (hs.prefix = "MemberDeclarationInfo_with_")

typedef glean.Id MemberDeclaration_id

@glean.PredicateAnnotation{
  name="flow.MemberDeclaration";
  version=3;
}
union MemberDeclaration {
  1: MemberDeclaration_id id (hs.strict);
  2: MemberDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "MemberDeclaration_with_")

typedef glean.Id LocalDeclarationReference_id

@glean.PredicateAnnotation{
  name="flow.LocalDeclarationReference";
  version=3;
}
union LocalDeclarationReference {
  1: LocalDeclarationReference_id id (hs.strict);
  2: LocalDeclarationReference_key key;
  3: builtin.Unit get;
} (hs.prefix = "LocalDeclarationReference_with_")

typedef glean.Id ImportDeclaration_id

@glean.PredicateAnnotation{
  name="flow.ImportDeclaration";
  version=3;
}
union ImportDeclaration {
  1: ImportDeclaration_id id (hs.strict);
  2: ImportDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "ImportDeclaration_with_")

typedef glean.Id FileXRef_id

@glean.PredicateAnnotation{
  name="flow.FileXRef";
  version=3;
}
union FileXRef {
  1: FileXRef_id id (hs.strict);
  2: FileXRef_key key;
  3: builtin.Unit get;
} (hs.prefix = "FileXRef_with_")

typedef glean.Id FileOfStringModule_id

@glean.PredicateAnnotation{
  name="flow.FileOfStringModule";
  version=3;
}
union FileOfStringModule {
  1: FileOfStringModule_id id (hs.strict);
  2: FileOfStringModule_key key;
  3: builtin.Unit get;
} (hs.prefix = "FileOfStringModule_with_")

typedef glean.Id FileDeclaration_id

@glean.PredicateAnnotation{
  name="flow.FileDeclaration";
  version=3;
}
union FileDeclaration {
  1: FileDeclaration_id id (hs.strict);
  2: FileDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "FileDeclaration_with_")

typedef glean.Id Export_id

@glean.PredicateAnnotation{
  name="flow.Export";
  version=3;
}
union Export {
  1: Export_id id (hs.strict);
  2: Export_key key;
  3: builtin.Unit get;
} (hs.prefix = "Export_with_")

typedef glean.Id Documentation_id

@glean.PredicateAnnotation{
  name="flow.Documentation";
  version=3;
}
union Documentation {
  1: Documentation_id id (hs.strict);
  2: Range key;
  3: builtin.Unit get;
} (hs.prefix = "Documentation_with_")

typedef glean.Id DeclarationInfo_id

@glean.PredicateAnnotation{
  name="flow.DeclarationInfo";
  version=3;
}
union DeclarationInfo {
  1: DeclarationInfo_id id (hs.strict);
  2: DeclarationInfo_key key;
  3: builtin.Unit get;
} (hs.prefix = "DeclarationInfo_with_")

typedef glean.Id Declaration_id

@glean.PredicateAnnotation{
  name="flow.Declaration";
  version=3;
}
union Declaration {
  1: Declaration_id id (hs.strict);
  2: Declaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "Declaration_with_")

struct XRef {
  1: optional LocalDeclarationReference localRef (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional MemberDeclarationReference memberRef (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional TypeDeclarationReference typeRef (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: bool any = false;
}

struct TypeImportDeclaration_import_ {
  1: optional ModuleTypeExport type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ModuleExport typeof_ (java.swift.name = "typeof_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional Module moduleTypeof (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: bool any = false;
}

struct TypeImportDeclaration_key {
  1: optional TypeDeclaration typeDeclaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional TypeImportDeclaration_import_ import_ (java.swift.name = "import_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct TypeExport_key {
  1: optional Name named (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Module star (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct TypeDeclarationReference_key {
  1: optional TypeDeclaration typeDeclaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Range loc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct TypeDeclarationInfo_documentation {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Documentation just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct TypeDeclarationInfo_key {
  1: optional TypeDeclaration typeDeclaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_ type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional TypeDeclarationInfo_documentation documentation (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct TypeDeclaration_key {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Range loc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct StringToFileModule_key {
  1: optional string string_ (java.swift.name = "string_");
  2: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct SourceOfTypeExport_source {
  1: optional TypeDeclaration typeDeclaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ModuleTypeExport moduleTypeExport (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional Module moduleNamespace (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: bool any = false;
}

struct SourceOfTypeExport_key {
  1: optional ModuleTypeExport moduleTypeExport (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional SourceOfTypeExport_source source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct SourceOfExport_source {
  1: optional Declaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional MemberDeclaration memberDeclaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional ModuleExport moduleExport (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional Module moduleNamespace (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: bool any = false;
}

struct SourceOfExport_key {
  1: optional ModuleExport moduleExport (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional SourceOfExport_source source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct SomeDeclaration {
  1: optional Declaration localDecl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional MemberDeclaration memberDecl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional TypeDeclaration typeDecl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: bool any = false;
}

struct Range_key {
  1: optional Module module (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.ByteSpan span (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct ModuleTypeExport_key {
  1: optional Module module (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional TypeExport typeExport (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct ModuleExport_key {
  1: optional Module module (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Export export_ (java.swift.name = "export_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Module_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional builtin.Unit builtin (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional string lib;
  4: optional builtin.Unit noSource (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional string string_ (java.swift.name = "string_");
  6: bool any = false;
}

struct MemberDeclarationReference_key {
  1: optional MemberDeclaration memberDeclaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Range loc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct MemberDeclarationInfo_documentation {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Documentation just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct MemberDeclarationInfo_key {
  1: optional MemberDeclaration memberDeclaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_ type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional MemberDeclarationInfo_documentation documentation (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct MemberDeclaration_key {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Range loc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct LocalDeclarationReference_key {
  1: optional Declaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Range loc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct ImportDeclaration_import_ {
  1: optional ModuleExport moduleExport (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Module moduleNamespace (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct ImportDeclaration_key {
  1: optional Declaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ImportDeclaration_import_ import_ (java.swift.name = "import_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FileXRef_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional XRef ref (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FileOfStringModule_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional string string_ (java.swift.name = "string_");
}

struct FileDeclaration_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional SomeDeclaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Export_key {
  1: optional builtin.Unit commonJS (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Name commonJSMember (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional Name named (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional builtin.Unit default_ (java.swift.name = "default_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional Module star (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: bool any = false;
}

struct DeclarationInfo_documentation {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Documentation just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct DeclarationInfo_key {
  1: optional Declaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_ type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional DeclarationInfo_documentation documentation (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Declaration_key {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Range loc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}
