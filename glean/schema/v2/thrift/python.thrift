// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/v2/thrift/builtin.thrift"
include "glean/schema/v2/thrift/src.thrift"

namespace cpp2 facebook.glean.schema.python
namespace hs Glean.Schema
namespace php glean_schema_python
namespace py glean.schema.python
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.python
namespace rust glean_schema_python

hs_include "glean/schema/v2/thrift/python_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "ClassDefinition": 2,
  "DeclarationReference": 3,
  "ImportStatementByName": 2,
  "DeclarationToName": 2,
  "DeclarationByName": 2,
  "DocstringContent": 2,
  "VariableBySName": 2,
  "FileDirectXRefs": 2,
  "FunctionDeclaration": 1,
  "DirectXRefsByFile": 2,
  "FunctionDefinition": 2,
  "IsTopLevelDeclaration": 3,
  "DeclarationUses": 2,
  "ImportStatement": 2,
  "ImportStatement_1": 1,
  "DeclarationLocation": 2,
  "DeclarationLocation_1": 1,
  "ModuleBySName": 2,
  "ClassDeclaration": 1,
  "VariableDefinition": 2,
  "FileXRefs": 2,
  "FileXRefs_1": 1,
  "SName": 2,
  "TargetUses": 2,
  "TargetUses_1": 1,
  "ClassBySName": 2,
  "XRefIndirectTarget": 2,
  "XRefIndirectTarget_1": 1,
  "Module": 1,
  "DeclarationBySName": 2,
  "Name": 1,
  "ModuleDefinition": 2,
  "SNameToName": 2,
  "NameToSName": 2,
  "Type": 1,
  "DeclarationWithName": 2,
  "FunctionBySName": 2,
  "ImportStatementByAsSName": 3,
  "ImportStatementByAsSName_2": 2,
  "ContainingTopLevelDeclaration": 3,
  "VariableDeclaration": 1,
  "DeclarationsByFile": 2,
  "XRefsViaNameByFile": 2,
  "ImportStatementByAsName": 3,
  "ImportStatementByAsName_2": 2,
}


typedef glean.Id XRefsViaNameByFile_id

@glean.PredicateAnnotation{
  name="python.XRefsViaNameByFile";
  version=2;
}
struct XRefsViaNameByFile {
  1: XRefsViaNameByFile_id id (hs.strict);
  2: optional XRefsViaNameByFile_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id VariableDefinition_id

@glean.PredicateAnnotation{
  name="python.VariableDefinition";
  version=2;
}
struct VariableDefinition {
  1: VariableDefinition_id id (hs.strict);
  2: optional VariableDefinition_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id VariableDeclaration_id

@glean.PredicateAnnotation{
  name="python.VariableDeclaration";
  version=1;
}
struct VariableDeclaration {
  1: VariableDeclaration_id id (hs.strict);
  2: optional VariableDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id VariableBySName_id

@glean.PredicateAnnotation{
  name="python.VariableBySName";
  version=2;
}
struct VariableBySName {
  1: VariableBySName_id id (hs.strict);
  2: optional SName key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional VariableBySName_value value (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Type_id

@glean.PredicateAnnotation{
  name="python.Type";
  version=1;
}
struct Type {
  1: Type_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id SNameToName_id

@glean.PredicateAnnotation{
  name="python.SNameToName";
  version=2;
}
struct SNameToName {
  1: SNameToName_id id (hs.strict);
  2: optional SName key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional SNameToName_value value (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id SName_id

@glean.PredicateAnnotation{
  name="python.SName";
  version=2;
}
struct SName {
  1: SName_id id (hs.strict);
  2: optional SName_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id NameToSName_id

@glean.PredicateAnnotation{
  name="python.NameToSName";
  version=2;
}
struct NameToSName {
  1: NameToSName_id id (hs.strict);
  2: optional Name key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional NameToSName_value value (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Name_id

@glean.PredicateAnnotation{
  name="python.Name";
  version=1;
}
struct Name {
  1: Name_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id ModuleBySName_id

@glean.PredicateAnnotation{
  name="python.ModuleBySName";
  version=2;
}
struct ModuleBySName {
  1: ModuleBySName_id id (hs.strict);
  2: optional SName key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional ModuleBySName_value value (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Module_id

@glean.PredicateAnnotation{
  name="python.Module";
  version=1;
}
struct Module {
  1: Module_id id (hs.strict);
  2: optional Module_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ImportStatementByName_id

@glean.PredicateAnnotation{
  name="python.ImportStatementByName";
  version=2;
}
struct ImportStatementByName {
  1: ImportStatementByName_id id (hs.strict);
  2: optional ImportStatementByName_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ImportStatementByAsSName_id

@glean.PredicateAnnotation{
  name="python.ImportStatementByAsSName";
  version=3;
}
struct ImportStatementByAsSName {
  1: ImportStatementByAsSName_id id (hs.strict);
  2: optional ImportStatementByAsSName_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ImportStatementByAsSName_2_id

@glean.PredicateAnnotation{
  name="python.ImportStatementByAsSName";
  version=2;
}
struct ImportStatementByAsSName_2 {
  1: ImportStatementByAsSName_2_id id (hs.strict);
  2: optional SName key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional ImportStatementByAsSName_2_value value (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ImportStatementByAsName_id

@glean.PredicateAnnotation{
  name="python.ImportStatementByAsName";
  version=3;
}
struct ImportStatementByAsName {
  1: ImportStatementByAsName_id id (hs.strict);
  2: optional ImportStatementByAsName_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ImportStatementByAsName_2_id

@glean.PredicateAnnotation{
  name="python.ImportStatementByAsName";
  version=2;
}
struct ImportStatementByAsName_2 {
  1: ImportStatementByAsName_2_id id (hs.strict);
  2: optional Name key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional ImportStatementByAsName_2_value value (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ImportStatement_id

@glean.PredicateAnnotation{
  name="python.ImportStatement";
  version=2;
}
struct ImportStatement {
  1: ImportStatement_id id (hs.strict);
  2: optional ImportStatement_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ImportStatement_1_id

@glean.PredicateAnnotation{
  name="python.ImportStatement";
  version=1;
}
struct ImportStatement_1 {
  1: ImportStatement_1_id id (hs.strict);
  2: optional ImportStatement_1_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FunctionDeclaration_id

@glean.PredicateAnnotation{
  name="python.FunctionDeclaration";
  version=1;
}
struct FunctionDeclaration {
  1: FunctionDeclaration_id id (hs.strict);
  2: optional FunctionDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FunctionBySName_id

@glean.PredicateAnnotation{
  name="python.FunctionBySName";
  version=2;
}
struct FunctionBySName {
  1: FunctionBySName_id id (hs.strict);
  2: optional SName key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional FunctionBySName_value value (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DocstringContent_id

@glean.PredicateAnnotation{
  name="python.DocstringContent";
  version=2;
}
struct DocstringContent {
  1: DocstringContent_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id ModuleDefinition_id

@glean.PredicateAnnotation{
  name="python.ModuleDefinition";
  version=2;
}
struct ModuleDefinition {
  1: ModuleDefinition_id id (hs.strict);
  2: optional ModuleDefinition_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FunctionDefinition_id

@glean.PredicateAnnotation{
  name="python.FunctionDefinition";
  version=2;
}
struct FunctionDefinition {
  1: FunctionDefinition_id id (hs.strict);
  2: optional FunctionDefinition_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclarationByName_id

@glean.PredicateAnnotation{
  name="python.DeclarationByName";
  version=2;
}
struct DeclarationByName {
  1: DeclarationByName_id id (hs.strict);
  2: optional Name key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional DeclarationByName_value value (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclarationBySName_id

@glean.PredicateAnnotation{
  name="python.DeclarationBySName";
  version=2;
}
struct DeclarationBySName {
  1: DeclarationBySName_id id (hs.strict);
  2: optional SName key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional DeclarationBySName_value value (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclarationLocation_id

@glean.PredicateAnnotation{
  name="python.DeclarationLocation";
  version=2;
}
struct DeclarationLocation {
  1: DeclarationLocation_id id (hs.strict);
  2: optional DeclarationLocation_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclarationReference_id

@glean.PredicateAnnotation{
  name="python.DeclarationReference";
  version=3;
}
struct DeclarationReference {
  1: DeclarationReference_id id (hs.strict);
  2: optional DeclarationReference_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclarationToName_id

@glean.PredicateAnnotation{
  name="python.DeclarationToName";
  version=2;
}
struct DeclarationToName {
  1: DeclarationToName_id id (hs.strict);
  2: optional Declaration key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional DeclarationToName_value value (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclarationUses_id

@glean.PredicateAnnotation{
  name="python.DeclarationUses";
  version=2;
}
struct DeclarationUses {
  1: DeclarationUses_id id (hs.strict);
  2: optional DeclarationUses_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclarationWithName_id

@glean.PredicateAnnotation{
  name="python.DeclarationWithName";
  version=2;
}
struct DeclarationWithName {
  1: DeclarationWithName_id id (hs.strict);
  2: optional DeclarationWithName_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclarationsByFile_id

@glean.PredicateAnnotation{
  name="python.DeclarationsByFile";
  version=2;
}
struct DeclarationsByFile {
  1: DeclarationsByFile_id id (hs.strict);
  2: optional DeclarationsByFile_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DirectXRefsByFile_id

@glean.PredicateAnnotation{
  name="python.DirectXRefsByFile";
  version=2;
}
struct DirectXRefsByFile {
  1: DirectXRefsByFile_id id (hs.strict);
  2: optional DirectXRefsByFile_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FileDirectXRefs_id

@glean.PredicateAnnotation{
  name="python.FileDirectXRefs";
  version=2;
}
struct FileDirectXRefs {
  1: FileDirectXRefs_id id (hs.strict);
  2: optional FileDirectXRefs_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id IsTopLevelDeclaration_id

@glean.PredicateAnnotation{
  name="python.IsTopLevelDeclaration";
  version=3;
}
struct IsTopLevelDeclaration {
  1: IsTopLevelDeclaration_id id (hs.strict);
  2: optional Declaration key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TargetUses_id

@glean.PredicateAnnotation{
  name="python.TargetUses";
  version=2;
}
struct TargetUses {
  1: TargetUses_id id (hs.strict);
  2: optional TargetUses_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FileXRefs_id

@glean.PredicateAnnotation{
  name="python.FileXRefs";
  version=2;
}
struct FileXRefs {
  1: FileXRefs_id id (hs.strict);
  2: optional FileXRefs_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id XRefIndirectTarget_id

@glean.PredicateAnnotation{
  name="python.XRefIndirectTarget";
  version=2;
}
struct XRefIndirectTarget {
  1: XRefIndirectTarget_id id (hs.strict);
  2: optional XRefIndirectTarget_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclarationLocation_1_id

@glean.PredicateAnnotation{
  name="python.DeclarationLocation";
  version=1;
}
struct DeclarationLocation_1 {
  1: DeclarationLocation_1_id id (hs.strict);
  2: optional DeclarationLocation_1_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TargetUses_1_id

@glean.PredicateAnnotation{
  name="python.TargetUses";
  version=1;
}
struct TargetUses_1 {
  1: TargetUses_1_id id (hs.strict);
  2: optional TargetUses_1_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FileXRefs_1_id

@glean.PredicateAnnotation{
  name="python.FileXRefs";
  version=1;
}
struct FileXRefs_1 {
  1: FileXRefs_1_id id (hs.strict);
  2: optional FileXRefs_1_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id XRefIndirectTarget_1_id

@glean.PredicateAnnotation{
  name="python.XRefIndirectTarget";
  version=1;
}
struct XRefIndirectTarget_1 {
  1: XRefIndirectTarget_1_id id (hs.strict);
  2: optional XRefIndirectTarget_1_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ContainingTopLevelDeclaration_id

@glean.PredicateAnnotation{
  name="python.ContainingTopLevelDeclaration";
  version=3;
}
struct ContainingTopLevelDeclaration {
  1: ContainingTopLevelDeclaration_id id (hs.strict);
  2: optional ContainingTopLevelDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ClassDefinition_id

@glean.PredicateAnnotation{
  name="python.ClassDefinition";
  version=2;
}
struct ClassDefinition {
  1: ClassDefinition_id id (hs.strict);
  2: optional ClassDefinition_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ClassDeclaration_id

@glean.PredicateAnnotation{
  name="python.ClassDeclaration";
  version=1;
}
struct ClassDeclaration {
  1: ClassDeclaration_id id (hs.strict);
  2: optional ClassDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ClassBySName_id

@glean.PredicateAnnotation{
  name="python.ClassBySName";
  version=2;
}
struct ClassBySName {
  1: ClassBySName_id id (hs.strict);
  2: optional SName key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional ClassBySName_value value (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct XRefViaName {
  1: Name target;
  2: src.ByteSpan source;
}

struct XRefsViaNameByFile_key {
  1: src.File file;
  2: list<XRefViaName> xrefs;
}

struct VariableDefinition_key {
  1: VariableDeclaration declaration;
  2: optional Type type;
}

struct VariableDeclaration_key {
  1: Name name;
}

typedef VariableDeclaration VariableBySName_value

typedef Name SNameToName_value

struct SName_key {
  1: Name local_name;
  2: optional SName parent;
}

struct Parameter {
  1: Name name;
  2: optional Type type;
  3: optional string value;
}

typedef SName NameToSName_value

typedef Module ModuleBySName_value

struct Module_key {
  1: Name name;
}

struct ImportStatementByName_key {
  1: Name as_name;
  2: Name from_name;
}

struct ImportStatementByAsSName_key {
  1: SName sname;
  2: ImportStatement import_ (java.swift.name = "import_");
}

typedef ImportStatement ImportStatementByAsSName_2_value

struct ImportStatementByAsName_key {
  1: Name name;
  2: ImportStatement import_ (java.swift.name = "import_");
}

typedef ImportStatement ImportStatementByAsName_2_value

struct ImportStatement_key {
  1: Name from_name;
  2: Name as_name;
}

struct ImportStatement_1_key {
  1: Name name;
  2: optional Name as_name;
  3: src.ByteSpan span;
}

struct FunctionDeclaration_key {
  1: Name name;
}

typedef FunctionDeclaration FunctionBySName_value

struct Docstring {
  1: src.ByteSpan location;
}

struct ModuleDefinition_key {
  1: Module module;
  2: optional Docstring docstring;
}

typedef string Decorator

struct FunctionDefinition_key {
  1: FunctionDeclaration declaration;
  2: bool is_async;
  3: optional Type returns;
  4: list<Parameter> params;
  5: optional list<Parameter> posonly_params;
  6: optional list<Parameter> kwonly_params;
  7: optional Parameter star_arg;
  8: optional Parameter star_kwarg;
  9: optional list<Decorator> decorators;
  10: optional Docstring docstring;
}

union Declaration {
  1: ClassDeclaration cls;
  2: FunctionDeclaration func;
  3: VariableDeclaration variable;
  4: ImportStatement imp;
  5: Module module;
} (hs.nonempty)

typedef Declaration DeclarationByName_value

typedef Declaration DeclarationBySName_value

struct DeclarationLocation_key {
  1: Declaration declaration;
  2: src.File file;
  3: src.ByteSpan span;
}

struct DeclarationReference_key {
  1: Declaration target;
  2: Declaration source;
}

typedef Name DeclarationToName_value

struct DeclarationUses_key {
  1: Declaration declaration;
  2: src.File file;
  3: src.ByteSpan span;
}

struct DeclarationWithName_key {
  1: Name name;
  2: Declaration declaration;
}

struct DeclarationsByFile_key {
  1: src.File file;
  2: src.ByteSpan span;
  3: Declaration declaration;
}

struct DirectXRef {
  1: Declaration target;
  2: src.ByteSpan source;
}

struct DirectXRefsByFile_key {
  1: src.File file;
  2: DirectXRef xref;
}

struct FileDirectXRefs_key {
  1: src.File file;
  2: list<DirectXRef> xrefs;
}

union XRefTarget {
  1: Declaration declaration;
  2: XRefIndirectTarget indirect;
  3: builtin.Unit unknown;
} (hs.nonempty)

struct TargetUses_key {
  1: XRefTarget target;
  2: src.File file;
  3: src.ByteSpans uses;
}

struct XRef {
  1: XRefTarget target;
  2: src.ByteSpan source;
}

struct FileXRefs_key {
  1: src.File file;
  2: list<XRef> xrefs;
}

struct XRefIndirectTarget_key {
  1: ImportStatement import_statement;
  2: XRefTarget target;
}

union Declaration_1 {
  1: ClassDeclaration cls;
  2: FunctionDeclaration func;
  3: VariableDeclaration variable;
} (hs.nonempty)

struct DeclarationLocation_1_key {
  1: Declaration_1 declaration;
  2: src.File file;
  3: src.ByteSpan span;
}

union XRefTarget_1 {
  1: Declaration_1 declaration;
  2: Module module;
  3: XRefIndirectTarget_1 indirect;
  4: builtin.Unit unknown;
} (hs.nonempty)

struct TargetUses_1_key {
  1: XRefTarget_1 target;
  2: src.File file;
  3: src.ByteSpans uses;
}

struct XRef_1 {
  1: XRefTarget_1 target;
  2: src.ByteSpan source;
}

struct FileXRefs_1_key {
  1: src.File file;
  2: list<XRef_1> xrefs;
}

struct XRefIndirectTarget_1_key {
  1: ImportStatement_1 import_statement;
  2: XRefTarget_1 target;
}

struct ContainingTopLevelDeclaration_key {
  1: Declaration declaration;
  2: Declaration container;
}

struct ClassDefinition_key {
  1: ClassDeclaration declaration;
  2: optional list<ClassDeclaration> bases;
  3: optional list<Parameter> keywords;
  4: optional list<Decorator> decorators;
  5: optional Docstring docstring;
}

struct ClassDeclaration_key {
  1: Name name;
  2: optional list<Name> bases;
}

typedef ClassDeclaration ClassBySName_value
