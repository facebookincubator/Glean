// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/thrift/query/builtin.thrift"
include "glean/schema/thrift/query/src.thrift"

namespace cpp2 facebook.glean.schema.query.python
namespace hs Glean.Schema.Query
namespace php glean_schema_query_python
namespace py glean.schema.query.python
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.python
namespace rust glean_schema_query_python

hs_include "glean/schema/thrift/query/python_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "ClassDefinition": 2,
  "ImportStatementByName": 2,
  "DeclarationToName": 2,
  "DeclarationByName": 2,
  "DocstringContent": 2,
  "VariableBySName": 2,
  "FileDirectXRefs": 2,
  "FunctionDeclaration": 1,
  "DirectXRefsByFile": 2,
  "FunctionDefinition": 2,
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
union XRefsViaNameByFile {
  1: XRefsViaNameByFile_id id (hs.strict);
  2: XRefsViaNameByFile_key key;
  3: builtin.Unit get;
} (hs.prefix = "XRefsViaNameByFile_with_")

typedef glean.Id VariableDefinition_id

@glean.PredicateAnnotation{
  name="python.VariableDefinition";
  version=2;
}
union VariableDefinition {
  1: VariableDefinition_id id (hs.strict);
  2: VariableDefinition_key key;
  3: builtin.Unit get;
} (hs.prefix = "VariableDefinition_with_")

typedef glean.Id VariableDeclaration_id

@glean.PredicateAnnotation{
  name="python.VariableDeclaration";
  version=1;
}
union VariableDeclaration {
  1: VariableDeclaration_id id (hs.strict);
  2: VariableDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "VariableDeclaration_with_")

typedef glean.Id VariableBySName_id

@glean.PredicateAnnotation{
  name="python.VariableBySName";
  version=2;
}
union VariableBySName {
  1: VariableBySName_id id (hs.strict);
  2: SName key;
  3: builtin.Unit get;
} (hs.prefix = "VariableBySName_with_")

typedef glean.Id Type_id

@glean.PredicateAnnotation{
  name="python.Type";
  version=1;
}
union Type_ {
  1: Type_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "Type__with_")

typedef glean.Id SNameToName_id

@glean.PredicateAnnotation{
  name="python.SNameToName";
  version=2;
}
union SNameToName {
  1: SNameToName_id id (hs.strict);
  2: SName key;
  3: builtin.Unit get;
} (hs.prefix = "SNameToName_with_")

typedef glean.Id SName_id

@glean.PredicateAnnotation{
  name="python.SName";
  version=2;
}
union SName {
  1: SName_id id (hs.strict);
  2: SName_key key;
  3: builtin.Unit get;
} (hs.prefix = "SName_with_")

typedef glean.Id NameToSName_id

@glean.PredicateAnnotation{
  name="python.NameToSName";
  version=2;
}
union NameToSName {
  1: NameToSName_id id (hs.strict);
  2: Name key;
  3: builtin.Unit get;
} (hs.prefix = "NameToSName_with_")

typedef glean.Id Name_id

@glean.PredicateAnnotation{
  name="python.Name";
  version=1;
}
union Name {
  1: Name_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "Name_with_")

typedef glean.Id ModuleBySName_id

@glean.PredicateAnnotation{
  name="python.ModuleBySName";
  version=2;
}
union ModuleBySName {
  1: ModuleBySName_id id (hs.strict);
  2: SName key;
  3: builtin.Unit get;
} (hs.prefix = "ModuleBySName_with_")

typedef glean.Id Module_id

@glean.PredicateAnnotation{
  name="python.Module";
  version=1;
}
union Module {
  1: Module_id id (hs.strict);
  2: Module_key key;
  3: builtin.Unit get;
} (hs.prefix = "Module_with_")

typedef glean.Id ImportStatementByName_id

@glean.PredicateAnnotation{
  name="python.ImportStatementByName";
  version=2;
}
union ImportStatementByName {
  1: ImportStatementByName_id id (hs.strict);
  2: ImportStatementByName_key key;
  3: builtin.Unit get;
} (hs.prefix = "ImportStatementByName_with_")

typedef glean.Id ImportStatementByAsSName_id

@glean.PredicateAnnotation{
  name="python.ImportStatementByAsSName";
  version=3;
}
union ImportStatementByAsSName {
  1: ImportStatementByAsSName_id id (hs.strict);
  2: ImportStatementByAsSName_key key;
  3: builtin.Unit get;
} (hs.prefix = "ImportStatementByAsSName_with_")

typedef glean.Id ImportStatementByAsSName_2_id

@glean.PredicateAnnotation{
  name="python.ImportStatementByAsSName";
  version=2;
}
union ImportStatementByAsSName_2 {
  1: ImportStatementByAsSName_2_id id (hs.strict);
  2: SName key;
  3: builtin.Unit get;
} (hs.prefix = "ImportStatementByAsSName_2_with_")

typedef glean.Id ImportStatementByAsName_id

@glean.PredicateAnnotation{
  name="python.ImportStatementByAsName";
  version=3;
}
union ImportStatementByAsName {
  1: ImportStatementByAsName_id id (hs.strict);
  2: ImportStatementByAsName_key key;
  3: builtin.Unit get;
} (hs.prefix = "ImportStatementByAsName_with_")

typedef glean.Id ImportStatementByAsName_2_id

@glean.PredicateAnnotation{
  name="python.ImportStatementByAsName";
  version=2;
}
union ImportStatementByAsName_2 {
  1: ImportStatementByAsName_2_id id (hs.strict);
  2: Name key;
  3: builtin.Unit get;
} (hs.prefix = "ImportStatementByAsName_2_with_")

typedef glean.Id ImportStatement_id

@glean.PredicateAnnotation{
  name="python.ImportStatement";
  version=2;
}
union ImportStatement {
  1: ImportStatement_id id (hs.strict);
  2: ImportStatement_key key;
  3: builtin.Unit get;
} (hs.prefix = "ImportStatement_with_")

typedef glean.Id ImportStatement_1_id

@glean.PredicateAnnotation{
  name="python.ImportStatement";
  version=1;
}
union ImportStatement_1 {
  1: ImportStatement_1_id id (hs.strict);
  2: ImportStatement_1_key key;
  3: builtin.Unit get;
} (hs.prefix = "ImportStatement_1_with_")

typedef glean.Id FunctionDeclaration_id

@glean.PredicateAnnotation{
  name="python.FunctionDeclaration";
  version=1;
}
union FunctionDeclaration {
  1: FunctionDeclaration_id id (hs.strict);
  2: FunctionDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "FunctionDeclaration_with_")

typedef glean.Id FunctionBySName_id

@glean.PredicateAnnotation{
  name="python.FunctionBySName";
  version=2;
}
union FunctionBySName {
  1: FunctionBySName_id id (hs.strict);
  2: SName key;
  3: builtin.Unit get;
} (hs.prefix = "FunctionBySName_with_")

typedef glean.Id DocstringContent_id

@glean.PredicateAnnotation{
  name="python.DocstringContent";
  version=2;
}
union DocstringContent {
  1: DocstringContent_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "DocstringContent_with_")

typedef glean.Id ModuleDefinition_id

@glean.PredicateAnnotation{
  name="python.ModuleDefinition";
  version=2;
}
union ModuleDefinition {
  1: ModuleDefinition_id id (hs.strict);
  2: ModuleDefinition_key key;
  3: builtin.Unit get;
} (hs.prefix = "ModuleDefinition_with_")

typedef glean.Id FunctionDefinition_id

@glean.PredicateAnnotation{
  name="python.FunctionDefinition";
  version=2;
}
union FunctionDefinition {
  1: FunctionDefinition_id id (hs.strict);
  2: FunctionDefinition_key key;
  3: builtin.Unit get;
} (hs.prefix = "FunctionDefinition_with_")

typedef glean.Id DeclarationByName_id

@glean.PredicateAnnotation{
  name="python.DeclarationByName";
  version=2;
}
union DeclarationByName {
  1: DeclarationByName_id id (hs.strict);
  2: Name key;
  3: builtin.Unit get;
} (hs.prefix = "DeclarationByName_with_")

typedef glean.Id DeclarationBySName_id

@glean.PredicateAnnotation{
  name="python.DeclarationBySName";
  version=2;
}
union DeclarationBySName {
  1: DeclarationBySName_id id (hs.strict);
  2: SName key;
  3: builtin.Unit get;
} (hs.prefix = "DeclarationBySName_with_")

typedef glean.Id DeclarationLocation_id

@glean.PredicateAnnotation{
  name="python.DeclarationLocation";
  version=2;
}
union DeclarationLocation {
  1: DeclarationLocation_id id (hs.strict);
  2: DeclarationLocation_key key;
  3: builtin.Unit get;
} (hs.prefix = "DeclarationLocation_with_")

typedef glean.Id DeclarationToName_id

@glean.PredicateAnnotation{
  name="python.DeclarationToName";
  version=2;
}
union DeclarationToName {
  1: DeclarationToName_id id (hs.strict);
  2: Declaration key;
  3: builtin.Unit get;
} (hs.prefix = "DeclarationToName_with_")

typedef glean.Id DeclarationUses_id

@glean.PredicateAnnotation{
  name="python.DeclarationUses";
  version=2;
}
union DeclarationUses {
  1: DeclarationUses_id id (hs.strict);
  2: DeclarationUses_key key;
  3: builtin.Unit get;
} (hs.prefix = "DeclarationUses_with_")

typedef glean.Id DeclarationWithName_id

@glean.PredicateAnnotation{
  name="python.DeclarationWithName";
  version=2;
}
union DeclarationWithName {
  1: DeclarationWithName_id id (hs.strict);
  2: DeclarationWithName_key key;
  3: builtin.Unit get;
} (hs.prefix = "DeclarationWithName_with_")

typedef glean.Id DeclarationsByFile_id

@glean.PredicateAnnotation{
  name="python.DeclarationsByFile";
  version=2;
}
union DeclarationsByFile {
  1: DeclarationsByFile_id id (hs.strict);
  2: DeclarationsByFile_key key;
  3: builtin.Unit get;
} (hs.prefix = "DeclarationsByFile_with_")

typedef glean.Id DirectXRefsByFile_id

@glean.PredicateAnnotation{
  name="python.DirectXRefsByFile";
  version=2;
}
union DirectXRefsByFile {
  1: DirectXRefsByFile_id id (hs.strict);
  2: DirectXRefsByFile_key key;
  3: builtin.Unit get;
} (hs.prefix = "DirectXRefsByFile_with_")

typedef glean.Id FileDirectXRefs_id

@glean.PredicateAnnotation{
  name="python.FileDirectXRefs";
  version=2;
}
union FileDirectXRefs {
  1: FileDirectXRefs_id id (hs.strict);
  2: FileDirectXRefs_key key;
  3: builtin.Unit get;
} (hs.prefix = "FileDirectXRefs_with_")

typedef glean.Id TargetUses_id

@glean.PredicateAnnotation{
  name="python.TargetUses";
  version=2;
}
union TargetUses {
  1: TargetUses_id id (hs.strict);
  2: TargetUses_key key;
  3: builtin.Unit get;
} (hs.prefix = "TargetUses_with_")

typedef glean.Id FileXRefs_id

@glean.PredicateAnnotation{
  name="python.FileXRefs";
  version=2;
}
union FileXRefs {
  1: FileXRefs_id id (hs.strict);
  2: FileXRefs_key key;
  3: builtin.Unit get;
} (hs.prefix = "FileXRefs_with_")

typedef glean.Id XRefIndirectTarget_id

@glean.PredicateAnnotation{
  name="python.XRefIndirectTarget";
  version=2;
}
union XRefIndirectTarget {
  1: XRefIndirectTarget_id id (hs.strict);
  2: XRefIndirectTarget_key key;
  3: builtin.Unit get;
} (hs.prefix = "XRefIndirectTarget_with_")

typedef glean.Id DeclarationLocation_1_id

@glean.PredicateAnnotation{
  name="python.DeclarationLocation";
  version=1;
}
union DeclarationLocation_1 {
  1: DeclarationLocation_1_id id (hs.strict);
  2: DeclarationLocation_1_key key;
  3: builtin.Unit get;
} (hs.prefix = "DeclarationLocation_1_with_")

typedef glean.Id TargetUses_1_id

@glean.PredicateAnnotation{
  name="python.TargetUses";
  version=1;
}
union TargetUses_1 {
  1: TargetUses_1_id id (hs.strict);
  2: TargetUses_1_key key;
  3: builtin.Unit get;
} (hs.prefix = "TargetUses_1_with_")

typedef glean.Id FileXRefs_1_id

@glean.PredicateAnnotation{
  name="python.FileXRefs";
  version=1;
}
union FileXRefs_1 {
  1: FileXRefs_1_id id (hs.strict);
  2: FileXRefs_1_key key;
  3: builtin.Unit get;
} (hs.prefix = "FileXRefs_1_with_")

typedef glean.Id XRefIndirectTarget_1_id

@glean.PredicateAnnotation{
  name="python.XRefIndirectTarget";
  version=1;
}
union XRefIndirectTarget_1 {
  1: XRefIndirectTarget_1_id id (hs.strict);
  2: XRefIndirectTarget_1_key key;
  3: builtin.Unit get;
} (hs.prefix = "XRefIndirectTarget_1_with_")

typedef glean.Id ContainingTopLevelDeclaration_id

@glean.PredicateAnnotation{
  name="python.ContainingTopLevelDeclaration";
  version=3;
}
union ContainingTopLevelDeclaration {
  1: ContainingTopLevelDeclaration_id id (hs.strict);
  2: ContainingTopLevelDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "ContainingTopLevelDeclaration_with_")

typedef glean.Id ClassDefinition_id

@glean.PredicateAnnotation{
  name="python.ClassDefinition";
  version=2;
}
union ClassDefinition {
  1: ClassDefinition_id id (hs.strict);
  2: ClassDefinition_key key;
  3: builtin.Unit get;
} (hs.prefix = "ClassDefinition_with_")

typedef glean.Id ClassDeclaration_id

@glean.PredicateAnnotation{
  name="python.ClassDeclaration";
  version=1;
}
union ClassDeclaration {
  1: ClassDeclaration_id id (hs.strict);
  2: ClassDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "ClassDeclaration_with_")

typedef glean.Id ClassBySName_id

@glean.PredicateAnnotation{
  name="python.ClassBySName";
  version=2;
}
union ClassBySName {
  1: ClassBySName_id id (hs.strict);
  2: SName key;
  3: builtin.Unit get;
} (hs.prefix = "ClassBySName_with_")

struct XRefViaName {
  1: optional Name target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.ByteSpan source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union XRefsViaNameByFile_xrefs_array {
  1: XRefViaName every;
  2: list<XRefViaName> exact;
}

struct XRefsViaNameByFile_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional XRefsViaNameByFile_xrefs_array xrefs (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct VariableDefinition_type {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_ just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct VariableDefinition_key {
  1: optional VariableDeclaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional VariableDefinition_type type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct VariableDeclaration_key {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef VariableDeclaration VariableBySName_value

typedef Name SNameToName_value

struct SName_parent {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional SName just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct SName_key {
  1: optional Name local_name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional SName_parent parent (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Parameter_type {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_ just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct Parameter_value {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional string just;
  3: bool any = false;
}

struct Parameter {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Parameter_type type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional Parameter_value value;
}

typedef SName NameToSName_value

typedef Module ModuleBySName_value

struct Module_key {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct ImportStatementByName_key {
  1: optional Name as_name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Name from_name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct ImportStatementByAsSName_key {
  1: optional SName sname (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ImportStatement import_ (java.swift.name = "import_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef ImportStatement ImportStatementByAsSName_2_value

struct ImportStatementByAsName_key {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ImportStatement import_ (java.swift.name = "import_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef ImportStatement ImportStatementByAsName_2_value

struct ImportStatement_key {
  1: optional Name from_name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Name as_name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct ImportStatement_1_as_name {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Name just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct ImportStatement_1_key {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ImportStatement_1_as_name as_name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.ByteSpan span (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FunctionDeclaration_key {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef FunctionDeclaration FunctionBySName_value

struct Docstring {
  1: optional src.ByteSpan location (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct ModuleDefinition_docstring {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Docstring just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct ModuleDefinition_key {
  1: optional Module module (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ModuleDefinition_docstring docstring (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef string Decorator

struct FunctionDefinition_returns {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_ just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

union FunctionDefinition_params_array {
  1: Parameter every;
  2: list<Parameter> exact;
}

union FunctionDefinition_posonly_params_just__array {
  1: Parameter every;
  2: list<Parameter> exact;
}

struct FunctionDefinition_posonly_params {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional FunctionDefinition_posonly_params_just__array just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

union FunctionDefinition_kwonly_params_just__array {
  1: Parameter every;
  2: list<Parameter> exact;
}

struct FunctionDefinition_kwonly_params {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional FunctionDefinition_kwonly_params_just__array just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct FunctionDefinition_star_arg {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Parameter just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct FunctionDefinition_star_kwarg {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Parameter just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

union FunctionDefinition_decorators_just__array {
  1: Decorator every;
  2: list<Decorator> exact;
}

struct FunctionDefinition_decorators {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional FunctionDefinition_decorators_just__array just;
  3: bool any = false;
}

struct FunctionDefinition_docstring {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Docstring just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct FunctionDefinition_key {
  1: optional FunctionDeclaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional bool is_async;
  3: optional FunctionDefinition_returns returns (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional FunctionDefinition_params_array params (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional FunctionDefinition_posonly_params posonly_params (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: optional FunctionDefinition_kwonly_params kwonly_params (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  7: optional FunctionDefinition_star_arg star_arg (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  8: optional FunctionDefinition_star_kwarg star_kwarg (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  9: optional FunctionDefinition_decorators decorators;
  10: optional FunctionDefinition_docstring docstring (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Declaration {
  1: optional ClassDeclaration cls (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional FunctionDeclaration func (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional VariableDeclaration variable (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional ImportStatement imp (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional Module module (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: bool any = false;
}

typedef Declaration DeclarationByName_value

typedef Declaration DeclarationBySName_value

struct DeclarationLocation_key {
  1: optional Declaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.ByteSpan span (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef Name DeclarationToName_value

struct DeclarationUses_key {
  1: optional Declaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.ByteSpan span (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct DeclarationWithName_key {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Declaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct DeclarationsByFile_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.ByteSpan span (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional Declaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct DirectXRef {
  1: optional Declaration target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.ByteSpan source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct DirectXRefsByFile_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional DirectXRef xref (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union FileDirectXRefs_xrefs_array {
  1: DirectXRef every;
  2: list<DirectXRef> exact;
}

struct FileDirectXRefs_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional FileDirectXRefs_xrefs_array xrefs (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct XRefTarget {
  1: optional Declaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional XRefIndirectTarget indirect (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional builtin.Unit unknown (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: bool any = false;
}

struct TargetUses_key {
  1: optional XRefTarget target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.ByteSpans uses (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct XRef {
  1: optional XRefTarget target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.ByteSpan source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union FileXRefs_xrefs_array {
  1: XRef every;
  2: list<XRef> exact;
}

struct FileXRefs_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional FileXRefs_xrefs_array xrefs (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct XRefIndirectTarget_key {
  1: optional ImportStatement import_statement (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional XRefTarget target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Declaration_1 {
  1: optional ClassDeclaration cls (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional FunctionDeclaration func (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional VariableDeclaration variable (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: bool any = false;
}

struct DeclarationLocation_1_key {
  1: optional Declaration_1 declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.ByteSpan span (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct XRefTarget_1 {
  1: optional Declaration_1 declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Module module (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional XRefIndirectTarget_1 indirect (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional builtin.Unit unknown (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: bool any = false;
}

struct TargetUses_1_key {
  1: optional XRefTarget_1 target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.ByteSpans uses (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct XRef_1 {
  1: optional XRefTarget_1 target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.ByteSpan source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union FileXRefs_1_xrefs_array {
  1: XRef_1 every;
  2: list<XRef_1> exact;
}

struct FileXRefs_1_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional FileXRefs_1_xrefs_array xrefs (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct XRefIndirectTarget_1_key {
  1: optional ImportStatement_1 import_statement (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional XRefTarget_1 target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct ContainingTopLevelDeclaration_key {
  1: optional Declaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Declaration container (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union ClassDefinition_bases_just__array {
  1: ClassDeclaration every;
  2: list<ClassDeclaration> exact;
}

struct ClassDefinition_bases {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ClassDefinition_bases_just__array just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

union ClassDefinition_keywords_just__array {
  1: Parameter every;
  2: list<Parameter> exact;
}

struct ClassDefinition_keywords {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ClassDefinition_keywords_just__array just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

union ClassDefinition_decorators_just__array {
  1: Decorator every;
  2: list<Decorator> exact;
}

struct ClassDefinition_decorators {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ClassDefinition_decorators_just__array just;
  3: bool any = false;
}

struct ClassDefinition_docstring {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Docstring just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct ClassDefinition_key {
  1: optional ClassDeclaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ClassDefinition_bases bases (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional ClassDefinition_keywords keywords (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional ClassDefinition_decorators decorators;
  5: optional ClassDefinition_docstring docstring (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union ClassDeclaration_bases_just__array {
  1: Name every;
  2: list<Name> exact;
}

struct ClassDeclaration_bases {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ClassDeclaration_bases_just__array just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct ClassDeclaration_key {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ClassDeclaration_bases bases (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef ClassDeclaration ClassBySName_value
