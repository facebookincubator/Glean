// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/v2/buck.thrift"
include "glean/schema/v2/builtin.thrift"
include "glean/schema/v2/pp1.thrift"
include "glean/schema/v2/src.thrift"

namespace cpp2 facebook.glean.schema.cxx1
namespace hs Glean.Schema
namespace php glean_schema_cxx1
namespace py glean.schema.cxx1
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.cxx1
namespace rust glean_schema_cxx1

hs_include "glean/schema/v2/cxx1_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "NamespaceQName": 1,
  "DeclarationSrcRange": 4,
  "RecordDeclaration": 1,
  "XRefIndirectTarget": 2,
  "DeclByName": 4,
  "DeclInObjcContainer": 4,
  "DeclarationTargets": 1,
  "ObjcContainerInheritance": 4,
  "ObjcPropertyIVar": 4,
  "FunctionDefinition": 1,
  "TranslationUnitXRefs": 4,
  "Signature": 1,
  "Attribute": 4,
  "DeclInRecord": 4,
  "FileXRefs": 2,
  "ObjcSelector": 1,
  "NamespaceDefinition": 2,
  "FunctionAttribute": 4,
  "TypeAliasDeclaration": 2,
  "FileXRefMap": 2,
  "EnumDeclaration": 1,
  "ObjcContainerDeclaration": 1,
  "ObjcMethodDeclaration": 1,
  "MethodOverrides": 1,
  "ObjcInterfaceToImplementation": 1,
  "FunctionName": 1,
  "Same": 2,
  "FunctionDeclaration": 1,
  "Name": 1,
  "DeclImpliesRecord": 4,
  "FunctionQName": 1,
  "TranslationUnitTrace": 3,
  "Enumerator": 1,
  "ObjcPropertyImplementation": 1,
  "Trace": 2,
  "MethodOverridden": 4,
  "TargetUses": 2,
  "UsingDirective": 2,
  "DeclFamilyOf": 4,
  "PPTrace": 2,
  "QName": 1,
  "VariableDeclaration": 2,
  "DeclarationSources": 1,
  "ObjcContainerBase": 4,
  "UsingDeclaration": 2,
  "ObjcMethodDefinition": 1,
  "DeclarationInTrace": 4,
  "ObjcContainerDefinition": 2,
  "EnumDefinition": 1,
  "Declarations": 2,
  "DeclToFamily": 2,
  "NamespaceDeclaration": 1,
  "DeclarationComment": 1,
  "ObjcPropertyDeclaration": 1,
  "RecordDefinition": 2,
  "ObjcImplements": 1,
  "EnumeratorInEnum": 4,
  "DeclFamily": 2,
  "Type": 1,
  "RecordDerived": 4,
}


typedef glean.Id UsingDirective_id

@glean.PredicateAnnotation{
  name="cxx1.UsingDirective";
  version=2;
}
struct UsingDirective {
  1: UsingDirective_id id (hs.strict);
  2: optional UsingDirective_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id UsingDeclaration_id

@glean.PredicateAnnotation{
  name="cxx1.UsingDeclaration";
  version=2;
}
struct UsingDeclaration {
  1: UsingDeclaration_id id (hs.strict);
  2: optional UsingDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TypeAliasDeclaration_id

@glean.PredicateAnnotation{
  name="cxx1.TypeAliasDeclaration";
  version=2;
}
struct TypeAliasDeclaration {
  1: TypeAliasDeclaration_id id (hs.strict);
  2: optional TypeAliasDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Type_id

@glean.PredicateAnnotation{
  name="cxx1.Type";
  version=1;
}
struct Type {
  1: Type_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id TranslationUnitXRefs_id

@glean.PredicateAnnotation{
  name="cxx1.TranslationUnitXRefs";
  version=4;
}
struct TranslationUnitXRefs {
  1: TranslationUnitXRefs_id id (hs.strict);
  2: optional TranslationUnitXRefs_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TranslationUnitTrace_id

@glean.PredicateAnnotation{
  name="cxx1.TranslationUnitTrace";
  version=3;
}
struct TranslationUnitTrace {
  1: TranslationUnitTrace_id id (hs.strict);
  2: optional TranslationUnitTrace_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Trace_id

@glean.PredicateAnnotation{
  name="cxx1.Trace";
  version=2;
}
struct Trace {
  1: Trace_id id (hs.strict);
  2: optional Trace_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id RecordDerived_id

@glean.PredicateAnnotation{
  name="cxx1.RecordDerived";
  version=4;
}
struct RecordDerived {
  1: RecordDerived_id id (hs.strict);
  2: optional RecordDerived_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id RecordDeclaration_id

@glean.PredicateAnnotation{
  name="cxx1.RecordDeclaration";
  version=1;
}
struct RecordDeclaration {
  1: RecordDeclaration_id id (hs.strict);
  2: optional RecordDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Signature_id

@glean.PredicateAnnotation{
  name="cxx1.Signature";
  version=1;
}
struct Signature {
  1: Signature_id id (hs.strict);
  2: optional Signature_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ObjcSelector_id

@glean.PredicateAnnotation{
  name="cxx1.ObjcSelector";
  version=1;
}
struct ObjcSelector {
  1: ObjcSelector_id id (hs.strict);
  2: optional list<string> key;
}

typedef glean.Id ObjcPropertyImplementation_id

@glean.PredicateAnnotation{
  name="cxx1.ObjcPropertyImplementation";
  version=1;
}
struct ObjcPropertyImplementation {
  1: ObjcPropertyImplementation_id id (hs.strict);
  2: optional ObjcPropertyImplementation_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ObjcPropertyIVar_id

@glean.PredicateAnnotation{
  name="cxx1.ObjcPropertyIVar";
  version=4;
}
struct ObjcPropertyIVar {
  1: ObjcPropertyIVar_id id (hs.strict);
  2: optional ObjcPropertyIVar_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ObjcMethodDefinition_id

@glean.PredicateAnnotation{
  name="cxx1.ObjcMethodDefinition";
  version=1;
}
struct ObjcMethodDefinition {
  1: ObjcMethodDefinition_id id (hs.strict);
  2: optional ObjcMethodDeclaration key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ObjcInterfaceToImplementation_id

@glean.PredicateAnnotation{
  name="cxx1.ObjcInterfaceToImplementation";
  version=1;
}
struct ObjcInterfaceToImplementation {
  1: ObjcInterfaceToImplementation_id id (hs.strict);
  2: optional ObjcInterfaceToImplementation_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ObjcImplements_id

@glean.PredicateAnnotation{
  name="cxx1.ObjcImplements";
  version=1;
}
struct ObjcImplements {
  1: ObjcImplements_id id (hs.strict);
  2: optional ObjcImplements_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ObjcContainerInheritance_id

@glean.PredicateAnnotation{
  name="cxx1.ObjcContainerInheritance";
  version=4;
}
struct ObjcContainerInheritance {
  1: ObjcContainerInheritance_id id (hs.strict);
  2: optional ObjcContainerInheritance_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ObjcContainerDefinition_id

@glean.PredicateAnnotation{
  name="cxx1.ObjcContainerDefinition";
  version=2;
}
struct ObjcContainerDefinition {
  1: ObjcContainerDefinition_id id (hs.strict);
  2: optional ObjcContainerDefinition_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ObjcContainerBase_id

@glean.PredicateAnnotation{
  name="cxx1.ObjcContainerBase";
  version=4;
}
struct ObjcContainerBase {
  1: ObjcContainerBase_id id (hs.strict);
  2: optional ObjcContainerBase_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ObjcContainerDeclaration_id

@glean.PredicateAnnotation{
  name="cxx1.ObjcContainerDeclaration";
  version=1;
}
struct ObjcContainerDeclaration {
  1: ObjcContainerDeclaration_id id (hs.strict);
  2: optional ObjcContainerDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ObjcMethodDeclaration_id

@glean.PredicateAnnotation{
  name="cxx1.ObjcMethodDeclaration";
  version=1;
}
struct ObjcMethodDeclaration {
  1: ObjcMethodDeclaration_id id (hs.strict);
  2: optional ObjcMethodDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ObjcPropertyDeclaration_id

@glean.PredicateAnnotation{
  name="cxx1.ObjcPropertyDeclaration";
  version=1;
}
struct ObjcPropertyDeclaration {
  1: ObjcPropertyDeclaration_id id (hs.strict);
  2: optional ObjcPropertyDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id NamespaceQName_id

@glean.PredicateAnnotation{
  name="cxx1.NamespaceQName";
  version=1;
}
struct NamespaceQName {
  1: NamespaceQName_id id (hs.strict);
  2: optional NamespaceQName_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id NamespaceDefinition_id

@glean.PredicateAnnotation{
  name="cxx1.NamespaceDefinition";
  version=2;
}
struct NamespaceDefinition {
  1: NamespaceDefinition_id id (hs.strict);
  2: optional NamespaceDefinition_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id NamespaceDeclaration_id

@glean.PredicateAnnotation{
  name="cxx1.NamespaceDeclaration";
  version=1;
}
struct NamespaceDeclaration {
  1: NamespaceDeclaration_id id (hs.strict);
  2: optional NamespaceDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Name_id

@glean.PredicateAnnotation{
  name="cxx1.Name";
  version=1;
}
struct Name {
  1: Name_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id MethodOverrides_id

@glean.PredicateAnnotation{
  name="cxx1.MethodOverrides";
  version=1;
}
struct MethodOverrides {
  1: MethodOverrides_id id (hs.strict);
  2: optional MethodOverrides_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id MethodOverridden_id

@glean.PredicateAnnotation{
  name="cxx1.MethodOverridden";
  version=4;
}
struct MethodOverridden {
  1: MethodOverridden_id id (hs.strict);
  2: optional MethodOverridden_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id PPTrace_id

@glean.PredicateAnnotation{
  name="cxx1.PPTrace";
  version=2;
}
struct PPTrace {
  1: PPTrace_id id (hs.strict);
  2: optional PPTrace_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FunctionName_id

@glean.PredicateAnnotation{
  name="cxx1.FunctionName";
  version=1;
}
struct FunctionName {
  1: FunctionName_id id (hs.strict);
  2: optional FunctionName_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FunctionDefinition_id

@glean.PredicateAnnotation{
  name="cxx1.FunctionDefinition";
  version=1;
}
struct FunctionDefinition {
  1: FunctionDefinition_id id (hs.strict);
  2: optional FunctionDefinition_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FunctionDeclaration_id

@glean.PredicateAnnotation{
  name="cxx1.FunctionDeclaration";
  version=1;
}
struct FunctionDeclaration {
  1: FunctionDeclaration_id id (hs.strict);
  2: optional FunctionDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FunctionAttribute_id

@glean.PredicateAnnotation{
  name="cxx1.FunctionAttribute";
  version=4;
}
struct FunctionAttribute {
  1: FunctionAttribute_id id (hs.strict);
  2: optional FunctionAttribute_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id VariableDeclaration_id

@glean.PredicateAnnotation{
  name="cxx1.VariableDeclaration";
  version=2;
}
struct VariableDeclaration {
  1: VariableDeclaration_id id (hs.strict);
  2: optional VariableDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id EnumeratorInEnum_id

@glean.PredicateAnnotation{
  name="cxx1.EnumeratorInEnum";
  version=4;
}
struct EnumeratorInEnum {
  1: EnumeratorInEnum_id id (hs.strict);
  2: optional EnumeratorInEnum_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Enumerator_id

@glean.PredicateAnnotation{
  name="cxx1.Enumerator";
  version=1;
}
struct Enumerator {
  1: Enumerator_id id (hs.strict);
  2: optional Enumerator_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id EnumDefinition_id

@glean.PredicateAnnotation{
  name="cxx1.EnumDefinition";
  version=1;
}
struct EnumDefinition {
  1: EnumDefinition_id id (hs.strict);
  2: optional EnumDefinition_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id EnumDeclaration_id

@glean.PredicateAnnotation{
  name="cxx1.EnumDeclaration";
  version=1;
}
struct EnumDeclaration {
  1: EnumDeclaration_id id (hs.strict);
  2: optional EnumDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclarationComment_id

@glean.PredicateAnnotation{
  name="cxx1.DeclarationComment";
  version=1;
}
struct DeclarationComment {
  1: DeclarationComment_id id (hs.strict);
  2: optional DeclarationComment_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclarationInTrace_id

@glean.PredicateAnnotation{
  name="cxx1.DeclarationInTrace";
  version=4;
}
struct DeclarationInTrace {
  1: DeclarationInTrace_id id (hs.strict);
  2: optional DeclarationInTrace_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclarationSources_id

@glean.PredicateAnnotation{
  name="cxx1.DeclarationSources";
  version=1;
}
struct DeclarationSources {
  1: DeclarationSources_id id (hs.strict);
  2: optional DeclarationSources_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclarationSrcRange_id

@glean.PredicateAnnotation{
  name="cxx1.DeclarationSrcRange";
  version=4;
}
struct DeclarationSrcRange {
  1: DeclarationSrcRange_id id (hs.strict);
  2: optional DeclarationSrcRange_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclarationTargets_id

@glean.PredicateAnnotation{
  name="cxx1.DeclarationTargets";
  version=1;
}
struct DeclarationTargets {
  1: DeclarationTargets_id id (hs.strict);
  2: optional DeclarationTargets_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Declarations_id

@glean.PredicateAnnotation{
  name="cxx1.Declarations";
  version=2;
}
struct Declarations {
  1: Declarations_id id (hs.strict);
  2: optional list<Declaration> key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Same_id

@glean.PredicateAnnotation{
  name="cxx1.Same";
  version=2;
}
struct Same {
  1: Same_id id (hs.strict);
  2: optional Same_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FileXRefs_id

@glean.PredicateAnnotation{
  name="cxx1.FileXRefs";
  version=2;
}
struct FileXRefs {
  1: FileXRefs_id id (hs.strict);
  2: optional FileXRefs_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FileXRefMap_id

@glean.PredicateAnnotation{
  name="cxx1.FileXRefMap";
  version=2;
}
struct FileXRefMap {
  1: FileXRefMap_id id (hs.strict);
  2: optional FileXRefMap_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TargetUses_id

@glean.PredicateAnnotation{
  name="cxx1.TargetUses";
  version=2;
}
struct TargetUses {
  1: TargetUses_id id (hs.strict);
  2: optional TargetUses_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id XRefIndirectTarget_id

@glean.PredicateAnnotation{
  name="cxx1.XRefIndirectTarget";
  version=2;
}
struct XRefIndirectTarget {
  1: XRefIndirectTarget_id id (hs.strict);
  2: optional XRefIndirectTarget_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclToFamily_id

@glean.PredicateAnnotation{
  name="cxx1.DeclToFamily";
  version=2;
}
struct DeclToFamily {
  1: DeclToFamily_id id (hs.strict);
  2: optional DeclToFamily_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclInRecord_id

@glean.PredicateAnnotation{
  name="cxx1.DeclInRecord";
  version=4;
}
struct DeclInRecord {
  1: DeclInRecord_id id (hs.strict);
  2: optional DeclInRecord_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclInObjcContainer_id

@glean.PredicateAnnotation{
  name="cxx1.DeclInObjcContainer";
  version=4;
}
struct DeclInObjcContainer {
  1: DeclInObjcContainer_id id (hs.strict);
  2: optional DeclInObjcContainer_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclImpliesRecord_id

@glean.PredicateAnnotation{
  name="cxx1.DeclImpliesRecord";
  version=4;
}
struct DeclImpliesRecord {
  1: DeclImpliesRecord_id id (hs.strict);
  2: optional DeclImpliesRecord_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclFamilyOf_id

@glean.PredicateAnnotation{
  name="cxx1.DeclFamilyOf";
  version=4;
}
struct DeclFamilyOf {
  1: DeclFamilyOf_id id (hs.strict);
  2: optional DeclFamilyOf_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclFamily_id

@glean.PredicateAnnotation{
  name="cxx1.DeclFamily";
  version=2;
}
struct DeclFamily {
  1: DeclFamily_id id (hs.strict);
  2: optional list<Declaration> key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclByName_id

@glean.PredicateAnnotation{
  name="cxx1.DeclByName";
  version=4;
}
struct DeclByName {
  1: DeclByName_id id (hs.strict);
  2: optional DeclByName_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Attribute_id

@glean.PredicateAnnotation{
  name="cxx1.Attribute";
  version=4;
}
struct Attribute {
  1: Attribute_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id RecordDefinition_id

@glean.PredicateAnnotation{
  name="cxx1.RecordDefinition";
  version=2;
}
struct RecordDefinition {
  1: RecordDefinition_id id (hs.strict);
  2: optional RecordDefinition_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FunctionQName_id

@glean.PredicateAnnotation{
  name="cxx1.FunctionQName";
  version=1;
}
struct FunctionQName {
  1: FunctionQName_id id (hs.strict);
  2: optional FunctionQName_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id QName_id

@glean.PredicateAnnotation{
  name="cxx1.QName";
  version=1;
}
struct QName {
  1: QName_id id (hs.strict);
  2: optional QName_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union XRefVia {
  1: UsingDeclaration usingDeclaration;
  2: UsingDirective usingDirective;
  3: pp1.Use macro;
} (hs.nonempty)

struct UsingDirective_key {
  1: QName name;
  2: src.Range source;
}

struct UsingDeclaration_key {
  1: FunctionQName name;
  2: src.Range source;
}

enum TypeAliasKind {
  Typedef = 0,
  Using = 1
} (hs.nounknown)

struct TypeAliasDeclaration_key {
  1: QName name;
  2: Type type;
  3: TypeAliasKind kind;
  4: src.Range source;
}

struct TranslationUnitXRefs_key {
  1: buck.TranslationUnit tunit;
  2: list<FileXRefs> xrefs;
}

struct TranslationUnitTrace_key {
  1: buck.TranslationUnit tunit;
  2: Trace trace;
}

struct Trace_key {
  1: src.File file;
  2: Declarations declarations;
  3: PPTrace preprocessor;
}

enum RefQualifier {
  None_ = 0,
  LValue = 1,
  RValue = 2
} (hs.nounknown)

union RecordKind {
  1: builtin.Unit struct_ (java.swift.name = "struct_");
  2: builtin.Unit class_ (java.swift.name = "class_");
  3: builtin.Unit union_ (java.swift.name = "union_");
} (hs.nonempty)

struct RecordDerived_key {
  1: RecordDeclaration base;
  2: RecordDeclaration derived;
}

struct RecordDeclaration_key {
  1: QName name;
  2: RecordKind kind;
  3: src.Range source;
}

struct Parameter {
  1: Name name;
  2: Type type;
}

struct Signature_key {
  1: Type returns;
  2: list<Parameter> parameters;
}

typedef string Operator

enum ObjcPropertyKind {
  Synthesize = 0,
  Dynamic = 1
} (hs.nounknown)

struct ObjcPropertyImplementation_key {
  1: ObjcPropertyDeclaration declaration;
  2: ObjcPropertyKind kind;
  3: optional Name ivar;
  4: src.Range source;
}

struct ObjcPropertyIVar_key {
  1: ObjcPropertyDeclaration property;
  2: VariableDeclaration ivar;
}

struct ObjcInterfaceToImplementation_key {
  1: ObjcContainerDeclaration interface_ (java.swift.name = "interface_");
  2: ObjcContainerDeclaration implementation;
}

struct ObjcImplements_key {
  1: ObjcContainerDeclaration implementation;
  2: ObjcContainerDeclaration interface_ (java.swift.name = "interface_");
}

struct ObjcIVar {
  1: bool synthesize;
  2: optional glean.Nat bitsize;
}

struct ObjcContainerInheritance_key {
  1: ObjcContainerDeclaration base;
  2: ObjcContainerDeclaration declaration;
}

struct ObjcContainerDefinition_key {
  1: ObjcContainerDeclaration declaration;
  2: list<ObjcContainerDeclaration> protocols;
  3: Declarations members;
}

struct ObjcContainerBase_key {
  1: ObjcContainerDeclaration declaration;
  2: ObjcContainerDeclaration base;
}

struct ObjcCategoryId {
  1: Name className;
  2: Name categoryName;
}

union ObjcContainerId {
  1: Name protocol;
  2: Name interface_ (java.swift.name = "interface_");
  3: ObjcCategoryId categoryInterface;
  4: Name extensionInterface;
  5: Name implementation;
  6: ObjcCategoryId categoryImplementation;
} (hs.nonempty)

struct ObjcContainerDeclaration_key {
  1: ObjcContainerId id;
  2: src.Range source;
}

struct ObjcMethodDeclaration_key {
  1: ObjcSelector selector;
  2: ObjcContainerId container;
  3: Signature signature;
  4: bool isInstance;
  5: bool isOptional;
  6: bool isAccessor;
  7: src.Range source;
}

struct ObjcPropertyDeclaration_key {
  1: Name name;
  2: ObjcContainerId container;
  3: Type type;
  4: bool isInstance;
  5: bool isOptional;
  6: bool isReadOnly;
  7: bool isAtomic;
  8: src.Range source;
}

struct NamespaceQName_key {
  1: optional Name name;
  2: optional NamespaceQName parent;
}

struct NamespaceDefinition_key {
  1: NamespaceDeclaration declaration;
  2: Declarations members;
}

struct NamespaceDeclaration_key {
  1: NamespaceQName name;
  2: src.Range source;
}

struct MethodSignature {
  1: bool isVirtual;
  2: bool isConst;
  3: bool isVolatile;
  4: RefQualifier refQualifier;
}

struct MethodOverrides_key {
  1: FunctionDeclaration derived;
  2: FunctionDeclaration base;
}

struct MethodOverridden_key {
  1: FunctionDeclaration base;
  2: FunctionDeclaration derived;
}

typedef string LiteralOperator

struct IncludeTrace {
  1: pp1.Include include_ (java.swift.name = "include_");
  2: optional Trace trace;
}

union PPEvent {
  1: IncludeTrace include_ (java.swift.name = "include_");
  2: pp1.Define define;
  3: pp1.Undef undef;
  4: pp1.Use use;
} (hs.nonempty)

struct PPTrace_key {
  1: src.File file;
  2: list<PPEvent> events;
}

enum GlobalVariableKind {
  SimpleVariable = 0,
  StaticVariable = 1,
  StaticMember = 2
} (hs.nounknown)

enum GlobalVariableAttribute {
  Plain = 0,
  Inline = 1,
  Constexpr = 2
} (hs.nounknown)

struct GlobalVariable {
  1: GlobalVariableKind kind;
  2: GlobalVariableAttribute attribute;
  3: bool definition;
}

union FunctionName_key {
  1: Name name (py3.name = "name_");
  2: Operator operator_ (java.swift.name = "operator_");
  3: LiteralOperator literalOperator;
  4: builtin.Unit constructor;
  5: builtin.Unit destructor;
  6: Type conversionOperator;
} (hs.nonempty)

struct FunctionDefinition_key {
  1: FunctionDeclaration declaration;
  2: bool isInline;
}

struct FunctionDeclaration_key {
  1: FunctionQName name;
  2: Signature signature;
  3: optional MethodSignature method;
  4: src.Range source;
}

struct FunctionAttribute_key {
  1: Attribute attr;
  2: FunctionDeclaration declaration;
}

struct Field {
  1: bool mutable_ (java.swift.name = "mutable_");
  2: optional glean.Nat bitsize;
}

union VariableKind {
  1: GlobalVariable global_ (java.swift.name = "global_");
  2: Field field;
  3: ObjcIVar ivar;
} (hs.nonempty)

struct VariableDeclaration_key {
  1: QName name;
  2: Type type;
  3: VariableKind kind;
  4: src.Range source;
}

struct EnumeratorInEnum_key {
  1: Enumerator enumerator;
  2: EnumDefinition enum_ (java.swift.name = "enum_");
}

struct Enumerator_key {
  1: Name name;
  2: EnumDeclaration enumeration;
  3: src.Range source;
}

struct EnumDefinition_key {
  1: EnumDeclaration declaration;
  2: list<Enumerator> enumerators;
}

struct EnumDeclaration_key {
  1: QName name;
  2: bool isScoped;
  3: optional Type type;
  4: src.Range source;
}

union Declaration {
  1: NamespaceDeclaration namespace_ (java.swift.name = "namespace_");
  2: UsingDeclaration usingDeclaration;
  3: UsingDirective usingDirective;
  4: RecordDeclaration record_ (java.swift.name = "record_");
  5: EnumDeclaration enum_ (java.swift.name = "enum_");
  6: FunctionDeclaration function_ (java.swift.name = "function_");
  7: VariableDeclaration variable;
  8: ObjcContainerDeclaration objcContainer;
  9: ObjcMethodDeclaration objcMethod;
  10: ObjcPropertyDeclaration objcProperty;
  11: TypeAliasDeclaration typeAlias;
} (hs.nonempty)

struct DeclarationComment_key {
  1: Declaration declaration;
  2: src.File file;
  3: src.ByteSpan span;
}

struct DeclarationInTrace_key {
  1: Declaration decl;
  2: Trace trace;
}

struct DeclarationSources_key {
  1: Declaration target;
  2: list<Declaration> sources;
}

struct DeclarationSrcRange_key {
  1: Declaration decl;
  2: src.Range source;
}

struct DeclarationTargets_key {
  1: Declaration source;
  2: list<Declaration> targets;
}

struct Same_key {
  1: Declaration declaration1;
  2: Declaration declaration2;
}

union XRefTarget {
  1: Declaration declaration;
  2: Enumerator enumerator;
  3: ObjcSelector objcSelector;
  4: src.Loc unknown;
  5: XRefIndirectTarget indirect;
} (hs.nonempty)

struct FileXRefs_key {
  1: FileXRefMap xmap;
  2: list<XRefTarget> externals;
}

struct FixedXRef {
  1: XRefTarget target;
  2: src.ByteSpans ranges;
}

struct FileXRefMap_key {
  1: src.File file;
  2: list<FixedXRef> fixed;
  3: list<src.ByteSpans> variable;
}

struct TargetUses_key {
  1: XRefTarget target;
  2: src.File file;
  3: src.ByteSpans uses;
}

struct XRefIndirectTarget_key {
  1: XRefVia via;
  2: XRefTarget target;
}

struct DeclToFamily_key {
  1: Declaration decl;
  2: DeclFamily family;
}

enum DeclKind {
  namespace_ = 0,
  usingDeclaration = 1,
  usingDirective = 2,
  record_ = 3,
  enum_ = 4,
  enumerator = 5,
  function_ = 6,
  variable = 7,
  objcContainer = 8,
  objcMethod = 9,
  objcProperty = 10,
  typeAlias = 11,
  macro = 12
} (hs.nounknown)

struct DeclInRecord_key {
  1: Declaration decl;
  2: RecordDefinition record;
}

struct DeclInObjcContainer_key {
  1: Declaration decl;
  2: ObjcContainerDefinition record;
}

struct DeclImpliesRecord_key {
  1: Declaration decl;
  2: RecordDefinition record;
}

union DeclIdent {
  1: Name name (py3.name = "name_");
  2: pp1.Macro macro;
  3: ObjcSelector selector;
} (hs.nonempty)

struct DeclFamilyOf_key {
  1: Declaration decl;
  2: Declaration family;
}

struct DeclByName_key {
  1: string name_lowercase;
  2: DeclKind kind;
  3: DeclIdent ident;
}

enum Access {
  Public = 0,
  Protected = 1,
  Private = 2
} (hs.nounknown)

struct RecordBase {
  1: RecordDeclaration base;
  2: Access access;
  3: bool isVirtual;
}

struct RecordDefinition_key {
  1: RecordDeclaration declaration;
  2: list<RecordBase> bases;
  3: Declarations members;
}

struct Scope_recordWithAccess_ {
  1: QName record;
  2: Access access;
}

union Scope {
  1: builtin.Unit global_ (java.swift.name = "global_");
  2: NamespaceQName namespace_ (java.swift.name = "namespace_");
  3: Scope_recordWithAccess_ recordWithAccess;
  4: FunctionQName local;
} (hs.nonempty)

struct FunctionQName_key {
  1: FunctionName name;
  2: Scope scope;
}

struct QName_key {
  1: Name name;
  2: Scope scope;
}
