// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/query/buck.thrift"
include "glean/schema/query/builtin.thrift"
include "glean/schema/query/pp1.thrift"
include "glean/schema/query/src.thrift"

namespace cpp2 facebook.glean.schema.query.cxx1
namespace hs Glean.Schema.Query
namespace php glean_schema_query_cxx1
namespace py glean.schema.query.cxx1
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.cxx1
namespace rust glean_schema_query_cxx1

hs_include "glean/schema/query/cxx1_include.hs"
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
union UsingDirective {
  1: UsingDirective_id id (hs.strict);
  2: UsingDirective_key key;
  3: builtin.Unit get;
} (hs.prefix = "UsingDirective_with_")

typedef glean.Id UsingDeclaration_id

@glean.PredicateAnnotation{
  name="cxx1.UsingDeclaration";
  version=2;
}
union UsingDeclaration {
  1: UsingDeclaration_id id (hs.strict);
  2: UsingDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "UsingDeclaration_with_")

typedef glean.Id TypeAliasDeclaration_id

@glean.PredicateAnnotation{
  name="cxx1.TypeAliasDeclaration";
  version=2;
}
union TypeAliasDeclaration {
  1: TypeAliasDeclaration_id id (hs.strict);
  2: TypeAliasDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "TypeAliasDeclaration_with_")

typedef glean.Id Type_id

@glean.PredicateAnnotation{
  name="cxx1.Type";
  version=1;
}
union Type_ {
  1: Type_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "Type__with_")

typedef glean.Id TranslationUnitXRefs_id

@glean.PredicateAnnotation{
  name="cxx1.TranslationUnitXRefs";
  version=4;
}
union TranslationUnitXRefs {
  1: TranslationUnitXRefs_id id (hs.strict);
  2: TranslationUnitXRefs_key key;
  3: builtin.Unit get;
} (hs.prefix = "TranslationUnitXRefs_with_")

typedef glean.Id TranslationUnitTrace_id

@glean.PredicateAnnotation{
  name="cxx1.TranslationUnitTrace";
  version=3;
}
union TranslationUnitTrace {
  1: TranslationUnitTrace_id id (hs.strict);
  2: TranslationUnitTrace_key key;
  3: builtin.Unit get;
} (hs.prefix = "TranslationUnitTrace_with_")

typedef glean.Id Trace_id

@glean.PredicateAnnotation{
  name="cxx1.Trace";
  version=2;
}
union Trace {
  1: Trace_id id (hs.strict);
  2: Trace_key key;
  3: builtin.Unit get;
} (hs.prefix = "Trace_with_")

typedef glean.Id RecordDerived_id

@glean.PredicateAnnotation{
  name="cxx1.RecordDerived";
  version=4;
}
union RecordDerived {
  1: RecordDerived_id id (hs.strict);
  2: RecordDerived_key key;
  3: builtin.Unit get;
} (hs.prefix = "RecordDerived_with_")

typedef glean.Id RecordDeclaration_id

@glean.PredicateAnnotation{
  name="cxx1.RecordDeclaration";
  version=1;
}
union RecordDeclaration {
  1: RecordDeclaration_id id (hs.strict);
  2: RecordDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "RecordDeclaration_with_")

typedef glean.Id Signature_id

@glean.PredicateAnnotation{
  name="cxx1.Signature";
  version=1;
}
union Signature {
  1: Signature_id id (hs.strict);
  2: Signature_key key;
  3: builtin.Unit get;
} (hs.prefix = "Signature_with_")

typedef glean.Id ObjcSelector_id

@glean.PredicateAnnotation{
  name="cxx1.ObjcSelector";
  version=1;
}
union ObjcSelector {
  1: ObjcSelector_id id (hs.strict);
  2: ObjcSelector_array key;
  3: builtin.Unit get;
} (hs.prefix = "ObjcSelector_with_")

typedef glean.Id ObjcPropertyImplementation_id

@glean.PredicateAnnotation{
  name="cxx1.ObjcPropertyImplementation";
  version=1;
}
union ObjcPropertyImplementation {
  1: ObjcPropertyImplementation_id id (hs.strict);
  2: ObjcPropertyImplementation_key key;
  3: builtin.Unit get;
} (hs.prefix = "ObjcPropertyImplementation_with_")

typedef glean.Id ObjcPropertyIVar_id

@glean.PredicateAnnotation{
  name="cxx1.ObjcPropertyIVar";
  version=4;
}
union ObjcPropertyIVar {
  1: ObjcPropertyIVar_id id (hs.strict);
  2: ObjcPropertyIVar_key key;
  3: builtin.Unit get;
} (hs.prefix = "ObjcPropertyIVar_with_")

typedef glean.Id ObjcMethodDefinition_id

@glean.PredicateAnnotation{
  name="cxx1.ObjcMethodDefinition";
  version=1;
}
union ObjcMethodDefinition {
  1: ObjcMethodDefinition_id id (hs.strict);
  2: ObjcMethodDeclaration key;
  3: builtin.Unit get;
} (hs.prefix = "ObjcMethodDefinition_with_")

typedef glean.Id ObjcInterfaceToImplementation_id

@glean.PredicateAnnotation{
  name="cxx1.ObjcInterfaceToImplementation";
  version=1;
}
union ObjcInterfaceToImplementation {
  1: ObjcInterfaceToImplementation_id id (hs.strict);
  2: ObjcInterfaceToImplementation_key key;
  3: builtin.Unit get;
} (hs.prefix = "ObjcInterfaceToImplementation_with_")

typedef glean.Id ObjcImplements_id

@glean.PredicateAnnotation{
  name="cxx1.ObjcImplements";
  version=1;
}
union ObjcImplements {
  1: ObjcImplements_id id (hs.strict);
  2: ObjcImplements_key key;
  3: builtin.Unit get;
} (hs.prefix = "ObjcImplements_with_")

typedef glean.Id ObjcContainerInheritance_id

@glean.PredicateAnnotation{
  name="cxx1.ObjcContainerInheritance";
  version=4;
}
union ObjcContainerInheritance {
  1: ObjcContainerInheritance_id id (hs.strict);
  2: ObjcContainerInheritance_key key;
  3: builtin.Unit get;
} (hs.prefix = "ObjcContainerInheritance_with_")

typedef glean.Id ObjcContainerDefinition_id

@glean.PredicateAnnotation{
  name="cxx1.ObjcContainerDefinition";
  version=2;
}
union ObjcContainerDefinition {
  1: ObjcContainerDefinition_id id (hs.strict);
  2: ObjcContainerDefinition_key key;
  3: builtin.Unit get;
} (hs.prefix = "ObjcContainerDefinition_with_")

typedef glean.Id ObjcContainerBase_id

@glean.PredicateAnnotation{
  name="cxx1.ObjcContainerBase";
  version=4;
}
union ObjcContainerBase {
  1: ObjcContainerBase_id id (hs.strict);
  2: ObjcContainerBase_key key;
  3: builtin.Unit get;
} (hs.prefix = "ObjcContainerBase_with_")

typedef glean.Id ObjcContainerDeclaration_id

@glean.PredicateAnnotation{
  name="cxx1.ObjcContainerDeclaration";
  version=1;
}
union ObjcContainerDeclaration {
  1: ObjcContainerDeclaration_id id (hs.strict);
  2: ObjcContainerDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "ObjcContainerDeclaration_with_")

typedef glean.Id ObjcMethodDeclaration_id

@glean.PredicateAnnotation{
  name="cxx1.ObjcMethodDeclaration";
  version=1;
}
union ObjcMethodDeclaration {
  1: ObjcMethodDeclaration_id id (hs.strict);
  2: ObjcMethodDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "ObjcMethodDeclaration_with_")

typedef glean.Id ObjcPropertyDeclaration_id

@glean.PredicateAnnotation{
  name="cxx1.ObjcPropertyDeclaration";
  version=1;
}
union ObjcPropertyDeclaration {
  1: ObjcPropertyDeclaration_id id (hs.strict);
  2: ObjcPropertyDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "ObjcPropertyDeclaration_with_")

typedef glean.Id NamespaceQName_id

@glean.PredicateAnnotation{
  name="cxx1.NamespaceQName";
  version=1;
}
union NamespaceQName {
  1: NamespaceQName_id id (hs.strict);
  2: NamespaceQName_key key;
  3: builtin.Unit get;
} (hs.prefix = "NamespaceQName_with_")

typedef glean.Id NamespaceDefinition_id

@glean.PredicateAnnotation{
  name="cxx1.NamespaceDefinition";
  version=2;
}
union NamespaceDefinition {
  1: NamespaceDefinition_id id (hs.strict);
  2: NamespaceDefinition_key key;
  3: builtin.Unit get;
} (hs.prefix = "NamespaceDefinition_with_")

typedef glean.Id NamespaceDeclaration_id

@glean.PredicateAnnotation{
  name="cxx1.NamespaceDeclaration";
  version=1;
}
union NamespaceDeclaration {
  1: NamespaceDeclaration_id id (hs.strict);
  2: NamespaceDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "NamespaceDeclaration_with_")

typedef glean.Id Name_id

@glean.PredicateAnnotation{
  name="cxx1.Name";
  version=1;
}
union Name {
  1: Name_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "Name_with_")

typedef glean.Id MethodOverrides_id

@glean.PredicateAnnotation{
  name="cxx1.MethodOverrides";
  version=1;
}
union MethodOverrides {
  1: MethodOverrides_id id (hs.strict);
  2: MethodOverrides_key key;
  3: builtin.Unit get;
} (hs.prefix = "MethodOverrides_with_")

typedef glean.Id MethodOverridden_id

@glean.PredicateAnnotation{
  name="cxx1.MethodOverridden";
  version=4;
}
union MethodOverridden {
  1: MethodOverridden_id id (hs.strict);
  2: MethodOverridden_key key;
  3: builtin.Unit get;
} (hs.prefix = "MethodOverridden_with_")

typedef glean.Id PPTrace_id

@glean.PredicateAnnotation{
  name="cxx1.PPTrace";
  version=2;
}
union PPTrace {
  1: PPTrace_id id (hs.strict);
  2: PPTrace_key key;
  3: builtin.Unit get;
} (hs.prefix = "PPTrace_with_")

typedef glean.Id FunctionName_id

@glean.PredicateAnnotation{
  name="cxx1.FunctionName";
  version=1;
}
union FunctionName {
  1: FunctionName_id id (hs.strict);
  2: FunctionName_key key;
  3: builtin.Unit get;
} (hs.prefix = "FunctionName_with_")

typedef glean.Id FunctionDefinition_id

@glean.PredicateAnnotation{
  name="cxx1.FunctionDefinition";
  version=1;
}
union FunctionDefinition {
  1: FunctionDefinition_id id (hs.strict);
  2: FunctionDefinition_key key;
  3: builtin.Unit get;
} (hs.prefix = "FunctionDefinition_with_")

typedef glean.Id FunctionDeclaration_id

@glean.PredicateAnnotation{
  name="cxx1.FunctionDeclaration";
  version=1;
}
union FunctionDeclaration {
  1: FunctionDeclaration_id id (hs.strict);
  2: FunctionDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "FunctionDeclaration_with_")

typedef glean.Id FunctionAttribute_id

@glean.PredicateAnnotation{
  name="cxx1.FunctionAttribute";
  version=4;
}
union FunctionAttribute {
  1: FunctionAttribute_id id (hs.strict);
  2: FunctionAttribute_key key;
  3: builtin.Unit get;
} (hs.prefix = "FunctionAttribute_with_")

typedef glean.Id VariableDeclaration_id

@glean.PredicateAnnotation{
  name="cxx1.VariableDeclaration";
  version=2;
}
union VariableDeclaration {
  1: VariableDeclaration_id id (hs.strict);
  2: VariableDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "VariableDeclaration_with_")

typedef glean.Id EnumeratorInEnum_id

@glean.PredicateAnnotation{
  name="cxx1.EnumeratorInEnum";
  version=4;
}
union EnumeratorInEnum {
  1: EnumeratorInEnum_id id (hs.strict);
  2: EnumeratorInEnum_key key;
  3: builtin.Unit get;
} (hs.prefix = "EnumeratorInEnum_with_")

typedef glean.Id Enumerator_id

@glean.PredicateAnnotation{
  name="cxx1.Enumerator";
  version=1;
}
union Enumerator {
  1: Enumerator_id id (hs.strict);
  2: Enumerator_key key;
  3: builtin.Unit get;
} (hs.prefix = "Enumerator_with_")

typedef glean.Id EnumDefinition_id

@glean.PredicateAnnotation{
  name="cxx1.EnumDefinition";
  version=1;
}
union EnumDefinition {
  1: EnumDefinition_id id (hs.strict);
  2: EnumDefinition_key key;
  3: builtin.Unit get;
} (hs.prefix = "EnumDefinition_with_")

typedef glean.Id EnumDeclaration_id

@glean.PredicateAnnotation{
  name="cxx1.EnumDeclaration";
  version=1;
}
union EnumDeclaration {
  1: EnumDeclaration_id id (hs.strict);
  2: EnumDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "EnumDeclaration_with_")

typedef glean.Id DeclarationComment_id

@glean.PredicateAnnotation{
  name="cxx1.DeclarationComment";
  version=1;
}
union DeclarationComment {
  1: DeclarationComment_id id (hs.strict);
  2: DeclarationComment_key key;
  3: builtin.Unit get;
} (hs.prefix = "DeclarationComment_with_")

typedef glean.Id DeclarationInTrace_id

@glean.PredicateAnnotation{
  name="cxx1.DeclarationInTrace";
  version=4;
}
union DeclarationInTrace {
  1: DeclarationInTrace_id id (hs.strict);
  2: DeclarationInTrace_key key;
  3: builtin.Unit get;
} (hs.prefix = "DeclarationInTrace_with_")

typedef glean.Id DeclarationSources_id

@glean.PredicateAnnotation{
  name="cxx1.DeclarationSources";
  version=1;
}
union DeclarationSources {
  1: DeclarationSources_id id (hs.strict);
  2: DeclarationSources_key key;
  3: builtin.Unit get;
} (hs.prefix = "DeclarationSources_with_")

typedef glean.Id DeclarationSrcRange_id

@glean.PredicateAnnotation{
  name="cxx1.DeclarationSrcRange";
  version=4;
}
union DeclarationSrcRange {
  1: DeclarationSrcRange_id id (hs.strict);
  2: DeclarationSrcRange_key key;
  3: builtin.Unit get;
} (hs.prefix = "DeclarationSrcRange_with_")

typedef glean.Id DeclarationTargets_id

@glean.PredicateAnnotation{
  name="cxx1.DeclarationTargets";
  version=1;
}
union DeclarationTargets {
  1: DeclarationTargets_id id (hs.strict);
  2: DeclarationTargets_key key;
  3: builtin.Unit get;
} (hs.prefix = "DeclarationTargets_with_")

typedef glean.Id Declarations_id

@glean.PredicateAnnotation{
  name="cxx1.Declarations";
  version=2;
}
union Declarations {
  1: Declarations_id id (hs.strict);
  2: Declarations_array key;
  3: builtin.Unit get;
} (hs.prefix = "Declarations_with_")

typedef glean.Id Same_id

@glean.PredicateAnnotation{
  name="cxx1.Same";
  version=2;
}
union Same {
  1: Same_id id (hs.strict);
  2: Same_key key;
  3: builtin.Unit get;
} (hs.prefix = "Same_with_")

typedef glean.Id FileXRefs_id

@glean.PredicateAnnotation{
  name="cxx1.FileXRefs";
  version=2;
}
union FileXRefs {
  1: FileXRefs_id id (hs.strict);
  2: FileXRefs_key key;
  3: builtin.Unit get;
} (hs.prefix = "FileXRefs_with_")

typedef glean.Id FileXRefMap_id

@glean.PredicateAnnotation{
  name="cxx1.FileXRefMap";
  version=2;
}
union FileXRefMap {
  1: FileXRefMap_id id (hs.strict);
  2: FileXRefMap_key key;
  3: builtin.Unit get;
} (hs.prefix = "FileXRefMap_with_")

typedef glean.Id TargetUses_id

@glean.PredicateAnnotation{
  name="cxx1.TargetUses";
  version=2;
}
union TargetUses {
  1: TargetUses_id id (hs.strict);
  2: TargetUses_key key;
  3: builtin.Unit get;
} (hs.prefix = "TargetUses_with_")

typedef glean.Id XRefIndirectTarget_id

@glean.PredicateAnnotation{
  name="cxx1.XRefIndirectTarget";
  version=2;
}
union XRefIndirectTarget {
  1: XRefIndirectTarget_id id (hs.strict);
  2: XRefIndirectTarget_key key;
  3: builtin.Unit get;
} (hs.prefix = "XRefIndirectTarget_with_")

typedef glean.Id DeclToFamily_id

@glean.PredicateAnnotation{
  name="cxx1.DeclToFamily";
  version=2;
}
union DeclToFamily {
  1: DeclToFamily_id id (hs.strict);
  2: DeclToFamily_key key;
  3: builtin.Unit get;
} (hs.prefix = "DeclToFamily_with_")

typedef glean.Id DeclInRecord_id

@glean.PredicateAnnotation{
  name="cxx1.DeclInRecord";
  version=4;
}
union DeclInRecord {
  1: DeclInRecord_id id (hs.strict);
  2: DeclInRecord_key key;
  3: builtin.Unit get;
} (hs.prefix = "DeclInRecord_with_")

typedef glean.Id DeclInObjcContainer_id

@glean.PredicateAnnotation{
  name="cxx1.DeclInObjcContainer";
  version=4;
}
union DeclInObjcContainer {
  1: DeclInObjcContainer_id id (hs.strict);
  2: DeclInObjcContainer_key key;
  3: builtin.Unit get;
} (hs.prefix = "DeclInObjcContainer_with_")

typedef glean.Id DeclImpliesRecord_id

@glean.PredicateAnnotation{
  name="cxx1.DeclImpliesRecord";
  version=4;
}
union DeclImpliesRecord {
  1: DeclImpliesRecord_id id (hs.strict);
  2: DeclImpliesRecord_key key;
  3: builtin.Unit get;
} (hs.prefix = "DeclImpliesRecord_with_")

typedef glean.Id DeclFamilyOf_id

@glean.PredicateAnnotation{
  name="cxx1.DeclFamilyOf";
  version=4;
}
union DeclFamilyOf {
  1: DeclFamilyOf_id id (hs.strict);
  2: DeclFamilyOf_key key;
  3: builtin.Unit get;
} (hs.prefix = "DeclFamilyOf_with_")

typedef glean.Id DeclFamily_id

@glean.PredicateAnnotation{
  name="cxx1.DeclFamily";
  version=2;
}
union DeclFamily {
  1: DeclFamily_id id (hs.strict);
  2: DeclFamily_array key;
  3: builtin.Unit get;
} (hs.prefix = "DeclFamily_with_")

typedef glean.Id DeclByName_id

@glean.PredicateAnnotation{
  name="cxx1.DeclByName";
  version=4;
}
union DeclByName {
  1: DeclByName_id id (hs.strict);
  2: DeclByName_key key;
  3: builtin.Unit get;
} (hs.prefix = "DeclByName_with_")

typedef glean.Id Attribute_id

@glean.PredicateAnnotation{
  name="cxx1.Attribute";
  version=4;
}
union Attribute {
  1: Attribute_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "Attribute_with_")

typedef glean.Id RecordDefinition_id

@glean.PredicateAnnotation{
  name="cxx1.RecordDefinition";
  version=2;
}
union RecordDefinition {
  1: RecordDefinition_id id (hs.strict);
  2: RecordDefinition_key key;
  3: builtin.Unit get;
} (hs.prefix = "RecordDefinition_with_")

typedef glean.Id FunctionQName_id

@glean.PredicateAnnotation{
  name="cxx1.FunctionQName";
  version=1;
}
union FunctionQName {
  1: FunctionQName_id id (hs.strict);
  2: FunctionQName_key key;
  3: builtin.Unit get;
} (hs.prefix = "FunctionQName_with_")

typedef glean.Id QName_id

@glean.PredicateAnnotation{
  name="cxx1.QName";
  version=1;
}
union QName {
  1: QName_id id (hs.strict);
  2: QName_key key;
  3: builtin.Unit get;
} (hs.prefix = "QName_with_")

struct XRefVia {
  1: optional UsingDeclaration usingDeclaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional UsingDirective usingDirective (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional pp1.Use macro (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: bool any = false;
}

struct UsingDirective_key {
  1: optional QName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.Range source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct UsingDeclaration_key {
  1: optional FunctionQName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.Range source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

enum TypeAliasKind {
  Typedef = 0,
  Using = 1
} (hs.nounknown)

struct TypeAliasDeclaration_key {
  1: optional QName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_ type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional TypeAliasKind kind;
  4: optional src.Range source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union TranslationUnitXRefs_xrefs_array {
  1: FileXRefs every;
  2: list<FileXRefs> exact;
}

struct TranslationUnitXRefs_key {
  1: optional buck.TranslationUnit tunit (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional TranslationUnitXRefs_xrefs_array xrefs (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct TranslationUnitTrace_key {
  1: optional buck.TranslationUnit tunit (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Trace trace (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Trace_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Declarations declarations (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional PPTrace preprocessor (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

enum RefQualifier {
  None_ = 0,
  LValue = 1,
  RValue = 2
} (hs.nounknown)

struct RecordKind {
  1: optional builtin.Unit struct_ (java.swift.name = "struct_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional builtin.Unit class_ (java.swift.name = "class_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional builtin.Unit union_ (java.swift.name = "union_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: bool any = false;
}

struct RecordDerived_key {
  1: optional RecordDeclaration base (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional RecordDeclaration derived (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct RecordDeclaration_key {
  1: optional QName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional RecordKind kind (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.Range source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Parameter {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_ type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union Signature_parameters_array {
  1: Parameter every;
  2: list<Parameter> exact;
}

struct Signature_key {
  1: optional Type_ returns (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Signature_parameters_array parameters (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef string Operator

union ObjcSelector_array {
  1: string every;
  2: list<string> exact;
}

enum ObjcPropertyKind {
  Synthesize = 0,
  Dynamic = 1
} (hs.nounknown)

struct ObjcPropertyImplementation_ivar {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Name just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct ObjcPropertyImplementation_key {
  1: optional ObjcPropertyDeclaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ObjcPropertyKind kind;
  3: optional ObjcPropertyImplementation_ivar ivar (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional src.Range source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct ObjcPropertyIVar_key {
  1: optional ObjcPropertyDeclaration property (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional VariableDeclaration ivar (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct ObjcInterfaceToImplementation_key {
  1: optional ObjcContainerDeclaration interface_ (java.swift.name = "interface_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ObjcContainerDeclaration implementation (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct ObjcImplements_key {
  1: optional ObjcContainerDeclaration implementation (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ObjcContainerDeclaration interface_ (java.swift.name = "interface_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct ObjcIVar_bitsize {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional glean.Nat just;
  3: bool any = false;
}

struct ObjcIVar {
  1: optional bool synthesize;
  2: optional ObjcIVar_bitsize bitsize;
}

struct ObjcContainerInheritance_key {
  1: optional ObjcContainerDeclaration base (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ObjcContainerDeclaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union ObjcContainerDefinition_protocols_array {
  1: ObjcContainerDeclaration every;
  2: list<ObjcContainerDeclaration> exact;
}

struct ObjcContainerDefinition_key {
  1: optional ObjcContainerDeclaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ObjcContainerDefinition_protocols_array protocols (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional Declarations members (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct ObjcContainerBase_key {
  1: optional ObjcContainerDeclaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ObjcContainerDeclaration base (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct ObjcCategoryId {
  1: optional Name className (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Name categoryName (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct ObjcContainerId {
  1: optional Name protocol (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Name interface_ (java.swift.name = "interface_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional ObjcCategoryId categoryInterface (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional Name extensionInterface (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional Name implementation (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: optional ObjcCategoryId categoryImplementation (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  7: bool any = false;
}

struct ObjcContainerDeclaration_key {
  1: optional ObjcContainerId id (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.Range source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct ObjcMethodDeclaration_key {
  1: optional ObjcSelector selector (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ObjcContainerId container (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional Signature signature (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional bool isInstance;
  5: optional bool isOptional;
  6: optional bool isAccessor;
  7: optional src.Range source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct ObjcPropertyDeclaration_key {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ObjcContainerId container (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional Type_ type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional bool isInstance;
  5: optional bool isOptional;
  6: optional bool isReadOnly;
  7: optional bool isAtomic;
  8: optional src.Range source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct NamespaceQName_name {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Name just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct NamespaceQName_parent {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional NamespaceQName just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct NamespaceQName_key {
  1: optional NamespaceQName_name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional NamespaceQName_parent parent (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct NamespaceDefinition_key {
  1: optional NamespaceDeclaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Declarations members (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct NamespaceDeclaration_key {
  1: optional NamespaceQName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.Range source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct MethodSignature {
  1: optional bool isVirtual;
  2: optional bool isConst;
  3: optional bool isVolatile;
  4: optional RefQualifier refQualifier;
}

struct MethodOverrides_key {
  1: optional FunctionDeclaration derived (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional FunctionDeclaration base (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct MethodOverridden_key {
  1: optional FunctionDeclaration base (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional FunctionDeclaration derived (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef string LiteralOperator

struct IncludeTrace_trace {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Trace just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct IncludeTrace {
  1: optional pp1.Include include_ (java.swift.name = "include_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional IncludeTrace_trace trace (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct PPEvent {
  1: optional IncludeTrace include_ (java.swift.name = "include_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional pp1.Define define (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional pp1.Undef undef (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional pp1.Use use (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: bool any = false;
}

union PPTrace_events_array {
  1: PPEvent every;
  2: list<PPEvent> exact;
}

struct PPTrace_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional PPTrace_events_array events (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
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
  1: optional GlobalVariableKind kind;
  2: optional GlobalVariableAttribute attribute;
  3: optional bool definition;
}

struct FunctionName_key {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Operator operator_ (java.swift.name = "operator_");
  3: optional LiteralOperator literalOperator;
  4: optional builtin.Unit constructor (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional builtin.Unit destructor (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: optional Type_ conversionOperator (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  7: bool any = false;
}

struct FunctionDefinition_key {
  1: optional FunctionDeclaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional bool isInline;
}

struct FunctionDeclaration_method {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional MethodSignature just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct FunctionDeclaration_key {
  1: optional FunctionQName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Signature signature (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional FunctionDeclaration_method method (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional src.Range source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FunctionAttribute_key {
  1: optional Attribute attr (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional FunctionDeclaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Field_bitsize {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional glean.Nat just;
  3: bool any = false;
}

struct Field {
  1: optional bool mutable_ (java.swift.name = "mutable_");
  2: optional Field_bitsize bitsize;
}

struct VariableKind {
  1: optional GlobalVariable global_ (java.swift.name = "global_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Field field (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional ObjcIVar ivar (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: bool any = false;
}

struct VariableDeclaration_key {
  1: optional QName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_ type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional VariableKind kind (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional src.Range source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct EnumeratorInEnum_key {
  1: optional Enumerator enumerator (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional EnumDefinition enum_ (java.swift.name = "enum_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Enumerator_key {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional EnumDeclaration enumeration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.Range source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union EnumDefinition_enumerators_array {
  1: Enumerator every;
  2: list<Enumerator> exact;
}

struct EnumDefinition_key {
  1: optional EnumDeclaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional EnumDefinition_enumerators_array enumerators (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct EnumDeclaration_type {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_ just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct EnumDeclaration_key {
  1: optional QName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional bool isScoped;
  3: optional EnumDeclaration_type type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional src.Range source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Declaration {
  1: optional NamespaceDeclaration namespace_ (java.swift.name = "namespace_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional UsingDeclaration usingDeclaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional UsingDirective usingDirective (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional RecordDeclaration record_ (java.swift.name = "record_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional EnumDeclaration enum_ (java.swift.name = "enum_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: optional FunctionDeclaration function_ (java.swift.name = "function_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  7: optional VariableDeclaration variable (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  8: optional ObjcContainerDeclaration objcContainer (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  9: optional ObjcMethodDeclaration objcMethod (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  10: optional ObjcPropertyDeclaration objcProperty (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  11: optional TypeAliasDeclaration typeAlias (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  12: bool any = false;
}

struct DeclarationComment_key {
  1: optional Declaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.ByteSpan span (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct DeclarationInTrace_key {
  1: optional Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Trace trace (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union DeclarationSources_sources_array {
  1: Declaration every;
  2: list<Declaration> exact;
}

struct DeclarationSources_key {
  1: optional Declaration target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional DeclarationSources_sources_array sources (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct DeclarationSrcRange_key {
  1: optional Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.Range source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union DeclarationTargets_targets_array {
  1: Declaration every;
  2: list<Declaration> exact;
}

struct DeclarationTargets_key {
  1: optional Declaration source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional DeclarationTargets_targets_array targets (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union Declarations_array {
  1: Declaration every;
  2: list<Declaration> exact;
}

struct Same_key {
  1: optional Declaration declaration1 (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Declaration declaration2 (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct XRefTarget {
  1: optional Declaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Enumerator enumerator (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional ObjcSelector objcSelector (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional src.Loc unknown (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional XRefIndirectTarget indirect (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: bool any = false;
}

union FileXRefs_externals_array {
  1: XRefTarget every;
  2: list<XRefTarget> exact;
}

struct FileXRefs_key {
  1: optional FileXRefMap xmap (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional FileXRefs_externals_array externals (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FixedXRef {
  1: optional XRefTarget target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.ByteSpans ranges (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union FileXRefMap_fixed_array {
  1: FixedXRef every;
  2: list<FixedXRef> exact;
}

union FileXRefMap_variable_array {
  1: src.ByteSpans every;
  2: list<src.ByteSpans> exact;
}

struct FileXRefMap_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional FileXRefMap_fixed_array fixed (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional FileXRefMap_variable_array variable (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct TargetUses_key {
  1: optional XRefTarget target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.ByteSpans uses (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct XRefIndirectTarget_key {
  1: optional XRefVia via (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional XRefTarget target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct DeclToFamily_key {
  1: optional Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional DeclFamily family (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
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
  1: optional Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional RecordDefinition record (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct DeclInObjcContainer_key {
  1: optional Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ObjcContainerDefinition record (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct DeclImpliesRecord_key {
  1: optional Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional RecordDefinition record (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct DeclIdent {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional pp1.Macro macro (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional ObjcSelector selector (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: bool any = false;
}

struct DeclFamilyOf_key {
  1: optional Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Declaration family (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union DeclFamily_array {
  1: Declaration every;
  2: list<Declaration> exact;
}

struct DeclByName_key {
  1: optional string name_lowercase;
  2: optional DeclKind kind;
  3: optional DeclIdent ident (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

enum Access {
  Public = 0,
  Protected = 1,
  Private = 2
} (hs.nounknown)

struct RecordBase {
  1: optional RecordDeclaration base (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Access access;
  3: optional bool isVirtual;
}

union RecordDefinition_bases_array {
  1: RecordBase every;
  2: list<RecordBase> exact;
}

struct RecordDefinition_key {
  1: optional RecordDeclaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional RecordDefinition_bases_array bases (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional Declarations members (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Scope_recordWithAccess_ {
  1: optional QName record (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Access access;
}

struct Scope {
  1: optional builtin.Unit global_ (java.swift.name = "global_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional NamespaceQName namespace_ (java.swift.name = "namespace_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional Scope_recordWithAccess_ recordWithAccess (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional FunctionQName local (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: bool any = false;
}

struct FunctionQName_key {
  1: optional FunctionName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Scope scope (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct QName_key {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Scope scope (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}
