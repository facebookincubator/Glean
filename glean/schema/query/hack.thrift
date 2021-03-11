// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/query/builtin.thrift"
include "glean/schema/query/src.thrift"

namespace cpp2 facebook.glean.schema.query.hack
namespace hs Glean.Schema.Query
namespace php glean_schema_query_hack
namespace py glean.schema.query.hack
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.hack
namespace rust glean_schema_query_hack

hs_include "glean/schema/query/hack_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "ClassDeclaration": 4,
  "QName": 4,
  "TargetUses": 4,
  "filename": 1,
  "TargetUsesAbs": 4,
  "TraitDefinition": 4,
  "DeclarationName": 4,
  "FunctionDefinition": 4,
  "PropertyDeclaration": 4,
  "PropertyDefinition": 4,
  "ContainerChild": 4,
  "symbolNamespace": 1,
  "UserAttribute": 4,
  "DeclarationSource": 4,
  "NamespaceQName": 4,
  "InterfaceDeclaration": 4,
  "kind": 1,
  "InterfaceDefinition": 4,
  "Comment": 4,
  "identifier": 1,
  "NamespaceMember": 4,
  "ContainerParent": 4,
  "GlobalConstDefinition": 4,
  "EnumDefinition": 4,
  "symbol": 1,
  "ClassConstDefinition": 4,
  "GlobalConstDeclaration": 4,
  "DeclarationTarget": 4,
  "FileXRefs": 4,
  "DeclarationSpan": 4,
  "Enumerator": 4,
  "Signature": 4,
  "TypedefDeclaration": 4,
  "Name": 4,
  "MethodDeclaration": 4,
  "TraitDeclaration": 4,
  "FunctionDeclaration": 4,
  "MethodDefinition": 4,
  "FileDeclarations": 4,
  "DeclarationLocation": 4,
  "TypeConstDeclaration": 4,
  "NameLowerCase": 4,
  "TypeConstDefinition": 4,
  "Type": 4,
  "DeclarationComment": 4,
  "ClassConstDeclaration": 4,
  "EnumDeclaration": 4,
  "NamespaceDeclaration": 4,
  "ClassDefinition": 4,
}


typedef glean.Id symbolNamespace_id

@glean.PredicateAnnotation{
  name="hack.symbolNamespace";
  version=1;
}
union symbolNamespace {
  1: symbolNamespace_id id (hs.strict);
  2: symbolNamespace_key key;
  3: builtin.Unit get;
} (hs.prefix = "symbolNamespace_with_")

typedef glean.Id kind_id

@glean.PredicateAnnotation{
  name="hack.kind";
  version=1;
}
union kind {
  1: kind_id id (hs.strict);
  2: kind_key key;
  3: builtin.Unit get;
} (hs.prefix = "kind_with_")

typedef glean.Id identifier_id

@glean.PredicateAnnotation{
  name="hack.identifier";
  version=1;
}
union identifier {
  1: identifier_id id (hs.strict);
  2: identifier_key key;
  3: builtin.Unit get;
} (hs.prefix = "identifier_with_")

typedef glean.Id filename_id

@glean.PredicateAnnotation{
  name="hack.filename";
  version=1;
}
union filename {
  1: filename_id id (hs.strict);
  2: filename_key key;
  3: builtin.Unit get;
} (hs.prefix = "filename_with_")

typedef glean.Id symbol_id

@glean.PredicateAnnotation{
  name="hack.symbol";
  version=1;
}
union symbol {
  1: symbol_id id (hs.strict);
  2: symbol_key key;
  3: builtin.Unit get;
} (hs.prefix = "symbol_with_")

typedef glean.Id UserAttribute_id

@glean.PredicateAnnotation{
  name="hack.UserAttribute";
  version=4;
}
union UserAttribute {
  1: UserAttribute_id id (hs.strict);
  2: UserAttribute_key key;
  3: builtin.Unit get;
} (hs.prefix = "UserAttribute_with_")

typedef glean.Id TypeConstDefinition_id

@glean.PredicateAnnotation{
  name="hack.TypeConstDefinition";
  version=4;
}
union TypeConstDefinition {
  1: TypeConstDefinition_id id (hs.strict);
  2: TypeConstDefinition_key key;
  3: builtin.Unit get;
} (hs.prefix = "TypeConstDefinition_with_")

typedef glean.Id Type_id

@glean.PredicateAnnotation{
  name="hack.Type";
  version=4;
}
union Type_ {
  1: Type_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "Type__with_")

typedef glean.Id TraitDeclaration_id

@glean.PredicateAnnotation{
  name="hack.TraitDeclaration";
  version=4;
}
union TraitDeclaration {
  1: TraitDeclaration_id id (hs.strict);
  2: TraitDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "TraitDeclaration_with_")

typedef glean.Id QName_id

@glean.PredicateAnnotation{
  name="hack.QName";
  version=4;
}
union QName {
  1: QName_id id (hs.strict);
  2: QName_key key;
  3: builtin.Unit get;
} (hs.prefix = "QName_with_")

typedef glean.Id PropertyDefinition_id

@glean.PredicateAnnotation{
  name="hack.PropertyDefinition";
  version=4;
}
union PropertyDefinition {
  1: PropertyDefinition_id id (hs.strict);
  2: PropertyDefinition_key key;
  3: builtin.Unit get;
} (hs.prefix = "PropertyDefinition_with_")

typedef glean.Id Signature_id

@glean.PredicateAnnotation{
  name="hack.Signature";
  version=4;
}
union Signature {
  1: Signature_id id (hs.strict);
  2: Signature_key key;
  3: builtin.Unit get;
} (hs.prefix = "Signature_with_")

typedef glean.Id NamespaceQName_id

@glean.PredicateAnnotation{
  name="hack.NamespaceQName";
  version=4;
}
union NamespaceQName {
  1: NamespaceQName_id id (hs.strict);
  2: NamespaceQName_key key;
  3: builtin.Unit get;
} (hs.prefix = "NamespaceQName_with_")

typedef glean.Id NamespaceDeclaration_id

@glean.PredicateAnnotation{
  name="hack.NamespaceDeclaration";
  version=4;
}
union NamespaceDeclaration {
  1: NamespaceDeclaration_id id (hs.strict);
  2: NamespaceDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "NamespaceDeclaration_with_")

typedef glean.Id NameLowerCase_id

@glean.PredicateAnnotation{
  name="hack.NameLowerCase";
  version=4;
}
union NameLowerCase {
  1: NameLowerCase_id id (hs.strict);
  2: NameLowerCase_key key;
  3: builtin.Unit get;
} (hs.prefix = "NameLowerCase_with_")

typedef glean.Id Name_id

@glean.PredicateAnnotation{
  name="hack.Name";
  version=4;
}
union Name {
  1: Name_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "Name_with_")

typedef glean.Id InterfaceDeclaration_id

@glean.PredicateAnnotation{
  name="hack.InterfaceDeclaration";
  version=4;
}
union InterfaceDeclaration {
  1: InterfaceDeclaration_id id (hs.strict);
  2: InterfaceDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "InterfaceDeclaration_with_")

typedef glean.Id GlobalConstDefinition_id

@glean.PredicateAnnotation{
  name="hack.GlobalConstDefinition";
  version=4;
}
union GlobalConstDefinition {
  1: GlobalConstDefinition_id id (hs.strict);
  2: GlobalConstDefinition_key key;
  3: builtin.Unit get;
} (hs.prefix = "GlobalConstDefinition_with_")

typedef glean.Id GlobalConstDeclaration_id

@glean.PredicateAnnotation{
  name="hack.GlobalConstDeclaration";
  version=4;
}
union GlobalConstDeclaration {
  1: GlobalConstDeclaration_id id (hs.strict);
  2: GlobalConstDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "GlobalConstDeclaration_with_")

typedef glean.Id FunctionDeclaration_id

@glean.PredicateAnnotation{
  name="hack.FunctionDeclaration";
  version=4;
}
union FunctionDeclaration {
  1: FunctionDeclaration_id id (hs.strict);
  2: FunctionDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "FunctionDeclaration_with_")

typedef glean.Id Enumerator_id

@glean.PredicateAnnotation{
  name="hack.Enumerator";
  version=4;
}
union Enumerator {
  1: Enumerator_id id (hs.strict);
  2: Enumerator_key key;
  3: builtin.Unit get;
} (hs.prefix = "Enumerator_with_")

typedef glean.Id EnumDefinition_id

@glean.PredicateAnnotation{
  name="hack.EnumDefinition";
  version=4;
}
union EnumDefinition {
  1: EnumDefinition_id id (hs.strict);
  2: EnumDefinition_key key;
  3: builtin.Unit get;
} (hs.prefix = "EnumDefinition_with_")

typedef glean.Id EnumDeclaration_id

@glean.PredicateAnnotation{
  name="hack.EnumDeclaration";
  version=4;
}
union EnumDeclaration {
  1: EnumDeclaration_id id (hs.strict);
  2: EnumDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "EnumDeclaration_with_")

typedef glean.Id ContainerParent_id

@glean.PredicateAnnotation{
  name="hack.ContainerParent";
  version=4;
}
union ContainerParent {
  1: ContainerParent_id id (hs.strict);
  2: ContainerParent_key key;
  3: builtin.Unit get;
} (hs.prefix = "ContainerParent_with_")

typedef glean.Id DeclarationComment_id

@glean.PredicateAnnotation{
  name="hack.DeclarationComment";
  version=4;
}
union DeclarationComment {
  1: DeclarationComment_id id (hs.strict);
  2: DeclarationComment_key key;
  3: builtin.Unit get;
} (hs.prefix = "DeclarationComment_with_")

typedef glean.Id DeclarationLocation_id

@glean.PredicateAnnotation{
  name="hack.DeclarationLocation";
  version=4;
}
union DeclarationLocation {
  1: DeclarationLocation_id id (hs.strict);
  2: DeclarationLocation_key key;
  3: builtin.Unit get;
} (hs.prefix = "DeclarationLocation_with_")

typedef glean.Id DeclarationName_id

@glean.PredicateAnnotation{
  name="hack.DeclarationName";
  version=4;
}
union DeclarationName {
  1: DeclarationName_id id (hs.strict);
  2: DeclarationName_key key;
  3: builtin.Unit get;
} (hs.prefix = "DeclarationName_with_")

typedef glean.Id DeclarationSource_id

@glean.PredicateAnnotation{
  name="hack.DeclarationSource";
  version=4;
}
union DeclarationSource {
  1: DeclarationSource_id id (hs.strict);
  2: DeclarationSource_key key;
  3: builtin.Unit get;
} (hs.prefix = "DeclarationSource_with_")

typedef glean.Id DeclarationSpan_id

@glean.PredicateAnnotation{
  name="hack.DeclarationSpan";
  version=4;
}
union DeclarationSpan {
  1: DeclarationSpan_id id (hs.strict);
  2: DeclarationSpan_key key;
  3: builtin.Unit get;
} (hs.prefix = "DeclarationSpan_with_")

typedef glean.Id DeclarationTarget_id

@glean.PredicateAnnotation{
  name="hack.DeclarationTarget";
  version=4;
}
union DeclarationTarget {
  1: DeclarationTarget_id id (hs.strict);
  2: DeclarationTarget_key key;
  3: builtin.Unit get;
} (hs.prefix = "DeclarationTarget_with_")

typedef glean.Id FileDeclarations_id

@glean.PredicateAnnotation{
  name="hack.FileDeclarations";
  version=4;
}
union FileDeclarations {
  1: FileDeclarations_id id (hs.strict);
  2: FileDeclarations_key key;
  3: builtin.Unit get;
} (hs.prefix = "FileDeclarations_with_")

typedef glean.Id NamespaceMember_id

@glean.PredicateAnnotation{
  name="hack.NamespaceMember";
  version=4;
}
union NamespaceMember {
  1: NamespaceMember_id id (hs.strict);
  2: NamespaceMember_key key;
  3: builtin.Unit get;
} (hs.prefix = "NamespaceMember_with_")

typedef glean.Id TargetUses_id

@glean.PredicateAnnotation{
  name="hack.TargetUses";
  version=4;
}
union TargetUses {
  1: TargetUses_id id (hs.strict);
  2: TargetUses_key key;
  3: builtin.Unit get;
} (hs.prefix = "TargetUses_with_")

typedef glean.Id TargetUsesAbs_id

@glean.PredicateAnnotation{
  name="hack.TargetUsesAbs";
  version=4;
}
union TargetUsesAbs {
  1: TargetUsesAbs_id id (hs.strict);
  2: TargetUsesAbs_key key;
  3: builtin.Unit get;
} (hs.prefix = "TargetUsesAbs_with_")

typedef glean.Id FileXRefs_id

@glean.PredicateAnnotation{
  name="hack.FileXRefs";
  version=4;
}
union FileXRefs {
  1: FileXRefs_id id (hs.strict);
  2: FileXRefs_key key;
  3: builtin.Unit get;
} (hs.prefix = "FileXRefs_with_")

typedef glean.Id MethodDeclaration_id

@glean.PredicateAnnotation{
  name="hack.MethodDeclaration";
  version=4;
}
union MethodDeclaration {
  1: MethodDeclaration_id id (hs.strict);
  2: MethodDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "MethodDeclaration_with_")

typedef glean.Id PropertyDeclaration_id

@glean.PredicateAnnotation{
  name="hack.PropertyDeclaration";
  version=4;
}
union PropertyDeclaration {
  1: PropertyDeclaration_id id (hs.strict);
  2: PropertyDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "PropertyDeclaration_with_")

typedef glean.Id TypeConstDeclaration_id

@glean.PredicateAnnotation{
  name="hack.TypeConstDeclaration";
  version=4;
}
union TypeConstDeclaration {
  1: TypeConstDeclaration_id id (hs.strict);
  2: TypeConstDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "TypeConstDeclaration_with_")

typedef glean.Id ContainerChild_id

@glean.PredicateAnnotation{
  name="hack.ContainerChild";
  version=4;
}
union ContainerChild {
  1: ContainerChild_id id (hs.strict);
  2: ContainerChild_key key;
  3: builtin.Unit get;
} (hs.prefix = "ContainerChild_with_")

typedef glean.Id FunctionDefinition_id

@glean.PredicateAnnotation{
  name="hack.FunctionDefinition";
  version=4;
}
union FunctionDefinition {
  1: FunctionDefinition_id id (hs.strict);
  2: FunctionDefinition_key key;
  3: builtin.Unit get;
} (hs.prefix = "FunctionDefinition_with_")

typedef glean.Id InterfaceDefinition_id

@glean.PredicateAnnotation{
  name="hack.InterfaceDefinition";
  version=4;
}
union InterfaceDefinition {
  1: InterfaceDefinition_id id (hs.strict);
  2: InterfaceDefinition_key key;
  3: builtin.Unit get;
} (hs.prefix = "InterfaceDefinition_with_")

typedef glean.Id MethodDefinition_id

@glean.PredicateAnnotation{
  name="hack.MethodDefinition";
  version=4;
}
union MethodDefinition {
  1: MethodDefinition_id id (hs.strict);
  2: MethodDefinition_key key;
  3: builtin.Unit get;
} (hs.prefix = "MethodDefinition_with_")

typedef glean.Id TraitDefinition_id

@glean.PredicateAnnotation{
  name="hack.TraitDefinition";
  version=4;
}
union TraitDefinition {
  1: TraitDefinition_id id (hs.strict);
  2: TraitDefinition_key key;
  3: builtin.Unit get;
} (hs.prefix = "TraitDefinition_with_")

typedef glean.Id TypedefDeclaration_id

@glean.PredicateAnnotation{
  name="hack.TypedefDeclaration";
  version=4;
}
union TypedefDeclaration {
  1: TypedefDeclaration_id id (hs.strict);
  2: TypedefDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "TypedefDeclaration_with_")

typedef glean.Id Comment_id

@glean.PredicateAnnotation{
  name="hack.Comment";
  version=4;
}
union Comment {
  1: Comment_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "Comment_with_")

typedef glean.Id ClassDefinition_id

@glean.PredicateAnnotation{
  name="hack.ClassDefinition";
  version=4;
}
union ClassDefinition {
  1: ClassDefinition_id id (hs.strict);
  2: ClassDefinition_key key;
  3: builtin.Unit get;
} (hs.prefix = "ClassDefinition_with_")

typedef glean.Id ClassDeclaration_id

@glean.PredicateAnnotation{
  name="hack.ClassDeclaration";
  version=4;
}
union ClassDeclaration {
  1: ClassDeclaration_id id (hs.strict);
  2: ClassDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "ClassDeclaration_with_")

typedef glean.Id ClassConstDefinition_id

@glean.PredicateAnnotation{
  name="hack.ClassConstDefinition";
  version=4;
}
union ClassConstDefinition {
  1: ClassConstDefinition_id id (hs.strict);
  2: ClassConstDefinition_key key;
  3: builtin.Unit get;
} (hs.prefix = "ClassConstDefinition_with_")

typedef glean.Id ClassConstDeclaration_id

@glean.PredicateAnnotation{
  name="hack.ClassConstDeclaration";
  version=4;
}
union ClassConstDeclaration {
  1: ClassConstDeclaration_id id (hs.strict);
  2: ClassConstDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "ClassConstDeclaration_with_")

struct symbolNamespace_key {
  1: optional glean.Nat namespace_id;
  2: optional string namespace_name;
}

struct kind_key {
  1: optional glean.Nat id;
  2: optional string name;
}

struct identifier_key {
  1: optional glean.Nat kind;
  2: optional string name;
}

struct filename_key {
  1: optional string filename;
  2: optional string filehash_id;
}

struct context {
  1: optional bool acid;
  2: optional bool actype;
  3: optional bool acnew;
}

struct symbol_key {
  1: optional string name_lowercase;
  2: optional context valid (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional glean.Nat kind_id;
  4: optional glean.Nat ns_id;
  5: optional string filehash_id;
  6: optional bool is_abstract;
  7: optional bool is_final;
  8: optional string canonical_name;
}

enum Visibility {
  Private = 0,
  Protected = 1,
  Public = 2
} (hs.nounknown)

enum Variance {
  Contravariant = 0,
  Covariant = 1,
  Invariant = 2
} (hs.nounknown)

union UserAttribute_parameters_array {
  1: string every;
  2: list<string> exact;
}

struct UserAttribute_key {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional UserAttribute_parameters_array parameters;
}

enum TypeConstKind {
  Abstract = 0,
  Concrete = 1,
  PartiallyAbstract = 2
} (hs.nounknown)

struct TypeConstDefinition_type {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_ just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

union TypeConstDefinition_attributes_array {
  1: UserAttribute every;
  2: list<UserAttribute> exact;
}

struct TypeConstDefinition_key {
  1: optional TypeConstDeclaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional TypeConstDefinition_type type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional TypeConstKind kind;
  4: optional TypeConstDefinition_attributes_array attributes (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct TraitDeclaration_key {
  1: optional QName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

enum ReifyKind {
  Erased = 0,
  Reified = 1,
  SoftReified = 2
} (hs.nounknown)

struct QName_namespace_ {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional NamespaceQName just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct QName_key {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional QName_namespace_ namespace_ (java.swift.name = "namespace_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct PropertyDefinition_type {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_ just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

union PropertyDefinition_attributes_array {
  1: UserAttribute every;
  2: list<UserAttribute> exact;
}

struct PropertyDefinition_key {
  1: optional PropertyDeclaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional PropertyDefinition_type type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional Visibility visibility;
  4: optional bool isFinal;
  5: optional bool isAbstract;
  6: optional bool isStatic;
  7: optional PropertyDefinition_attributes_array attributes (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Parameter_type {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_ just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct Parameter_defaultValue {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional string just;
  3: bool any = false;
}

union Parameter_attributes_array {
  1: UserAttribute every;
  2: list<UserAttribute> exact;
}

struct Parameter {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Parameter_type type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional bool isInout;
  4: optional bool isVariadic;
  5: optional Parameter_defaultValue defaultValue;
  6: optional Parameter_attributes_array attributes (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Signature_returns {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_ just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

union Signature_parameters_array {
  1: Parameter every;
  2: list<Parameter> exact;
}

struct Signature_key {
  1: optional Signature_returns returns (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Signature_parameters_array parameters (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct NamespaceQName_parent {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional NamespaceQName just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct NamespaceQName_key {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional NamespaceQName_parent parent (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct NamespaceDeclaration_key {
  1: optional NamespaceQName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct NameLowerCase_key {
  1: optional string nameLowercase;
  2: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct InterfaceDeclaration_key {
  1: optional QName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct GlobalConstDefinition_type {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_ just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct GlobalConstDefinition_namespace_ {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional NamespaceDeclaration just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct GlobalConstDefinition_key {
  1: optional GlobalConstDeclaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional GlobalConstDefinition_type type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional GlobalConstDefinition_namespace_ namespace_ (java.swift.name = "namespace_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional string value;
}

struct GlobalConstDeclaration_key {
  1: optional QName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FunctionDeclaration_key {
  1: optional QName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Enumerator_key {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional EnumDeclaration enumeration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct EnumDefinition_enumBase {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_ just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct EnumDefinition_enumConstraint {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_ just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

union EnumDefinition_enumerators_array {
  1: Enumerator every;
  2: list<Enumerator> exact;
}

struct EnumDefinition_namespace_ {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional NamespaceDeclaration just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

union EnumDefinition_attributes_array {
  1: UserAttribute every;
  2: list<UserAttribute> exact;
}

struct EnumDefinition_key {
  1: optional EnumDeclaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional EnumDefinition_enumBase enumBase (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional EnumDefinition_enumConstraint enumConstraint (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional EnumDefinition_enumerators_array enumerators (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional EnumDefinition_namespace_ namespace_ (java.swift.name = "namespace_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: optional EnumDefinition_attributes_array attributes (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct EnumDeclaration_key {
  1: optional QName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct ContainerDeclaration {
  1: optional ClassDeclaration class_ (java.swift.name = "class_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional InterfaceDeclaration interface_ (java.swift.name = "interface_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional TraitDeclaration trait (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: bool any = false;
}

struct ContainerParent_key {
  1: optional ContainerDeclaration container (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ContainerDeclaration parent (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Declaration {
  1: optional ClassConstDeclaration classConst (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ContainerDeclaration container (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional EnumDeclaration enum_ (java.swift.name = "enum_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional Enumerator enumerator (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional FunctionDeclaration function_ (java.swift.name = "function_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: optional GlobalConstDeclaration globalConst (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  7: optional NamespaceDeclaration namespace_ (java.swift.name = "namespace_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  8: optional MethodDeclaration method (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  9: optional PropertyDeclaration property_ (java.swift.name = "property_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  10: optional TypeConstDeclaration typeConst (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  11: optional TypedefDeclaration typedef_ (java.swift.name = "typedef_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  12: bool any = false;
}

struct DeclarationComment_span {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.ByteSpan just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct DeclarationComment_comment {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Comment just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct DeclarationComment_key {
  1: optional Declaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional DeclarationComment_span span (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional DeclarationComment_comment comment (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct DeclarationLocation_key {
  1: optional Declaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.ByteSpan span (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct DeclarationName_key {
  1: optional Declaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct DeclarationSource_key {
  1: optional Declaration target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Declaration source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct DeclarationSpan_key {
  1: optional Declaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.ByteSpan span (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct DeclarationTarget_key {
  1: optional Declaration source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Declaration target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union FileDeclarations_declarations_array {
  1: Declaration every;
  2: list<Declaration> exact;
}

struct FileDeclarations_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional FileDeclarations_declarations_array declarations (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct NamespaceMember_key {
  1: optional NamespaceQName namespace_ (java.swift.name = "namespace_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct XRefTarget {
  1: optional Declaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: bool any = false;
}

struct TargetUses_key {
  1: optional XRefTarget target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.ByteSpans uses (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct TargetUsesAbs_key {
  1: optional XRefTarget target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.ByteSpans uses (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct XRef {
  1: optional XRefTarget target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.ByteSpans ranges (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union FileXRefs_xrefs_array {
  1: XRef every;
  2: list<XRef> exact;
}

struct FileXRefs_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional FileXRefs_xrefs_array xrefs (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct MethodDeclaration_key {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ContainerDeclaration container (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct PropertyDeclaration_key {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ContainerDeclaration container (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct TypeConstDeclaration_key {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ContainerDeclaration container (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct ContainerChild_key {
  1: optional ContainerDeclaration container (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ContainerDeclaration child (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

enum ConstraintKind {
  As = 0,
  Equal = 1,
  Super = 2
} (hs.nounknown)

struct Constraint {
  1: optional ConstraintKind constraintKind;
  2: optional Type_ type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union TypeParameter_constraints_array {
  1: Constraint every;
  2: list<Constraint> exact;
}

union TypeParameter_attributes_array {
  1: UserAttribute every;
  2: list<UserAttribute> exact;
}

struct TypeParameter {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Variance variance;
  3: optional ReifyKind reifyKind;
  4: optional TypeParameter_constraints_array constraints (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional TypeParameter_attributes_array attributes (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FunctionDefinition_namespace_ {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional NamespaceDeclaration just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

union FunctionDefinition_attributes_array {
  1: UserAttribute every;
  2: list<UserAttribute> exact;
}

union FunctionDefinition_typeParams_array {
  1: TypeParameter every;
  2: list<TypeParameter> exact;
}

struct FunctionDefinition_key {
  1: optional FunctionDeclaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Signature signature (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional bool isAsync;
  4: optional FunctionDefinition_namespace_ namespace_ (java.swift.name = "namespace_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional FunctionDefinition_attributes_array attributes (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: optional FunctionDefinition_typeParams_array typeParams (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union InterfaceDefinition_members_array {
  1: Declaration every;
  2: list<Declaration> exact;
}

struct InterfaceDefinition_namespace_ {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional NamespaceDeclaration just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

union InterfaceDefinition_extends__array {
  1: InterfaceDeclaration every;
  2: list<InterfaceDeclaration> exact;
}

union InterfaceDefinition_attributes_array {
  1: UserAttribute every;
  2: list<UserAttribute> exact;
}

union InterfaceDefinition_typeParams_array {
  1: TypeParameter every;
  2: list<TypeParameter> exact;
}

union InterfaceDefinition_requireExtends_array {
  1: ClassDeclaration every;
  2: list<ClassDeclaration> exact;
}

struct InterfaceDefinition_key {
  1: optional InterfaceDeclaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional InterfaceDefinition_members_array members (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional InterfaceDefinition_namespace_ namespace_ (java.swift.name = "namespace_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional InterfaceDefinition_extends__array extends_ (java.swift.name = "extends_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional InterfaceDefinition_attributes_array attributes (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: optional InterfaceDefinition_typeParams_array typeParams (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  7: optional InterfaceDefinition_requireExtends_array requireExtends (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union MethodDefinition_attributes_array {
  1: UserAttribute every;
  2: list<UserAttribute> exact;
}

union MethodDefinition_typeParams_array {
  1: TypeParameter every;
  2: list<TypeParameter> exact;
}

struct MethodDefinition_key {
  1: optional MethodDeclaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Signature signature (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional Visibility visibility;
  4: optional bool isAbstract;
  5: optional bool isAsync;
  6: optional bool isFinal;
  7: optional bool isStatic;
  8: optional MethodDefinition_attributes_array attributes (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  9: optional MethodDefinition_typeParams_array typeParams (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union TraitDefinition_members_array {
  1: Declaration every;
  2: list<Declaration> exact;
}

struct TraitDefinition_namespace_ {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional NamespaceDeclaration just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

union TraitDefinition_implements__array {
  1: InterfaceDeclaration every;
  2: list<InterfaceDeclaration> exact;
}

union TraitDefinition_uses_array {
  1: TraitDeclaration every;
  2: list<TraitDeclaration> exact;
}

union TraitDefinition_attributes_array {
  1: UserAttribute every;
  2: list<UserAttribute> exact;
}

union TraitDefinition_typeParams_array {
  1: TypeParameter every;
  2: list<TypeParameter> exact;
}

union TraitDefinition_requireExtends_array {
  1: ClassDeclaration every;
  2: list<ClassDeclaration> exact;
}

union TraitDefinition_requireImplements_array {
  1: InterfaceDeclaration every;
  2: list<InterfaceDeclaration> exact;
}

struct TraitDefinition_key {
  1: optional TraitDeclaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional TraitDefinition_members_array members (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional TraitDefinition_namespace_ namespace_ (java.swift.name = "namespace_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional TraitDefinition_implements__array implements_ (java.swift.name = "implements_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional TraitDefinition_uses_array uses (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: optional TraitDefinition_attributes_array attributes (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  7: optional TraitDefinition_typeParams_array typeParams (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  8: optional TraitDefinition_requireExtends_array requireExtends (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  9: optional TraitDefinition_requireImplements_array requireImplements (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct TypedefDeclaration_namespace_ {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional NamespaceDeclaration just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

union TypedefDeclaration_attributes_array {
  1: UserAttribute every;
  2: list<UserAttribute> exact;
}

union TypedefDeclaration_typeParams_array {
  1: TypeParameter every;
  2: list<TypeParameter> exact;
}

struct TypedefDeclaration_key {
  1: optional QName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional bool isTransparent;
  3: optional TypedefDeclaration_namespace_ namespace_ (java.swift.name = "namespace_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional TypedefDeclaration_attributes_array attributes (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional TypedefDeclaration_typeParams_array typeParams (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union ClassDefinition_members_array {
  1: Declaration every;
  2: list<Declaration> exact;
}

struct ClassDefinition_namespace_ {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional NamespaceDeclaration just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct ClassDefinition_extends_ {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ClassDeclaration just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

union ClassDefinition_implements__array {
  1: InterfaceDeclaration every;
  2: list<InterfaceDeclaration> exact;
}

union ClassDefinition_uses_array {
  1: TraitDeclaration every;
  2: list<TraitDeclaration> exact;
}

union ClassDefinition_attributes_array {
  1: UserAttribute every;
  2: list<UserAttribute> exact;
}

union ClassDefinition_typeParams_array {
  1: TypeParameter every;
  2: list<TypeParameter> exact;
}

struct ClassDefinition_key {
  1: optional ClassDeclaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional bool isAbstract;
  3: optional bool isFinal;
  4: optional ClassDefinition_members_array members (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional ClassDefinition_namespace_ namespace_ (java.swift.name = "namespace_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: optional ClassDefinition_extends_ extends_ (java.swift.name = "extends_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  7: optional ClassDefinition_implements__array implements_ (java.swift.name = "implements_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  8: optional ClassDefinition_uses_array uses (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  9: optional ClassDefinition_attributes_array attributes (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  10: optional ClassDefinition_typeParams_array typeParams (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct ClassDeclaration_key {
  1: optional QName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct ClassConstDefinition_type {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_ just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct ClassConstDefinition_value {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional string just;
  3: bool any = false;
}

struct ClassConstDefinition_key {
  1: optional ClassConstDeclaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ClassConstDefinition_type type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional ClassConstDefinition_value value;
}

struct ClassConstDeclaration_key {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ContainerDeclaration container (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}
