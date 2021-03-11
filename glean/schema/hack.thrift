// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/builtin.thrift"
include "glean/schema/src.thrift"

namespace cpp2 facebook.glean.schema.hack
namespace hs Glean.Schema
namespace php glean_schema_hack
namespace py glean.schema.hack
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.hack
namespace rust glean_schema_hack

hs_include "glean/schema/hack_include.hs"
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
struct symbolNamespace {
  1: symbolNamespace_id id (hs.strict);
  2: optional symbolNamespace_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id kind_id

@glean.PredicateAnnotation{
  name="hack.kind";
  version=1;
}
struct kind {
  1: kind_id id (hs.strict);
  2: optional kind_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id identifier_id

@glean.PredicateAnnotation{
  name="hack.identifier";
  version=1;
}
struct identifier {
  1: identifier_id id (hs.strict);
  2: optional identifier_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id filename_id

@glean.PredicateAnnotation{
  name="hack.filename";
  version=1;
}
struct filename {
  1: filename_id id (hs.strict);
  2: optional filename_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id symbol_id

@glean.PredicateAnnotation{
  name="hack.symbol";
  version=1;
}
struct symbol {
  1: symbol_id id (hs.strict);
  2: optional symbol_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id UserAttribute_id

@glean.PredicateAnnotation{
  name="hack.UserAttribute";
  version=4;
}
struct UserAttribute {
  1: UserAttribute_id id (hs.strict);
  2: optional UserAttribute_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TypeConstDefinition_id

@glean.PredicateAnnotation{
  name="hack.TypeConstDefinition";
  version=4;
}
struct TypeConstDefinition {
  1: TypeConstDefinition_id id (hs.strict);
  2: optional TypeConstDefinition_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Type_id

@glean.PredicateAnnotation{
  name="hack.Type";
  version=4;
}
struct Type {
  1: Type_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id TraitDeclaration_id

@glean.PredicateAnnotation{
  name="hack.TraitDeclaration";
  version=4;
}
struct TraitDeclaration {
  1: TraitDeclaration_id id (hs.strict);
  2: optional TraitDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id QName_id

@glean.PredicateAnnotation{
  name="hack.QName";
  version=4;
}
struct QName {
  1: QName_id id (hs.strict);
  2: optional QName_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id PropertyDefinition_id

@glean.PredicateAnnotation{
  name="hack.PropertyDefinition";
  version=4;
}
struct PropertyDefinition {
  1: PropertyDefinition_id id (hs.strict);
  2: optional PropertyDefinition_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Signature_id

@glean.PredicateAnnotation{
  name="hack.Signature";
  version=4;
}
struct Signature {
  1: Signature_id id (hs.strict);
  2: optional Signature_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id NamespaceQName_id

@glean.PredicateAnnotation{
  name="hack.NamespaceQName";
  version=4;
}
struct NamespaceQName {
  1: NamespaceQName_id id (hs.strict);
  2: optional NamespaceQName_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id NamespaceDeclaration_id

@glean.PredicateAnnotation{
  name="hack.NamespaceDeclaration";
  version=4;
}
struct NamespaceDeclaration {
  1: NamespaceDeclaration_id id (hs.strict);
  2: optional NamespaceDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id NameLowerCase_id

@glean.PredicateAnnotation{
  name="hack.NameLowerCase";
  version=4;
}
struct NameLowerCase {
  1: NameLowerCase_id id (hs.strict);
  2: optional NameLowerCase_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Name_id

@glean.PredicateAnnotation{
  name="hack.Name";
  version=4;
}
struct Name {
  1: Name_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id InterfaceDeclaration_id

@glean.PredicateAnnotation{
  name="hack.InterfaceDeclaration";
  version=4;
}
struct InterfaceDeclaration {
  1: InterfaceDeclaration_id id (hs.strict);
  2: optional InterfaceDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id GlobalConstDefinition_id

@glean.PredicateAnnotation{
  name="hack.GlobalConstDefinition";
  version=4;
}
struct GlobalConstDefinition {
  1: GlobalConstDefinition_id id (hs.strict);
  2: optional GlobalConstDefinition_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id GlobalConstDeclaration_id

@glean.PredicateAnnotation{
  name="hack.GlobalConstDeclaration";
  version=4;
}
struct GlobalConstDeclaration {
  1: GlobalConstDeclaration_id id (hs.strict);
  2: optional GlobalConstDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FunctionDeclaration_id

@glean.PredicateAnnotation{
  name="hack.FunctionDeclaration";
  version=4;
}
struct FunctionDeclaration {
  1: FunctionDeclaration_id id (hs.strict);
  2: optional FunctionDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Enumerator_id

@glean.PredicateAnnotation{
  name="hack.Enumerator";
  version=4;
}
struct Enumerator {
  1: Enumerator_id id (hs.strict);
  2: optional Enumerator_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id EnumDefinition_id

@glean.PredicateAnnotation{
  name="hack.EnumDefinition";
  version=4;
}
struct EnumDefinition {
  1: EnumDefinition_id id (hs.strict);
  2: optional EnumDefinition_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id EnumDeclaration_id

@glean.PredicateAnnotation{
  name="hack.EnumDeclaration";
  version=4;
}
struct EnumDeclaration {
  1: EnumDeclaration_id id (hs.strict);
  2: optional EnumDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ContainerParent_id

@glean.PredicateAnnotation{
  name="hack.ContainerParent";
  version=4;
}
struct ContainerParent {
  1: ContainerParent_id id (hs.strict);
  2: optional ContainerParent_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclarationComment_id

@glean.PredicateAnnotation{
  name="hack.DeclarationComment";
  version=4;
}
struct DeclarationComment {
  1: DeclarationComment_id id (hs.strict);
  2: optional DeclarationComment_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclarationLocation_id

@glean.PredicateAnnotation{
  name="hack.DeclarationLocation";
  version=4;
}
struct DeclarationLocation {
  1: DeclarationLocation_id id (hs.strict);
  2: optional DeclarationLocation_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclarationName_id

@glean.PredicateAnnotation{
  name="hack.DeclarationName";
  version=4;
}
struct DeclarationName {
  1: DeclarationName_id id (hs.strict);
  2: optional DeclarationName_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclarationSource_id

@glean.PredicateAnnotation{
  name="hack.DeclarationSource";
  version=4;
}
struct DeclarationSource {
  1: DeclarationSource_id id (hs.strict);
  2: optional DeclarationSource_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclarationSpan_id

@glean.PredicateAnnotation{
  name="hack.DeclarationSpan";
  version=4;
}
struct DeclarationSpan {
  1: DeclarationSpan_id id (hs.strict);
  2: optional DeclarationSpan_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DeclarationTarget_id

@glean.PredicateAnnotation{
  name="hack.DeclarationTarget";
  version=4;
}
struct DeclarationTarget {
  1: DeclarationTarget_id id (hs.strict);
  2: optional DeclarationTarget_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FileDeclarations_id

@glean.PredicateAnnotation{
  name="hack.FileDeclarations";
  version=4;
}
struct FileDeclarations {
  1: FileDeclarations_id id (hs.strict);
  2: optional FileDeclarations_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id NamespaceMember_id

@glean.PredicateAnnotation{
  name="hack.NamespaceMember";
  version=4;
}
struct NamespaceMember {
  1: NamespaceMember_id id (hs.strict);
  2: optional NamespaceMember_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TargetUses_id

@glean.PredicateAnnotation{
  name="hack.TargetUses";
  version=4;
}
struct TargetUses {
  1: TargetUses_id id (hs.strict);
  2: optional TargetUses_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TargetUsesAbs_id

@glean.PredicateAnnotation{
  name="hack.TargetUsesAbs";
  version=4;
}
struct TargetUsesAbs {
  1: TargetUsesAbs_id id (hs.strict);
  2: optional TargetUsesAbs_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FileXRefs_id

@glean.PredicateAnnotation{
  name="hack.FileXRefs";
  version=4;
}
struct FileXRefs {
  1: FileXRefs_id id (hs.strict);
  2: optional FileXRefs_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id MethodDeclaration_id

@glean.PredicateAnnotation{
  name="hack.MethodDeclaration";
  version=4;
}
struct MethodDeclaration {
  1: MethodDeclaration_id id (hs.strict);
  2: optional MethodDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id PropertyDeclaration_id

@glean.PredicateAnnotation{
  name="hack.PropertyDeclaration";
  version=4;
}
struct PropertyDeclaration {
  1: PropertyDeclaration_id id (hs.strict);
  2: optional PropertyDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TypeConstDeclaration_id

@glean.PredicateAnnotation{
  name="hack.TypeConstDeclaration";
  version=4;
}
struct TypeConstDeclaration {
  1: TypeConstDeclaration_id id (hs.strict);
  2: optional TypeConstDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ContainerChild_id

@glean.PredicateAnnotation{
  name="hack.ContainerChild";
  version=4;
}
struct ContainerChild {
  1: ContainerChild_id id (hs.strict);
  2: optional ContainerChild_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FunctionDefinition_id

@glean.PredicateAnnotation{
  name="hack.FunctionDefinition";
  version=4;
}
struct FunctionDefinition {
  1: FunctionDefinition_id id (hs.strict);
  2: optional FunctionDefinition_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id InterfaceDefinition_id

@glean.PredicateAnnotation{
  name="hack.InterfaceDefinition";
  version=4;
}
struct InterfaceDefinition {
  1: InterfaceDefinition_id id (hs.strict);
  2: optional InterfaceDefinition_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id MethodDefinition_id

@glean.PredicateAnnotation{
  name="hack.MethodDefinition";
  version=4;
}
struct MethodDefinition {
  1: MethodDefinition_id id (hs.strict);
  2: optional MethodDefinition_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TraitDefinition_id

@glean.PredicateAnnotation{
  name="hack.TraitDefinition";
  version=4;
}
struct TraitDefinition {
  1: TraitDefinition_id id (hs.strict);
  2: optional TraitDefinition_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TypedefDeclaration_id

@glean.PredicateAnnotation{
  name="hack.TypedefDeclaration";
  version=4;
}
struct TypedefDeclaration {
  1: TypedefDeclaration_id id (hs.strict);
  2: optional TypedefDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Comment_id

@glean.PredicateAnnotation{
  name="hack.Comment";
  version=4;
}
struct Comment {
  1: Comment_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id ClassDefinition_id

@glean.PredicateAnnotation{
  name="hack.ClassDefinition";
  version=4;
}
struct ClassDefinition {
  1: ClassDefinition_id id (hs.strict);
  2: optional ClassDefinition_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ClassDeclaration_id

@glean.PredicateAnnotation{
  name="hack.ClassDeclaration";
  version=4;
}
struct ClassDeclaration {
  1: ClassDeclaration_id id (hs.strict);
  2: optional ClassDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ClassConstDefinition_id

@glean.PredicateAnnotation{
  name="hack.ClassConstDefinition";
  version=4;
}
struct ClassConstDefinition {
  1: ClassConstDefinition_id id (hs.strict);
  2: optional ClassConstDefinition_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ClassConstDeclaration_id

@glean.PredicateAnnotation{
  name="hack.ClassConstDeclaration";
  version=4;
}
struct ClassConstDeclaration {
  1: ClassConstDeclaration_id id (hs.strict);
  2: optional ClassConstDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct symbolNamespace_key {
  1: glean.Nat namespace_id;
  2: string namespace_name;
}

struct kind_key {
  1: glean.Nat id;
  2: string name;
}

struct identifier_key {
  1: glean.Nat kind;
  2: string name;
}

struct filename_key {
  1: string filename;
  2: string filehash_id;
}

struct context {
  1: bool acid;
  2: bool actype;
  3: bool acnew;
}

struct symbol_key {
  1: string name_lowercase;
  2: context valid;
  3: glean.Nat kind_id;
  4: glean.Nat ns_id;
  5: string filehash_id;
  6: bool is_abstract;
  7: bool is_final;
  8: string canonical_name;
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

struct UserAttribute_key {
  1: Name name;
  2: list<string> parameters;
}

enum TypeConstKind {
  Abstract = 0,
  Concrete = 1,
  PartiallyAbstract = 2
} (hs.nounknown)

struct TypeConstDefinition_key {
  1: TypeConstDeclaration declaration;
  2: optional Type type;
  3: TypeConstKind kind;
  4: list<UserAttribute> attributes;
}

struct TraitDeclaration_key {
  1: QName name;
}

enum ReifyKind {
  Erased = 0,
  Reified = 1,
  SoftReified = 2
} (hs.nounknown)

struct QName_key {
  1: Name name;
  2: optional NamespaceQName namespace_ (java.swift.name = "namespace_");
}

struct PropertyDefinition_key {
  1: PropertyDeclaration declaration;
  2: optional Type type;
  3: Visibility visibility;
  4: bool isFinal;
  5: bool isAbstract;
  6: bool isStatic;
  7: list<UserAttribute> attributes;
}

struct Parameter {
  1: Name name;
  2: optional Type type;
  3: bool isInout;
  4: bool isVariadic;
  5: optional string defaultValue;
  6: list<UserAttribute> attributes;
}

struct Signature_key {
  1: optional Type returns;
  2: list<Parameter> parameters;
}

struct NamespaceQName_key {
  1: Name name;
  2: optional NamespaceQName parent;
}

struct NamespaceDeclaration_key {
  1: NamespaceQName name;
}

struct NameLowerCase_key {
  1: string nameLowercase;
  2: Name name;
}

struct InterfaceDeclaration_key {
  1: QName name;
}

struct GlobalConstDefinition_key {
  1: GlobalConstDeclaration declaration;
  2: optional Type type;
  3: optional NamespaceDeclaration namespace_ (java.swift.name = "namespace_");
  4: string value;
}

struct GlobalConstDeclaration_key {
  1: QName name;
}

struct FunctionDeclaration_key {
  1: QName name;
}

struct Enumerator_key {
  1: Name name;
  2: EnumDeclaration enumeration;
}

struct EnumDefinition_key {
  1: EnumDeclaration declaration;
  2: optional Type enumBase;
  3: optional Type enumConstraint;
  4: list<Enumerator> enumerators;
  5: optional NamespaceDeclaration namespace_ (java.swift.name = "namespace_");
  6: list<UserAttribute> attributes;
}

struct EnumDeclaration_key {
  1: QName name;
}

union ContainerDeclaration {
  1: ClassDeclaration class_ (java.swift.name = "class_");
  2: InterfaceDeclaration interface_ (java.swift.name = "interface_");
  3: TraitDeclaration trait;
} (hs.nonempty)

struct ContainerParent_key {
  1: ContainerDeclaration container;
  2: ContainerDeclaration parent;
}

union Declaration {
  1: ClassConstDeclaration classConst;
  2: ContainerDeclaration container;
  3: EnumDeclaration enum_ (java.swift.name = "enum_");
  4: Enumerator enumerator;
  5: FunctionDeclaration function_ (java.swift.name = "function_");
  6: GlobalConstDeclaration globalConst;
  7: NamespaceDeclaration namespace_ (java.swift.name = "namespace_");
  8: MethodDeclaration method;
  9: PropertyDeclaration property_ (java.swift.name = "property_");
  10: TypeConstDeclaration typeConst;
  11: TypedefDeclaration typedef_ (java.swift.name = "typedef_");
} (hs.nonempty)

struct DeclarationComment_key {
  1: Declaration declaration;
  2: src.File file;
  3: optional src.ByteSpan span;
  4: optional Comment comment;
}

struct DeclarationLocation_key {
  1: Declaration declaration;
  2: src.File file;
  3: src.ByteSpan span;
}

struct DeclarationName_key {
  1: Declaration declaration;
  2: Name name;
}

struct DeclarationSource_key {
  1: Declaration target;
  2: Declaration source;
}

struct DeclarationSpan_key {
  1: Declaration declaration;
  2: src.File file;
  3: src.ByteSpan span;
}

struct DeclarationTarget_key {
  1: Declaration source;
  2: Declaration target;
}

struct FileDeclarations_key {
  1: src.File file;
  2: list<Declaration> declarations;
}

struct NamespaceMember_key {
  1: NamespaceQName namespace_ (java.swift.name = "namespace_");
  2: Declaration decl;
}

union XRefTarget {
  1: Declaration declaration;
} (hs.nonempty)

struct TargetUses_key {
  1: XRefTarget target;
  2: src.File file;
  3: src.ByteSpans uses;
}

struct TargetUsesAbs_key {
  1: XRefTarget target;
  2: src.File file;
  3: src.ByteSpans uses;
}

struct XRef {
  1: XRefTarget target;
  2: src.ByteSpans ranges;
}

struct FileXRefs_key {
  1: src.File file;
  2: list<XRef> xrefs;
}

struct MethodDeclaration_key {
  1: Name name;
  2: ContainerDeclaration container;
}

struct PropertyDeclaration_key {
  1: Name name;
  2: ContainerDeclaration container;
}

struct TypeConstDeclaration_key {
  1: Name name;
  2: ContainerDeclaration container;
}

struct ContainerChild_key {
  1: ContainerDeclaration container;
  2: ContainerDeclaration child;
}

enum ConstraintKind {
  As = 0,
  Equal = 1,
  Super = 2
} (hs.nounknown)

struct Constraint {
  1: ConstraintKind constraintKind;
  2: Type type;
}

struct TypeParameter {
  1: Name name;
  2: Variance variance;
  3: ReifyKind reifyKind;
  4: list<Constraint> constraints;
  5: list<UserAttribute> attributes;
}

struct FunctionDefinition_key {
  1: FunctionDeclaration declaration;
  2: Signature signature;
  3: bool isAsync;
  4: optional NamespaceDeclaration namespace_ (java.swift.name = "namespace_");
  5: list<UserAttribute> attributes;
  6: list<TypeParameter> typeParams;
}

struct InterfaceDefinition_key {
  1: InterfaceDeclaration declaration;
  2: list<Declaration> members;
  3: optional NamespaceDeclaration namespace_ (java.swift.name = "namespace_");
  4: list<InterfaceDeclaration> extends_ (java.swift.name = "extends_");
  5: list<UserAttribute> attributes;
  6: list<TypeParameter> typeParams;
  7: list<ClassDeclaration> requireExtends;
}

struct MethodDefinition_key {
  1: MethodDeclaration declaration;
  2: Signature signature;
  3: Visibility visibility;
  4: bool isAbstract;
  5: bool isAsync;
  6: bool isFinal;
  7: bool isStatic;
  8: list<UserAttribute> attributes;
  9: list<TypeParameter> typeParams;
}

struct TraitDefinition_key {
  1: TraitDeclaration declaration;
  2: list<Declaration> members;
  3: optional NamespaceDeclaration namespace_ (java.swift.name = "namespace_");
  4: list<InterfaceDeclaration> implements_ (java.swift.name = "implements_");
  5: list<TraitDeclaration> uses;
  6: list<UserAttribute> attributes;
  7: list<TypeParameter> typeParams;
  8: list<ClassDeclaration> requireExtends;
  9: list<InterfaceDeclaration> requireImplements;
}

struct TypedefDeclaration_key {
  1: QName name;
  2: bool isTransparent;
  3: optional NamespaceDeclaration namespace_ (java.swift.name = "namespace_");
  4: list<UserAttribute> attributes;
  5: list<TypeParameter> typeParams;
}

struct ClassDefinition_key {
  1: ClassDeclaration declaration;
  2: bool isAbstract;
  3: bool isFinal;
  4: list<Declaration> members;
  5: optional NamespaceDeclaration namespace_ (java.swift.name = "namespace_");
  6: optional ClassDeclaration extends_ (java.swift.name = "extends_");
  7: list<InterfaceDeclaration> implements_ (java.swift.name = "implements_");
  8: list<TraitDeclaration> uses;
  9: list<UserAttribute> attributes;
  10: list<TypeParameter> typeParams;
}

struct ClassDeclaration_key {
  1: QName name;
}

struct ClassConstDefinition_key {
  1: ClassConstDeclaration declaration;
  2: optional Type type;
  3: optional string value;
}

struct ClassConstDeclaration_key {
  1: Name name;
  2: ContainerDeclaration container;
}
