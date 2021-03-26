// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/v2/thrift/builtin.thrift"
include "glean/schema/v2/thrift/src.thrift"

namespace cpp2 facebook.glean.schema.java
namespace hs Glean.Schema
namespace php glean_schema_java
namespace py glean.schema.java
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.java
namespace rust glean_schema_java

hs_include "glean/schema/v2/thrift/java_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "XRef": 3,
  "TypeParam": 3,
  "Annotation": 4,
  "MethodDeclaration": 4,
  "MethodDeclaration_3": 3,
  "MethodDeclaration_2": 2,
  "AnnotatedClass": 4,
  "InterfaceDeclaration": 4,
  "InterfaceDeclaration_3": 3,
  "InterfaceDeclaration_2": 2,
  "ClassDeclaration": 4,
  "ClassDeclaration_3": 3,
  "ClassDeclaration_2": 2,
  "FileXRefs": 3,
  "TypeArg": 3,
  "Inheritance": 4,
  "Inheritance_3": 3,
  "Name": 2,
  "ConstructorDeclaration": 4,
  "ConstructorDeclaration_3": 3,
  "ConstructorDeclaration_2": 2,
  "Type": 3,
  "Type_2": 2,
  "VariableDeclaration": 4,
  "VariableDeclaration_3": 3,
  "VariableDeclaration_2": 2,
  "QName": 3,
}


typedef glean.Id XRef_id

@glean.PredicateAnnotation{
  name="java.XRef";
  version=3;
}
struct XRef {
  1: XRef_id id (hs.strict);
  2: optional XRef_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TypeParam_id

@glean.PredicateAnnotation{
  name="java.TypeParam";
  version=3;
}
struct TypeParam {
  1: TypeParam_id id (hs.strict);
  2: optional TypeParam_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TypeArg_id

@glean.PredicateAnnotation{
  name="java.TypeArg";
  version=3;
}
struct TypeArg {
  1: TypeArg_id id (hs.strict);
  2: optional TypeArg_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Type_id

@glean.PredicateAnnotation{
  name="java.Type";
  version=3;
}
struct Type {
  1: Type_id id (hs.strict);
  2: optional Type_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Type_2_id

@glean.PredicateAnnotation{
  name="java.Type";
  version=2;
}
struct Type_2 {
  1: Type_2_id id (hs.strict);
  2: optional Type_2_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id QName_id

@glean.PredicateAnnotation{
  name="java.QName";
  version=3;
}
struct QName {
  1: QName_id id (hs.strict);
  2: optional QName_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Name_id

@glean.PredicateAnnotation{
  name="java.Name";
  version=2;
}
struct Name {
  1: Name_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id VariableDeclaration_2_id

@glean.PredicateAnnotation{
  name="java.VariableDeclaration";
  version=2;
}
struct VariableDeclaration_2 {
  1: VariableDeclaration_2_id id (hs.strict);
  2: optional VariableDeclaration_2_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id VariableDeclaration_3_id

@glean.PredicateAnnotation{
  name="java.VariableDeclaration";
  version=3;
}
struct VariableDeclaration_3 {
  1: VariableDeclaration_3_id id (hs.strict);
  2: optional VariableDeclaration_3_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id VariableDeclaration_id

@glean.PredicateAnnotation{
  name="java.VariableDeclaration";
  version=4;
}
struct VariableDeclaration {
  1: VariableDeclaration_id id (hs.strict);
  2: optional VariableDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id MethodDeclaration_id

@glean.PredicateAnnotation{
  name="java.MethodDeclaration";
  version=4;
}
struct MethodDeclaration {
  1: MethodDeclaration_id id (hs.strict);
  2: optional MethodDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id MethodDeclaration_3_id

@glean.PredicateAnnotation{
  name="java.MethodDeclaration";
  version=3;
}
struct MethodDeclaration_3 {
  1: MethodDeclaration_3_id id (hs.strict);
  2: optional MethodDeclaration_3_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id MethodDeclaration_2_id

@glean.PredicateAnnotation{
  name="java.MethodDeclaration";
  version=2;
}
struct MethodDeclaration_2 {
  1: MethodDeclaration_2_id id (hs.strict);
  2: optional MethodDeclaration_2_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id InterfaceDeclaration_id

@glean.PredicateAnnotation{
  name="java.InterfaceDeclaration";
  version=4;
}
struct InterfaceDeclaration {
  1: InterfaceDeclaration_id id (hs.strict);
  2: optional InterfaceDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id InterfaceDeclaration_3_id

@glean.PredicateAnnotation{
  name="java.InterfaceDeclaration";
  version=3;
}
struct InterfaceDeclaration_3 {
  1: InterfaceDeclaration_3_id id (hs.strict);
  2: optional InterfaceDeclaration_3_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id InterfaceDeclaration_2_id

@glean.PredicateAnnotation{
  name="java.InterfaceDeclaration";
  version=2;
}
struct InterfaceDeclaration_2 {
  1: InterfaceDeclaration_2_id id (hs.strict);
  2: optional InterfaceDeclaration_2_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Inheritance_id

@glean.PredicateAnnotation{
  name="java.Inheritance";
  version=4;
}
struct Inheritance {
  1: Inheritance_id id (hs.strict);
  2: optional Inheritance_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Inheritance_3_id

@glean.PredicateAnnotation{
  name="java.Inheritance";
  version=3;
}
struct Inheritance_3 {
  1: Inheritance_3_id id (hs.strict);
  2: optional Inheritance_3_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FileXRefs_id

@glean.PredicateAnnotation{
  name="java.FileXRefs";
  version=3;
}
struct FileXRefs {
  1: FileXRefs_id id (hs.strict);
  2: optional FileXRefs_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ConstructorDeclaration_id

@glean.PredicateAnnotation{
  name="java.ConstructorDeclaration";
  version=4;
}
struct ConstructorDeclaration {
  1: ConstructorDeclaration_id id (hs.strict);
  2: optional ConstructorDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ConstructorDeclaration_3_id

@glean.PredicateAnnotation{
  name="java.ConstructorDeclaration";
  version=3;
}
struct ConstructorDeclaration_3 {
  1: ConstructorDeclaration_3_id id (hs.strict);
  2: optional ConstructorDeclaration_3_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ConstructorDeclaration_2_id

@glean.PredicateAnnotation{
  name="java.ConstructorDeclaration";
  version=2;
}
struct ConstructorDeclaration_2 {
  1: ConstructorDeclaration_2_id id (hs.strict);
  2: optional ConstructorDeclaration_2_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ClassDeclaration_id

@glean.PredicateAnnotation{
  name="java.ClassDeclaration";
  version=4;
}
struct ClassDeclaration {
  1: ClassDeclaration_id id (hs.strict);
  2: optional ClassDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ClassDeclaration_3_id

@glean.PredicateAnnotation{
  name="java.ClassDeclaration";
  version=3;
}
struct ClassDeclaration_3 {
  1: ClassDeclaration_3_id id (hs.strict);
  2: optional ClassDeclaration_3_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ClassDeclaration_2_id

@glean.PredicateAnnotation{
  name="java.ClassDeclaration";
  version=2;
}
struct ClassDeclaration_2 {
  1: ClassDeclaration_2_id id (hs.strict);
  2: optional ClassDeclaration_2_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Annotation_id

@glean.PredicateAnnotation{
  name="java.Annotation";
  version=4;
}
struct Annotation {
  1: Annotation_id id (hs.strict);
  2: optional Annotation_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id AnnotatedClass_id

@glean.PredicateAnnotation{
  name="java.AnnotatedClass";
  version=4;
}
struct AnnotatedClass {
  1: AnnotatedClass_id id (hs.strict);
  2: optional AnnotatedClass_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union XRefTarget {
  1: QName class_or_interface_ (java.swift.name = "class_or_interface_");
  2: QName ctor_ (java.swift.name = "ctor_");
  3: QName method_ (java.swift.name = "method_");
} (hs.nonempty)

struct XRef_key {
  1: XRefTarget target;
  2: list<src.ByteSpan> ranges;
}

union Wildcard {
  1: Type extends_ (java.swift.name = "extends_");
  2: Type super_ (java.swift.name = "super_");
  3: bool unbounded;
} (hs.nonempty)

struct TypeParam_key {
  1: string name;
  2: list<Type> extends_ (java.swift.name = "extends_");
}

union TypeArg_key {
  1: Type type (py3.name = "type_");
  2: Wildcard wildcard;
} (hs.nonempty)

struct Type_key {
  1: string type;
  2: list<TypeArg> typeArgs;
}

struct Type_2_key {
  1: Name type;
}

struct QName_key {
  1: string name;
  2: optional string fqn;
  3: optional string signature;
}

enum Modifier {
  abstract_ = 0,
  default_ = 1,
  final_ = 2,
  native_ = 3,
  private_ = 4,
  protected_ = 5,
  public_ = 6,
  static_ = 7,
  strictfp_ = 8,
  synchronized_ = 9,
  transient_ = 10,
  volatile_ = 11
} (hs.nounknown)

struct VariableDeclaration_2_key {
  1: Type_2 type;
  2: Name name;
  3: list<Modifier> modifiers;
  4: src.Loc loc;
}

struct VariableDeclaration_3_key {
  1: QName name;
  2: Type type;
  3: list<Modifier> modifiers;
  4: src.Loc loc;
}

struct VariableDeclaration_key {
  1: QName name;
  2: Type type;
  3: list<Annotation> annotations;
  4: list<Modifier> modifiers;
  5: src.Loc loc;
}

struct MethodDeclaration_key {
  1: QName name;
  2: list<VariableDeclaration> parameters;
  3: Type returnType;
  4: list<Annotation> annotations;
  5: list<Modifier> modifiers;
  6: list<TypeParam> typeParams;
  7: src.Loc loc;
}

struct MethodDeclaration_3_key {
  1: QName name;
  2: list<VariableDeclaration_3> parameters;
  3: Type returnType;
  4: list<Modifier> modifiers;
  5: list<TypeParam> typeParams;
  6: src.Loc loc;
}

struct MethodDeclaration_2_key {
  1: Name name;
  2: list<VariableDeclaration_2> parameters;
  3: Type_2 returnType;
  4: list<Modifier> modifiers;
  5: src.Loc loc;
}

struct InterfaceDeclaration_key {
  1: QName name;
  2: list<Annotation> annotations;
  3: list<Modifier> modifiers;
  4: list<Type> extends_ (java.swift.name = "extends_");
  5: list<MethodDeclaration> methods;
  6: list<TypeParam> typeParams;
  7: src.Loc loc;
}

struct InterfaceDeclaration_3_key {
  1: QName name;
  2: list<Modifier> modifiers;
  3: list<Type> extends_ (java.swift.name = "extends_");
  4: list<MethodDeclaration_3> methods;
  5: list<TypeParam> typeParams;
  6: src.Loc loc;
}

struct InterfaceDeclaration_2_key {
  1: Name name;
  2: list<Modifier> modifiers;
  3: list<Type_2> extends_ (java.swift.name = "extends_");
  4: list<MethodDeclaration_2> methods;
  5: src.Loc loc;
}

struct Inheritance_key {
  1: Type base;
  2: ClassDeclaration subclass;
}

struct Inheritance_3_key {
  1: Type base;
  2: ClassDeclaration_3 subclass;
}

struct FileXRefs_key {
  1: src.File file;
  2: list<XRef> xrefs;
}

struct ConstructorDeclaration_key {
  1: QName name;
  2: list<VariableDeclaration> parameters;
  3: list<Annotation> annotations;
  4: list<Modifier> modifiers;
  5: list<TypeParam> typeParams;
  6: src.Loc loc;
}

struct ConstructorDeclaration_3_key {
  1: QName name;
  2: list<VariableDeclaration_3> parameters;
  3: list<Modifier> modifiers;
  4: list<TypeParam> typeParams;
  5: src.Loc loc;
}

struct ConstructorDeclaration_2_key {
  1: list<VariableDeclaration_2> parameters;
  2: list<Modifier> modifiers;
  3: src.Loc loc;
}

struct ClassDeclaration_key {
  1: QName name;
  2: list<Modifier> modifiers;
  3: optional Type extends_ (java.swift.name = "extends_");
  4: list<Type> implements_ (java.swift.name = "implements_");
  5: list<Annotation> annotations;
  6: list<VariableDeclaration> variables;
  7: list<ConstructorDeclaration> constructors;
  8: list<MethodDeclaration> methods;
  9: list<TypeParam> typeParams;
  10: src.Loc loc;
}

struct ClassDeclaration_3_key {
  1: QName name;
  2: list<Modifier> modifiers;
  3: optional Type extends_ (java.swift.name = "extends_");
  4: list<Type> implements_ (java.swift.name = "implements_");
  5: list<VariableDeclaration_3> variables;
  6: list<ConstructorDeclaration_3> constructors;
  7: list<MethodDeclaration_3> methods;
  8: list<TypeParam> typeParams;
  9: src.Loc loc;
}

struct ClassDeclaration_2_key {
  1: Name name;
  2: list<Modifier> modifiers;
  3: optional Type_2 extends_ (java.swift.name = "extends_");
  4: list<Type_2> implements_ (java.swift.name = "implements_");
  5: list<VariableDeclaration_2> variables;
  6: list<ConstructorDeclaration_2> constructors;
  7: list<MethodDeclaration_2> methods;
  8: src.Loc loc;
}

struct Annotation_key {
  1: QName name;
}

struct AnnotatedClass_key {
  1: Annotation annotation;
  2: ClassDeclaration class_ (java.swift.name = "class_");
}
