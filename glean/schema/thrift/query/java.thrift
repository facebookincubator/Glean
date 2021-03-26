// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/thrift/query/builtin.thrift"
include "glean/schema/thrift/query/src.thrift"

namespace cpp2 facebook.glean.schema.query.java
namespace hs Glean.Schema.Query
namespace php glean_schema_query_java
namespace py glean.schema.query.java
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.java
namespace rust glean_schema_query_java

hs_include "glean/schema/thrift/query/java_include.hs"
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
union XRef {
  1: XRef_id id (hs.strict);
  2: XRef_key key;
  3: builtin.Unit get;
} (hs.prefix = "XRef_with_")

typedef glean.Id TypeParam_id

@glean.PredicateAnnotation{
  name="java.TypeParam";
  version=3;
}
union TypeParam {
  1: TypeParam_id id (hs.strict);
  2: TypeParam_key key;
  3: builtin.Unit get;
} (hs.prefix = "TypeParam_with_")

typedef glean.Id TypeArg_id

@glean.PredicateAnnotation{
  name="java.TypeArg";
  version=3;
}
union TypeArg {
  1: TypeArg_id id (hs.strict);
  2: TypeArg_key key;
  3: builtin.Unit get;
} (hs.prefix = "TypeArg_with_")

typedef glean.Id Type_id

@glean.PredicateAnnotation{
  name="java.Type";
  version=3;
}
union Type_ {
  1: Type_id id (hs.strict);
  2: Type_key key;
  3: builtin.Unit get;
} (hs.prefix = "Type__with_")

typedef glean.Id Type_2_id

@glean.PredicateAnnotation{
  name="java.Type";
  version=2;
}
union Type_2 {
  1: Type_2_id id (hs.strict);
  2: Type_2_key key;
  3: builtin.Unit get;
} (hs.prefix = "Type_2_with_")

typedef glean.Id QName_id

@glean.PredicateAnnotation{
  name="java.QName";
  version=3;
}
union QName {
  1: QName_id id (hs.strict);
  2: QName_key key;
  3: builtin.Unit get;
} (hs.prefix = "QName_with_")

typedef glean.Id Name_id

@glean.PredicateAnnotation{
  name="java.Name";
  version=2;
}
union Name {
  1: Name_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "Name_with_")

typedef glean.Id VariableDeclaration_2_id

@glean.PredicateAnnotation{
  name="java.VariableDeclaration";
  version=2;
}
union VariableDeclaration_2 {
  1: VariableDeclaration_2_id id (hs.strict);
  2: VariableDeclaration_2_key key;
  3: builtin.Unit get;
} (hs.prefix = "VariableDeclaration_2_with_")

typedef glean.Id VariableDeclaration_3_id

@glean.PredicateAnnotation{
  name="java.VariableDeclaration";
  version=3;
}
union VariableDeclaration_3 {
  1: VariableDeclaration_3_id id (hs.strict);
  2: VariableDeclaration_3_key key;
  3: builtin.Unit get;
} (hs.prefix = "VariableDeclaration_3_with_")

typedef glean.Id VariableDeclaration_id

@glean.PredicateAnnotation{
  name="java.VariableDeclaration";
  version=4;
}
union VariableDeclaration {
  1: VariableDeclaration_id id (hs.strict);
  2: VariableDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "VariableDeclaration_with_")

typedef glean.Id MethodDeclaration_id

@glean.PredicateAnnotation{
  name="java.MethodDeclaration";
  version=4;
}
union MethodDeclaration {
  1: MethodDeclaration_id id (hs.strict);
  2: MethodDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "MethodDeclaration_with_")

typedef glean.Id MethodDeclaration_3_id

@glean.PredicateAnnotation{
  name="java.MethodDeclaration";
  version=3;
}
union MethodDeclaration_3 {
  1: MethodDeclaration_3_id id (hs.strict);
  2: MethodDeclaration_3_key key;
  3: builtin.Unit get;
} (hs.prefix = "MethodDeclaration_3_with_")

typedef glean.Id MethodDeclaration_2_id

@glean.PredicateAnnotation{
  name="java.MethodDeclaration";
  version=2;
}
union MethodDeclaration_2 {
  1: MethodDeclaration_2_id id (hs.strict);
  2: MethodDeclaration_2_key key;
  3: builtin.Unit get;
} (hs.prefix = "MethodDeclaration_2_with_")

typedef glean.Id InterfaceDeclaration_id

@glean.PredicateAnnotation{
  name="java.InterfaceDeclaration";
  version=4;
}
union InterfaceDeclaration {
  1: InterfaceDeclaration_id id (hs.strict);
  2: InterfaceDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "InterfaceDeclaration_with_")

typedef glean.Id InterfaceDeclaration_3_id

@glean.PredicateAnnotation{
  name="java.InterfaceDeclaration";
  version=3;
}
union InterfaceDeclaration_3 {
  1: InterfaceDeclaration_3_id id (hs.strict);
  2: InterfaceDeclaration_3_key key;
  3: builtin.Unit get;
} (hs.prefix = "InterfaceDeclaration_3_with_")

typedef glean.Id InterfaceDeclaration_2_id

@glean.PredicateAnnotation{
  name="java.InterfaceDeclaration";
  version=2;
}
union InterfaceDeclaration_2 {
  1: InterfaceDeclaration_2_id id (hs.strict);
  2: InterfaceDeclaration_2_key key;
  3: builtin.Unit get;
} (hs.prefix = "InterfaceDeclaration_2_with_")

typedef glean.Id Inheritance_id

@glean.PredicateAnnotation{
  name="java.Inheritance";
  version=4;
}
union Inheritance {
  1: Inheritance_id id (hs.strict);
  2: Inheritance_key key;
  3: builtin.Unit get;
} (hs.prefix = "Inheritance_with_")

typedef glean.Id Inheritance_3_id

@glean.PredicateAnnotation{
  name="java.Inheritance";
  version=3;
}
union Inheritance_3 {
  1: Inheritance_3_id id (hs.strict);
  2: Inheritance_3_key key;
  3: builtin.Unit get;
} (hs.prefix = "Inheritance_3_with_")

typedef glean.Id FileXRefs_id

@glean.PredicateAnnotation{
  name="java.FileXRefs";
  version=3;
}
union FileXRefs {
  1: FileXRefs_id id (hs.strict);
  2: FileXRefs_key key;
  3: builtin.Unit get;
} (hs.prefix = "FileXRefs_with_")

typedef glean.Id ConstructorDeclaration_id

@glean.PredicateAnnotation{
  name="java.ConstructorDeclaration";
  version=4;
}
union ConstructorDeclaration {
  1: ConstructorDeclaration_id id (hs.strict);
  2: ConstructorDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "ConstructorDeclaration_with_")

typedef glean.Id ConstructorDeclaration_3_id

@glean.PredicateAnnotation{
  name="java.ConstructorDeclaration";
  version=3;
}
union ConstructorDeclaration_3 {
  1: ConstructorDeclaration_3_id id (hs.strict);
  2: ConstructorDeclaration_3_key key;
  3: builtin.Unit get;
} (hs.prefix = "ConstructorDeclaration_3_with_")

typedef glean.Id ConstructorDeclaration_2_id

@glean.PredicateAnnotation{
  name="java.ConstructorDeclaration";
  version=2;
}
union ConstructorDeclaration_2 {
  1: ConstructorDeclaration_2_id id (hs.strict);
  2: ConstructorDeclaration_2_key key;
  3: builtin.Unit get;
} (hs.prefix = "ConstructorDeclaration_2_with_")

typedef glean.Id ClassDeclaration_id

@glean.PredicateAnnotation{
  name="java.ClassDeclaration";
  version=4;
}
union ClassDeclaration {
  1: ClassDeclaration_id id (hs.strict);
  2: ClassDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "ClassDeclaration_with_")

typedef glean.Id ClassDeclaration_3_id

@glean.PredicateAnnotation{
  name="java.ClassDeclaration";
  version=3;
}
union ClassDeclaration_3 {
  1: ClassDeclaration_3_id id (hs.strict);
  2: ClassDeclaration_3_key key;
  3: builtin.Unit get;
} (hs.prefix = "ClassDeclaration_3_with_")

typedef glean.Id ClassDeclaration_2_id

@glean.PredicateAnnotation{
  name="java.ClassDeclaration";
  version=2;
}
union ClassDeclaration_2 {
  1: ClassDeclaration_2_id id (hs.strict);
  2: ClassDeclaration_2_key key;
  3: builtin.Unit get;
} (hs.prefix = "ClassDeclaration_2_with_")

typedef glean.Id Annotation_id

@glean.PredicateAnnotation{
  name="java.Annotation";
  version=4;
}
union Annotation {
  1: Annotation_id id (hs.strict);
  2: Annotation_key key;
  3: builtin.Unit get;
} (hs.prefix = "Annotation_with_")

typedef glean.Id AnnotatedClass_id

@glean.PredicateAnnotation{
  name="java.AnnotatedClass";
  version=4;
}
union AnnotatedClass {
  1: AnnotatedClass_id id (hs.strict);
  2: AnnotatedClass_key key;
  3: builtin.Unit get;
} (hs.prefix = "AnnotatedClass_with_")

struct XRefTarget {
  1: optional QName class_or_interface_ (java.swift.name = "class_or_interface_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional QName ctor_ (java.swift.name = "ctor_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional QName method_ (java.swift.name = "method_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: bool any = false;
}

union XRef_ranges_array {
  1: src.ByteSpan every;
  2: list<src.ByteSpan> exact;
}

struct XRef_key {
  1: optional XRefTarget target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional XRef_ranges_array ranges (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Wildcard {
  1: optional Type_ extends_ (java.swift.name = "extends_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_ super_ (java.swift.name = "super_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional bool unbounded;
  4: bool any = false;
}

union TypeParam_extends__array {
  1: Type_ every;
  2: list<Type_> exact;
}

struct TypeParam_key {
  1: optional string name;
  2: optional TypeParam_extends__array extends_ (java.swift.name = "extends_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct TypeArg_key {
  1: optional Type_ type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Wildcard wildcard (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

union Type_typeArgs_array {
  1: TypeArg every;
  2: list<TypeArg> exact;
}

struct Type_key {
  1: optional string type;
  2: optional Type_typeArgs_array typeArgs (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Type_2_key {
  1: optional Name type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct QName_fqn {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional string just;
  3: bool any = false;
}

struct QName_signature {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional string just;
  3: bool any = false;
}

struct QName_key {
  1: optional string name;
  2: optional QName_fqn fqn;
  3: optional QName_signature signature;
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

union VariableDeclaration_2_modifiers_array {
  1: Modifier every;
  2: list<Modifier> exact;
}

struct VariableDeclaration_2_key {
  1: optional Type_2 type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional VariableDeclaration_2_modifiers_array modifiers;
  4: optional src.Loc loc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union VariableDeclaration_3_modifiers_array {
  1: Modifier every;
  2: list<Modifier> exact;
}

struct VariableDeclaration_3_key {
  1: optional QName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_ type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional VariableDeclaration_3_modifiers_array modifiers;
  4: optional src.Loc loc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union VariableDeclaration_annotations_array {
  1: Annotation every;
  2: list<Annotation> exact;
}

union VariableDeclaration_modifiers_array {
  1: Modifier every;
  2: list<Modifier> exact;
}

struct VariableDeclaration_key {
  1: optional QName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_ type (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional VariableDeclaration_annotations_array annotations (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional VariableDeclaration_modifiers_array modifiers;
  5: optional src.Loc loc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union MethodDeclaration_parameters_array {
  1: VariableDeclaration every;
  2: list<VariableDeclaration> exact;
}

union MethodDeclaration_annotations_array {
  1: Annotation every;
  2: list<Annotation> exact;
}

union MethodDeclaration_modifiers_array {
  1: Modifier every;
  2: list<Modifier> exact;
}

union MethodDeclaration_typeParams_array {
  1: TypeParam every;
  2: list<TypeParam> exact;
}

struct MethodDeclaration_key {
  1: optional QName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional MethodDeclaration_parameters_array parameters (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional Type_ returnType (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional MethodDeclaration_annotations_array annotations (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional MethodDeclaration_modifiers_array modifiers;
  6: optional MethodDeclaration_typeParams_array typeParams (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  7: optional src.Loc loc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union MethodDeclaration_3_parameters_array {
  1: VariableDeclaration_3 every;
  2: list<VariableDeclaration_3> exact;
}

union MethodDeclaration_3_modifiers_array {
  1: Modifier every;
  2: list<Modifier> exact;
}

union MethodDeclaration_3_typeParams_array {
  1: TypeParam every;
  2: list<TypeParam> exact;
}

struct MethodDeclaration_3_key {
  1: optional QName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional MethodDeclaration_3_parameters_array parameters (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional Type_ returnType (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional MethodDeclaration_3_modifiers_array modifiers;
  5: optional MethodDeclaration_3_typeParams_array typeParams (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: optional src.Loc loc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union MethodDeclaration_2_parameters_array {
  1: VariableDeclaration_2 every;
  2: list<VariableDeclaration_2> exact;
}

union MethodDeclaration_2_modifiers_array {
  1: Modifier every;
  2: list<Modifier> exact;
}

struct MethodDeclaration_2_key {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional MethodDeclaration_2_parameters_array parameters (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional Type_2 returnType (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional MethodDeclaration_2_modifiers_array modifiers;
  5: optional src.Loc loc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union InterfaceDeclaration_annotations_array {
  1: Annotation every;
  2: list<Annotation> exact;
}

union InterfaceDeclaration_modifiers_array {
  1: Modifier every;
  2: list<Modifier> exact;
}

union InterfaceDeclaration_extends__array {
  1: Type_ every;
  2: list<Type_> exact;
}

union InterfaceDeclaration_methods_array {
  1: MethodDeclaration every;
  2: list<MethodDeclaration> exact;
}

union InterfaceDeclaration_typeParams_array {
  1: TypeParam every;
  2: list<TypeParam> exact;
}

struct InterfaceDeclaration_key {
  1: optional QName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional InterfaceDeclaration_annotations_array annotations (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional InterfaceDeclaration_modifiers_array modifiers;
  4: optional InterfaceDeclaration_extends__array extends_ (java.swift.name = "extends_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional InterfaceDeclaration_methods_array methods (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: optional InterfaceDeclaration_typeParams_array typeParams (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  7: optional src.Loc loc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union InterfaceDeclaration_3_modifiers_array {
  1: Modifier every;
  2: list<Modifier> exact;
}

union InterfaceDeclaration_3_extends__array {
  1: Type_ every;
  2: list<Type_> exact;
}

union InterfaceDeclaration_3_methods_array {
  1: MethodDeclaration_3 every;
  2: list<MethodDeclaration_3> exact;
}

union InterfaceDeclaration_3_typeParams_array {
  1: TypeParam every;
  2: list<TypeParam> exact;
}

struct InterfaceDeclaration_3_key {
  1: optional QName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional InterfaceDeclaration_3_modifiers_array modifiers;
  3: optional InterfaceDeclaration_3_extends__array extends_ (java.swift.name = "extends_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional InterfaceDeclaration_3_methods_array methods (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional InterfaceDeclaration_3_typeParams_array typeParams (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: optional src.Loc loc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union InterfaceDeclaration_2_modifiers_array {
  1: Modifier every;
  2: list<Modifier> exact;
}

union InterfaceDeclaration_2_extends__array {
  1: Type_2 every;
  2: list<Type_2> exact;
}

union InterfaceDeclaration_2_methods_array {
  1: MethodDeclaration_2 every;
  2: list<MethodDeclaration_2> exact;
}

struct InterfaceDeclaration_2_key {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional InterfaceDeclaration_2_modifiers_array modifiers;
  3: optional InterfaceDeclaration_2_extends__array extends_ (java.swift.name = "extends_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional InterfaceDeclaration_2_methods_array methods (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional src.Loc loc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Inheritance_key {
  1: optional Type_ base (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ClassDeclaration subclass (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Inheritance_3_key {
  1: optional Type_ base (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ClassDeclaration_3 subclass (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union FileXRefs_xrefs_array {
  1: XRef every;
  2: list<XRef> exact;
}

struct FileXRefs_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional FileXRefs_xrefs_array xrefs (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union ConstructorDeclaration_parameters_array {
  1: VariableDeclaration every;
  2: list<VariableDeclaration> exact;
}

union ConstructorDeclaration_annotations_array {
  1: Annotation every;
  2: list<Annotation> exact;
}

union ConstructorDeclaration_modifiers_array {
  1: Modifier every;
  2: list<Modifier> exact;
}

union ConstructorDeclaration_typeParams_array {
  1: TypeParam every;
  2: list<TypeParam> exact;
}

struct ConstructorDeclaration_key {
  1: optional QName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ConstructorDeclaration_parameters_array parameters (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional ConstructorDeclaration_annotations_array annotations (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional ConstructorDeclaration_modifiers_array modifiers;
  5: optional ConstructorDeclaration_typeParams_array typeParams (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: optional src.Loc loc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union ConstructorDeclaration_3_parameters_array {
  1: VariableDeclaration_3 every;
  2: list<VariableDeclaration_3> exact;
}

union ConstructorDeclaration_3_modifiers_array {
  1: Modifier every;
  2: list<Modifier> exact;
}

union ConstructorDeclaration_3_typeParams_array {
  1: TypeParam every;
  2: list<TypeParam> exact;
}

struct ConstructorDeclaration_3_key {
  1: optional QName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ConstructorDeclaration_3_parameters_array parameters (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional ConstructorDeclaration_3_modifiers_array modifiers;
  4: optional ConstructorDeclaration_3_typeParams_array typeParams (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional src.Loc loc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union ConstructorDeclaration_2_parameters_array {
  1: VariableDeclaration_2 every;
  2: list<VariableDeclaration_2> exact;
}

union ConstructorDeclaration_2_modifiers_array {
  1: Modifier every;
  2: list<Modifier> exact;
}

struct ConstructorDeclaration_2_key {
  1: optional ConstructorDeclaration_2_parameters_array parameters (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ConstructorDeclaration_2_modifiers_array modifiers;
  3: optional src.Loc loc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union ClassDeclaration_modifiers_array {
  1: Modifier every;
  2: list<Modifier> exact;
}

struct ClassDeclaration_extends_ {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_ just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

union ClassDeclaration_implements__array {
  1: Type_ every;
  2: list<Type_> exact;
}

union ClassDeclaration_annotations_array {
  1: Annotation every;
  2: list<Annotation> exact;
}

union ClassDeclaration_variables_array {
  1: VariableDeclaration every;
  2: list<VariableDeclaration> exact;
}

union ClassDeclaration_constructors_array {
  1: ConstructorDeclaration every;
  2: list<ConstructorDeclaration> exact;
}

union ClassDeclaration_methods_array {
  1: MethodDeclaration every;
  2: list<MethodDeclaration> exact;
}

union ClassDeclaration_typeParams_array {
  1: TypeParam every;
  2: list<TypeParam> exact;
}

struct ClassDeclaration_key {
  1: optional QName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ClassDeclaration_modifiers_array modifiers;
  3: optional ClassDeclaration_extends_ extends_ (java.swift.name = "extends_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional ClassDeclaration_implements__array implements_ (java.swift.name = "implements_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional ClassDeclaration_annotations_array annotations (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: optional ClassDeclaration_variables_array variables (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  7: optional ClassDeclaration_constructors_array constructors (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  8: optional ClassDeclaration_methods_array methods (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  9: optional ClassDeclaration_typeParams_array typeParams (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  10: optional src.Loc loc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union ClassDeclaration_3_modifiers_array {
  1: Modifier every;
  2: list<Modifier> exact;
}

struct ClassDeclaration_3_extends_ {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_ just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

union ClassDeclaration_3_implements__array {
  1: Type_ every;
  2: list<Type_> exact;
}

union ClassDeclaration_3_variables_array {
  1: VariableDeclaration_3 every;
  2: list<VariableDeclaration_3> exact;
}

union ClassDeclaration_3_constructors_array {
  1: ConstructorDeclaration_3 every;
  2: list<ConstructorDeclaration_3> exact;
}

union ClassDeclaration_3_methods_array {
  1: MethodDeclaration_3 every;
  2: list<MethodDeclaration_3> exact;
}

union ClassDeclaration_3_typeParams_array {
  1: TypeParam every;
  2: list<TypeParam> exact;
}

struct ClassDeclaration_3_key {
  1: optional QName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ClassDeclaration_3_modifiers_array modifiers;
  3: optional ClassDeclaration_3_extends_ extends_ (java.swift.name = "extends_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional ClassDeclaration_3_implements__array implements_ (java.swift.name = "implements_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional ClassDeclaration_3_variables_array variables (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: optional ClassDeclaration_3_constructors_array constructors (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  7: optional ClassDeclaration_3_methods_array methods (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  8: optional ClassDeclaration_3_typeParams_array typeParams (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  9: optional src.Loc loc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union ClassDeclaration_2_modifiers_array {
  1: Modifier every;
  2: list<Modifier> exact;
}

struct ClassDeclaration_2_extends_ {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_2 just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

union ClassDeclaration_2_implements__array {
  1: Type_2 every;
  2: list<Type_2> exact;
}

union ClassDeclaration_2_variables_array {
  1: VariableDeclaration_2 every;
  2: list<VariableDeclaration_2> exact;
}

union ClassDeclaration_2_constructors_array {
  1: ConstructorDeclaration_2 every;
  2: list<ConstructorDeclaration_2> exact;
}

union ClassDeclaration_2_methods_array {
  1: MethodDeclaration_2 every;
  2: list<MethodDeclaration_2> exact;
}

struct ClassDeclaration_2_key {
  1: optional Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ClassDeclaration_2_modifiers_array modifiers;
  3: optional ClassDeclaration_2_extends_ extends_ (java.swift.name = "extends_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional ClassDeclaration_2_implements__array implements_ (java.swift.name = "implements_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional ClassDeclaration_2_variables_array variables (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: optional ClassDeclaration_2_constructors_array constructors (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  7: optional ClassDeclaration_2_methods_array methods (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  8: optional src.Loc loc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Annotation_key {
  1: optional QName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct AnnotatedClass_key {
  1: optional Annotation annotation (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ClassDeclaration class_ (java.swift.name = "class_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}
