# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


from glean.schema.java.types import (
    Name,
    ClassDeclaration,
    Path,
    InterfaceDeclaration,
    ImportDeclaration,
    EnumDeclaration,
    PackageDeclaration,
    ArrayType,
    AnnotatedClass,
    TypeParam,
    Annotation,
    XRefFile,
    MethodDeclaration,
    QName,
    VariableDeclaration,
    TypeVar,
    DeclaredType,
    TypeArg,
    FileXRefs,
    Inheritance,
    JavaFile,
    PrimitiveType,
    XRef,
    ConstructorDeclaration,
    Type,
)


class JavaName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"java.Name.6 {{ }}", Name
    return f"java.Name.6 {key}", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "JavaName":
    raise Exception("this function can only be called from @angle_query")

class JavaClassDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"java.ClassDeclaration.6 {{ }}", ClassDeclaration
    return f"java.ClassDeclaration.6 { concatenateFields(key) }", ClassDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, modifiers: Optional[Tuple[()]] = None, extends_: Optional[Tuple[()]] = None, implements_: Optional[Tuple[()]] = None, annotations: Optional[Tuple[()]] = None, variables: Optional[Tuple[()]] = None, constructors: Optional[Tuple[()]] = None, methods: Optional[Tuple[()]] = None, typeParams: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None, innerDefinitions: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaClassDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaPath(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"java.Path.6 {{ }}", Path
    return f"java.Path.6 { concatenateFields(key) }", Path

  @staticmethod
  def angle_query(*, base: Optional[Tuple[()]] = None, container: Optional[Tuple[()]] = None) -> "JavaPath":
    raise Exception("this function can only be called from @angle_query")

class JavaInterfaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"java.InterfaceDeclaration.6 {{ }}", InterfaceDeclaration
    return f"java.InterfaceDeclaration.6 { concatenateFields(key) }", InterfaceDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, annotations: Optional[Tuple[()]] = None, modifiers: Optional[Tuple[()]] = None, extends_: Optional[Tuple[()]] = None, methods: Optional[Tuple[()]] = None, typeParams: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None, variables: Optional[Tuple[()]] = None, innerDefinitions: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaInterfaceDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaImportDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"java.ImportDeclaration.6 {{ }}", ImportDeclaration
    return f"java.ImportDeclaration.6 { concatenateFields(key) }", ImportDeclaration

  @staticmethod
  def angle_query(*, name: Optional[str] = None, import_on_demand: Optional[bool] = None, static_member: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None, path: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaImportDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaEnumDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"java.EnumDeclaration.6 {{ }}", EnumDeclaration
    return f"java.EnumDeclaration.6 { concatenateFields(key) }", EnumDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, annotations: Optional[Tuple[()]] = None, modifiers: Optional[Tuple[()]] = None, implements_: Optional[Tuple[()]] = None, variables: Optional[Tuple[()]] = None, methods: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None, innerDefinitions: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaEnumDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaPackageDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"java.PackageDeclaration.6 {{ }}", PackageDeclaration
    return f"java.PackageDeclaration.6 { concatenateFields(key) }", PackageDeclaration

  @staticmethod
  def angle_query(*, name: Optional[str] = None, annotation: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None, path: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaPackageDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaArrayType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"java.ArrayType.6 {{ }}", ArrayType
    return f"java.ArrayType.6 { concatenateFields(key) }", ArrayType

  @staticmethod
  def angle_query(*, contents: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaArrayType":
    raise Exception("this function can only be called from @angle_query")

class JavaAnnotatedClass(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"java.AnnotatedClass.6 {{ }}", AnnotatedClass
    return f"java.AnnotatedClass.6 { concatenateFields(key) }", AnnotatedClass

  @staticmethod
  def angle_query(*, annotation: Optional[Tuple[()]] = None, class_: Optional[Tuple[()]] = None) -> "JavaAnnotatedClass":
    raise Exception("this function can only be called from @angle_query")

class JavaTypeParam(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"java.TypeParam.6 {{ }}", TypeParam
    return f"java.TypeParam.6 { concatenateFields(key) }", TypeParam

  @staticmethod
  def angle_query(*, name: Optional[str] = None, extends_: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaTypeParam":
    raise Exception("this function can only be called from @angle_query")

class JavaAnnotation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"java.Annotation.6 {{ }}", Annotation
    return f"java.Annotation.6 { concatenateFields(key) }", Annotation

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaAnnotation":
    raise Exception("this function can only be called from @angle_query")

class JavaXRefFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"java.XRefFile.6 {{ }}", XRefFile
    return f"java.XRefFile.6 { concatenateFields(key) }", XRefFile

  @staticmethod
  def angle_query(*, xref: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None) -> "JavaXRefFile":
    raise Exception("this function can only be called from @angle_query")

class JavaMethodDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"java.MethodDeclaration.6 {{ }}", MethodDeclaration
    return f"java.MethodDeclaration.6 { concatenateFields(key) }", MethodDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, parameters: Optional[Tuple[()]] = None, returnType: Optional[Tuple[()]] = None, annotations: Optional[Tuple[()]] = None, modifiers: Optional[Tuple[()]] = None, typeParams: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None, throws_: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaMethodDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"java.QName.6 {{ }}", QName
    return f"java.QName.6 { concatenateFields(key) }", QName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, fqn: Optional[Tuple[()]] = None, signature: Optional[Tuple[()]] = None, simple: Optional[Tuple[()]] = None, path: Optional[Tuple[()]] = None) -> "JavaQName":
    raise Exception("this function can only be called from @angle_query")

class JavaVariableDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"java.VariableDeclaration.6 {{ }}", VariableDeclaration
    return f"java.VariableDeclaration.6 { concatenateFields(key) }", VariableDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None, annotations: Optional[Tuple[()]] = None, modifiers: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaVariableDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaTypeVar(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"java.TypeVar.6 {{ }}", TypeVar
    return f"java.TypeVar.6 { concatenateFields(key) }", TypeVar

  @staticmethod
  def angle_query(*, type: Optional[str] = None, location: Optional[Tuple[()]] = None) -> "JavaTypeVar":
    raise Exception("this function can only be called from @angle_query")

class JavaDeclaredType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"java.DeclaredType.6 {{ }}", DeclaredType
    return f"java.DeclaredType.6 { concatenateFields(key) }", DeclaredType

  @staticmethod
  def angle_query(*, type: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaDeclaredType":
    raise Exception("this function can only be called from @angle_query")

class JavaTypeArg(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"java.TypeArg.6 {{ }}", TypeArg
    return f"java.TypeArg.6 {key}", TypeArg

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "JavaTypeArg":
    raise Exception("this function can only be called from @angle_query")

class JavaFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"java.FileXRefs.6 {{ }}", FileXRefs
    return f"java.FileXRefs.6 { concatenateFields(key) }", FileXRefs

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xrefs: Optional[Tuple[()]] = None) -> "JavaFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class JavaInheritance(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"java.Inheritance.6 {{ }}", Inheritance
    return f"java.Inheritance.6 { concatenateFields(key) }", Inheritance

  @staticmethod
  def angle_query(*, base: Optional[Tuple[()]] = None, subclass: Optional[Tuple[()]] = None) -> "JavaInheritance":
    raise Exception("this function can only be called from @angle_query")

class JavaJavaFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"java.JavaFile.6 {{ }}", JavaFile
    return f"java.JavaFile.6 { concatenateFields(key) }", JavaFile

  @staticmethod
  def angle_query(*, package_: Optional[Tuple[()]] = None, import_: Optional[Tuple[()]] = None, definition: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None) -> "JavaJavaFile":
    raise Exception("this function can only be called from @angle_query")

class JavaPrimitiveType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"java.PrimitiveType.6 {{ }}", PrimitiveType
    return f"java.PrimitiveType.6 { concatenateFields(key) }", PrimitiveType

  @staticmethod
  def angle_query(*, type: Optional[str] = None) -> "JavaPrimitiveType":
    raise Exception("this function can only be called from @angle_query")

class JavaXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"java.XRef.6 {{ }}", XRef
    return f"java.XRef.6 { concatenateFields(key) }", XRef

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, ranges: Optional[Tuple[()]] = None, xtarget: Optional[Tuple[()]] = None) -> "JavaXRef":
    raise Exception("this function can only be called from @angle_query")

class JavaConstructorDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"java.ConstructorDeclaration.6 {{ }}", ConstructorDeclaration
    return f"java.ConstructorDeclaration.6 { concatenateFields(key) }", ConstructorDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, parameters: Optional[Tuple[()]] = None, annotations: Optional[Tuple[()]] = None, modifiers: Optional[Tuple[()]] = None, typeParams: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None, throws_: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaConstructorDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"java.Type.6 {{ }}", Type
    return f"java.Type.6 { concatenateFields(key) }", Type

  @staticmethod
  def angle_query(*, type: Optional[str] = None, typeArgs: Optional[Tuple[()]] = None, stype: Optional[Tuple[()]] = None) -> "JavaType":
    raise Exception("this function can only be called from @angle_query")


