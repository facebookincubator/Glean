# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


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
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.Name.6 {json.dumps(key)}", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "JavaName":
    raise Exception("this function can only be called from @angle_query")

class JavaClassDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.ClassDeclaration.6 {{ name = _, modifiers = _, extends_ = _, implements_ = _, annotations = _, variables = _, constructors = _, methods = _, typeParams = _, loc = _, innerDefinitions = _, location = _ }}", ClassDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, modifiers: Optional[Tuple[()]] = None, extends_: Optional[Tuple[()]] = None, implements_: Optional[Tuple[()]] = None, annotations: Optional[Tuple[()]] = None, variables: Optional[Tuple[()]] = None, constructors: Optional[Tuple[()]] = None, methods: Optional[Tuple[()]] = None, typeParams: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None, innerDefinitions: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaClassDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaPath(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.Path.6 {{ base = _, container = _ }}", Path

  @staticmethod
  def angle_query(*, base: Optional[Tuple[()]] = None, container: Optional[Tuple[()]] = None) -> "JavaPath":
    raise Exception("this function can only be called from @angle_query")

class JavaInterfaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.InterfaceDeclaration.6 {{ name = _, annotations = _, modifiers = _, extends_ = _, methods = _, typeParams = _, loc = _, variables = _, innerDefinitions = _, location = _ }}", InterfaceDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, annotations: Optional[Tuple[()]] = None, modifiers: Optional[Tuple[()]] = None, extends_: Optional[Tuple[()]] = None, methods: Optional[Tuple[()]] = None, typeParams: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None, variables: Optional[Tuple[()]] = None, innerDefinitions: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaInterfaceDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaImportDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.ImportDeclaration.6 {{ name = _, import_on_demand = _, static_member = _, loc = _, path = _, location = _ }}", ImportDeclaration

  @staticmethod
  def angle_query(*, name: Optional[str] = None, import_on_demand: Optional[bool] = None, static_member: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None, path: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaImportDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaEnumDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.EnumDeclaration.6 {{ name = _, annotations = _, modifiers = _, implements_ = _, variables = _, methods = _, loc = _, innerDefinitions = _, location = _ }}", EnumDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, annotations: Optional[Tuple[()]] = None, modifiers: Optional[Tuple[()]] = None, implements_: Optional[Tuple[()]] = None, variables: Optional[Tuple[()]] = None, methods: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None, innerDefinitions: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaEnumDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaPackageDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.PackageDeclaration.6 {{ name = _, annotation = _, loc = _, path = _, location = _ }}", PackageDeclaration

  @staticmethod
  def angle_query(*, name: Optional[str] = None, annotation: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None, path: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaPackageDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaArrayType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.ArrayType.6 {{ contents = _, location = _ }}", ArrayType

  @staticmethod
  def angle_query(*, contents: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaArrayType":
    raise Exception("this function can only be called from @angle_query")

class JavaAnnotatedClass(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.AnnotatedClass.6 {{ annotation = _, class_ = _ }}", AnnotatedClass

  @staticmethod
  def angle_query(*, annotation: Optional[Tuple[()]] = None, class_: Optional[Tuple[()]] = None) -> "JavaAnnotatedClass":
    raise Exception("this function can only be called from @angle_query")

class JavaTypeParam(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.TypeParam.6 {{ name = _, extends_ = _, location = _ }}", TypeParam

  @staticmethod
  def angle_query(*, name: Optional[str] = None, extends_: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaTypeParam":
    raise Exception("this function can only be called from @angle_query")

class JavaAnnotation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.Annotation.6 {{ name = _, location = _ }}", Annotation

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaAnnotation":
    raise Exception("this function can only be called from @angle_query")

class JavaXRefFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.XRefFile.6 {{ xref = _, file = _ }}", XRefFile

  @staticmethod
  def angle_query(*, xref: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None) -> "JavaXRefFile":
    raise Exception("this function can only be called from @angle_query")

class JavaMethodDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.MethodDeclaration.6 {{ name = _, parameters = _, returnType = _, annotations = _, modifiers = _, typeParams = _, loc = _, throws_ = _, location = _ }}", MethodDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, parameters: Optional[Tuple[()]] = None, returnType: Optional[Tuple[()]] = None, annotations: Optional[Tuple[()]] = None, modifiers: Optional[Tuple[()]] = None, typeParams: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None, throws_: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaMethodDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.QName.6 {{ name = _, fqn = _, signature = _, simple = _, path = _ }}", QName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, fqn: Optional[Tuple[()]] = None, signature: Optional[Tuple[()]] = None, simple: Optional[Tuple[()]] = None, path: Optional[Tuple[()]] = None) -> "JavaQName":
    raise Exception("this function can only be called from @angle_query")

class JavaVariableDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.VariableDeclaration.6 {{ name = _, type = _, annotations = _, modifiers = _, loc = _, location = _ }}", VariableDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None, annotations: Optional[Tuple[()]] = None, modifiers: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaVariableDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaTypeVar(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.TypeVar.6 {{ type = _, location = _ }}", TypeVar

  @staticmethod
  def angle_query(*, type: Optional[str] = None, location: Optional[Tuple[()]] = None) -> "JavaTypeVar":
    raise Exception("this function can only be called from @angle_query")

class JavaDeclaredType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.DeclaredType.6 {{ type = _, location = _ }}", DeclaredType

  @staticmethod
  def angle_query(*, type: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaDeclaredType":
    raise Exception("this function can only be called from @angle_query")

class JavaTypeArg(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.TypeArg.6 {json.dumps(key)}", TypeArg

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "JavaTypeArg":
    raise Exception("this function can only be called from @angle_query")

class JavaFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.FileXRefs.6 {{ file = _, xrefs = _ }}", FileXRefs

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xrefs: Optional[Tuple[()]] = None) -> "JavaFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class JavaInheritance(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.Inheritance.6 {{ base = _, subclass = _ }}", Inheritance

  @staticmethod
  def angle_query(*, base: Optional[Tuple[()]] = None, subclass: Optional[Tuple[()]] = None) -> "JavaInheritance":
    raise Exception("this function can only be called from @angle_query")

class JavaJavaFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.JavaFile.6 {{ package_ = _, import_ = _, definition = _, file = _ }}", JavaFile

  @staticmethod
  def angle_query(*, package_: Optional[Tuple[()]] = None, import_: Optional[Tuple[()]] = None, definition: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None) -> "JavaJavaFile":
    raise Exception("this function can only be called from @angle_query")

class JavaPrimitiveType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.PrimitiveType.6 {{ type = _ }}", PrimitiveType

  @staticmethod
  def angle_query(*, type: Optional[str] = None) -> "JavaPrimitiveType":
    raise Exception("this function can only be called from @angle_query")

class JavaXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.XRef.6 {{ target = _, ranges = _, xtarget = _ }}", XRef

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, ranges: Optional[Tuple[()]] = None, xtarget: Optional[Tuple[()]] = None) -> "JavaXRef":
    raise Exception("this function can only be called from @angle_query")

class JavaConstructorDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.ConstructorDeclaration.6 {{ name = _, parameters = _, annotations = _, modifiers = _, typeParams = _, loc = _, throws_ = _, location = _ }}", ConstructorDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, parameters: Optional[Tuple[()]] = None, annotations: Optional[Tuple[()]] = None, modifiers: Optional[Tuple[()]] = None, typeParams: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None, throws_: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaConstructorDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.Type.6 {{ type = _, typeArgs = _, stype = _ }}", Type

  @staticmethod
  def angle_query(*, type: Optional[str] = None, typeArgs: Optional[Tuple[()]] = None, stype: Optional[Tuple[()]] = None) -> "JavaType":
    raise Exception("this function can only be called from @angle_query")


