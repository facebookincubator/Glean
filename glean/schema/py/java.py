# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
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
    return f"java.Name.6 { json.dumps(key) }", Name

  @staticmethod
  def angle_query(*, name: str) -> "JavaName":
    raise Exception("this function can only be called from @angle_query")

class JavaClassDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.ClassDeclaration.6 { { } }", ClassDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "JavaClassDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaPath(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.Path.6 { { } }", Path

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "JavaPath":
    raise Exception("this function can only be called from @angle_query")

class JavaInterfaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.InterfaceDeclaration.6 { { } }", InterfaceDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "JavaInterfaceDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaImportDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.ImportDeclaration.6 { { } }", ImportDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "JavaImportDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaEnumDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.EnumDeclaration.6 { { } }", EnumDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "JavaEnumDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaPackageDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.PackageDeclaration.6 { { } }", PackageDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "JavaPackageDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaArrayType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.ArrayType.6 { { } }", ArrayType

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "JavaArrayType":
    raise Exception("this function can only be called from @angle_query")

class JavaAnnotatedClass(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.AnnotatedClass.6 { { } }", AnnotatedClass

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "JavaAnnotatedClass":
    raise Exception("this function can only be called from @angle_query")

class JavaTypeParam(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.TypeParam.6 { { } }", TypeParam

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "JavaTypeParam":
    raise Exception("this function can only be called from @angle_query")

class JavaAnnotation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.Annotation.6 { { } }", Annotation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "JavaAnnotation":
    raise Exception("this function can only be called from @angle_query")

class JavaXRefFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.XRefFile.6 { { } }", XRefFile

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "JavaXRefFile":
    raise Exception("this function can only be called from @angle_query")

class JavaMethodDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.MethodDeclaration.6 { { } }", MethodDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "JavaMethodDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.QName.6 { { } }", QName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "JavaQName":
    raise Exception("this function can only be called from @angle_query")

class JavaVariableDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.VariableDeclaration.6 { { } }", VariableDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "JavaVariableDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaTypeVar(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.TypeVar.6 { { } }", TypeVar

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "JavaTypeVar":
    raise Exception("this function can only be called from @angle_query")

class JavaDeclaredType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.DeclaredType.6 { { } }", DeclaredType

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "JavaDeclaredType":
    raise Exception("this function can only be called from @angle_query")

class JavaTypeArg(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.TypeArg.6 { json.dumps(key) }", TypeArg

  @staticmethod
  def angle_query(*, name: str) -> "JavaTypeArg":
    raise Exception("this function can only be called from @angle_query")

class JavaFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.FileXRefs.6 { { } }", FileXRefs

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "JavaFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class JavaInheritance(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.Inheritance.6 { { } }", Inheritance

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "JavaInheritance":
    raise Exception("this function can only be called from @angle_query")

class JavaJavaFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.JavaFile.6 { { } }", JavaFile

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "JavaJavaFile":
    raise Exception("this function can only be called from @angle_query")

class JavaPrimitiveType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.PrimitiveType.6 { { } }", PrimitiveType

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "JavaPrimitiveType":
    raise Exception("this function can only be called from @angle_query")

class JavaXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.XRef.6 { { } }", XRef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "JavaXRef":
    raise Exception("this function can only be called from @angle_query")

class JavaConstructorDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.ConstructorDeclaration.6 { { } }", ConstructorDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "JavaConstructorDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"java.Type.6 { { } }", Type

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "JavaType":
    raise Exception("this function can only be called from @angle_query")


