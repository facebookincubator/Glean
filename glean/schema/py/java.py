# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSJavaName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"java.Name.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSJavaName":
    raise Exception("this function can only be called from @angle_query")

class GSJavaClassDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"java.ClassDeclaration.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSJavaClassDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSJavaPath(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"java.Path.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSJavaPath":
    raise Exception("this function can only be called from @angle_query")

class GSJavaInterfaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"java.InterfaceDeclaration.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSJavaInterfaceDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSJavaImportDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"java.ImportDeclaration.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSJavaImportDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSJavaEnumDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"java.EnumDeclaration.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSJavaEnumDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSJavaPackageDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"java.PackageDeclaration.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSJavaPackageDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSJavaArrayType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"java.ArrayType.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSJavaArrayType":
    raise Exception("this function can only be called from @angle_query")

class GSJavaAnnotatedClass(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"java.AnnotatedClass.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSJavaAnnotatedClass":
    raise Exception("this function can only be called from @angle_query")

class GSJavaTypeParam(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"java.TypeParam.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSJavaTypeParam":
    raise Exception("this function can only be called from @angle_query")

class GSJavaAnnotation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"java.Annotation.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSJavaAnnotation":
    raise Exception("this function can only be called from @angle_query")

class GSJavaXRefFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"java.XRefFile.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSJavaXRefFile":
    raise Exception("this function can only be called from @angle_query")

class GSJavaMethodDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"java.MethodDeclaration.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSJavaMethodDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSJavaQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"java.QName.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSJavaQName":
    raise Exception("this function can only be called from @angle_query")

class GSJavaVariableDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"java.VariableDeclaration.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSJavaVariableDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSJavaTypeVar(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"java.TypeVar.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSJavaTypeVar":
    raise Exception("this function can only be called from @angle_query")

class GSJavaDeclaredType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"java.DeclaredType.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSJavaDeclaredType":
    raise Exception("this function can only be called from @angle_query")

class GSJavaTypeArg(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"java.TypeArg.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSJavaTypeArg":
    raise Exception("this function can only be called from @angle_query")

class GSJavaFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"java.FileXRefs.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSJavaFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class GSJavaInheritance(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"java.Inheritance.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSJavaInheritance":
    raise Exception("this function can only be called from @angle_query")

class GSJavaJavaFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"java.JavaFile.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSJavaJavaFile":
    raise Exception("this function can only be called from @angle_query")

class GSJavaPrimitiveType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"java.PrimitiveType.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSJavaPrimitiveType":
    raise Exception("this function can only be called from @angle_query")

class GSJavaXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"java.XRef.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSJavaXRef":
    raise Exception("this function can only be called from @angle_query")

class GSJavaConstructorDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"java.ConstructorDeclaration.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSJavaConstructorDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSJavaType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"java.Type.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSJavaType":
    raise Exception("this function can only be called from @angle_query")


