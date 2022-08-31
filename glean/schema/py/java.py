# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.src import *


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
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"java.Name.6 {angle_for(__env, arg)}", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "JavaName":
    raise Exception("this function can only be called from @angle_query")

class JavaClassDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, modifiers: ast.Expr, extends_: ast.Expr, implements_: ast.Expr, annotations: ast.Expr, variables: ast.Expr, constructors: ast.Expr, methods: ast.Expr, typeParams: ast.Expr, loc: ast.Expr, innerDefinitions: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.ClassDeclaration.6 {{ name = {angle_for(__env, name)}, modifiers = {angle_for(__env, modifiers)}, extends_ = {angle_for(__env, extends_)}, implements_ = {angle_for(__env, implements_)}, annotations = {angle_for(__env, annotations)}, variables = {angle_for(__env, variables)}, constructors = {angle_for(__env, constructors)}, methods = {angle_for(__env, methods)}, typeParams = {angle_for(__env, typeParams)}, loc = {angle_for(__env, loc)}, innerDefinitions = {angle_for(__env, innerDefinitions)}, location = {angle_for(__env, location)} }}", ClassDeclaration

  @staticmethod
  def angle_query(*, name: Optional["JavaQName"] = None, modifiers: Optional[Tuple[()]] = None, extends_: Optional[Tuple[()]] = None, implements_: Optional[Tuple[()]] = None, annotations: Optional[Tuple[()]] = None, variables: Optional[Tuple[()]] = None, constructors: Optional[Tuple[()]] = None, methods: Optional[Tuple[()]] = None, typeParams: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None, innerDefinitions: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaClassDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaPath(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], base: ast.Expr, container: ast.Expr) -> Tuple[str, Struct]:
    return f"java.Path.6 {{ base = {angle_for(__env, base)}, container = {angle_for(__env, container)} }}", Path

  @staticmethod
  def angle_query(*, base: Optional["JavaName"] = None, container: Optional[Tuple[()]] = None) -> "JavaPath":
    raise Exception("this function can only be called from @angle_query")

class JavaInterfaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, annotations: ast.Expr, modifiers: ast.Expr, extends_: ast.Expr, methods: ast.Expr, typeParams: ast.Expr, loc: ast.Expr, variables: ast.Expr, innerDefinitions: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.InterfaceDeclaration.6 {{ name = {angle_for(__env, name)}, annotations = {angle_for(__env, annotations)}, modifiers = {angle_for(__env, modifiers)}, extends_ = {angle_for(__env, extends_)}, methods = {angle_for(__env, methods)}, typeParams = {angle_for(__env, typeParams)}, loc = {angle_for(__env, loc)}, variables = {angle_for(__env, variables)}, innerDefinitions = {angle_for(__env, innerDefinitions)}, location = {angle_for(__env, location)} }}", InterfaceDeclaration

  @staticmethod
  def angle_query(*, name: Optional["JavaQName"] = None, annotations: Optional[Tuple[()]] = None, modifiers: Optional[Tuple[()]] = None, extends_: Optional[Tuple[()]] = None, methods: Optional[Tuple[()]] = None, typeParams: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None, variables: Optional[Tuple[()]] = None, innerDefinitions: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaInterfaceDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaImportDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, import_on_demand: ast.Expr, static_member: ast.Expr, loc: ast.Expr, path: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.ImportDeclaration.6 {{ name = {angle_for(__env, name)}, import_on_demand = {angle_for(__env, import_on_demand)}, static_member = {angle_for(__env, static_member)}, loc = {angle_for(__env, loc)}, path = {angle_for(__env, path)}, location = {angle_for(__env, location)} }}", ImportDeclaration

  @staticmethod
  def angle_query(*, name: Optional[str] = None, import_on_demand: Optional[bool] = None, static_member: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None, path: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaImportDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaEnumDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, annotations: ast.Expr, modifiers: ast.Expr, implements_: ast.Expr, variables: ast.Expr, methods: ast.Expr, loc: ast.Expr, innerDefinitions: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.EnumDeclaration.6 {{ name = {angle_for(__env, name)}, annotations = {angle_for(__env, annotations)}, modifiers = {angle_for(__env, modifiers)}, implements_ = {angle_for(__env, implements_)}, variables = {angle_for(__env, variables)}, methods = {angle_for(__env, methods)}, loc = {angle_for(__env, loc)}, innerDefinitions = {angle_for(__env, innerDefinitions)}, location = {angle_for(__env, location)} }}", EnumDeclaration

  @staticmethod
  def angle_query(*, name: Optional["JavaQName"] = None, annotations: Optional[Tuple[()]] = None, modifiers: Optional[Tuple[()]] = None, implements_: Optional[Tuple[()]] = None, variables: Optional[Tuple[()]] = None, methods: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None, innerDefinitions: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaEnumDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaPackageDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, annotation: ast.Expr, loc: ast.Expr, path: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.PackageDeclaration.6 {{ name = {angle_for(__env, name)}, annotation = {angle_for(__env, annotation)}, loc = {angle_for(__env, loc)}, path = {angle_for(__env, path)}, location = {angle_for(__env, location)} }}", PackageDeclaration

  @staticmethod
  def angle_query(*, name: Optional[str] = None, annotation: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None, path: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaPackageDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaArrayType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], contents: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.ArrayType.6 {{ contents = {angle_for(__env, contents)}, location = {angle_for(__env, location)} }}", ArrayType

  @staticmethod
  def angle_query(*, contents: Optional["JavaType"] = None, location: Optional[Tuple[()]] = None) -> "JavaArrayType":
    raise Exception("this function can only be called from @angle_query")

class JavaAnnotatedClass(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], annotation: ast.Expr, class_: ast.Expr) -> Tuple[str, Struct]:
    return f"java.AnnotatedClass.6 {{ annotation = {angle_for(__env, annotation)}, class_ = {angle_for(__env, class_)} }}", AnnotatedClass

  @staticmethod
  def angle_query(*, annotation: Optional["JavaAnnotation"] = None, class_: Optional["JavaClassDeclaration"] = None) -> "JavaAnnotatedClass":
    raise Exception("this function can only be called from @angle_query")

class JavaTypeParam(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, extends_: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.TypeParam.6 {{ name = {angle_for(__env, name)}, extends_ = {angle_for(__env, extends_)}, location = {angle_for(__env, location)} }}", TypeParam

  @staticmethod
  def angle_query(*, name: Optional[str] = None, extends_: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaTypeParam":
    raise Exception("this function can only be called from @angle_query")

class JavaAnnotation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.Annotation.6 {{ name = {angle_for(__env, name)}, location = {angle_for(__env, location)} }}", Annotation

  @staticmethod
  def angle_query(*, name: Optional["JavaQName"] = None, location: Optional[Tuple[()]] = None) -> "JavaAnnotation":
    raise Exception("this function can only be called from @angle_query")

class JavaXRefFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], xref: ast.Expr, file: ast.Expr) -> Tuple[str, Struct]:
    return f"java.XRefFile.6 {{ xref = {angle_for(__env, xref)}, file = {angle_for(__env, file)} }}", XRefFile

  @staticmethod
  def angle_query(*, xref: Optional["JavaXRef"] = None, file: Optional["SrcFile"] = None) -> "JavaXRefFile":
    raise Exception("this function can only be called from @angle_query")

class JavaMethodDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, parameters: ast.Expr, returnType: ast.Expr, annotations: ast.Expr, modifiers: ast.Expr, typeParams: ast.Expr, loc: ast.Expr, throws_: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.MethodDeclaration.6 {{ name = {angle_for(__env, name)}, parameters = {angle_for(__env, parameters)}, returnType = {angle_for(__env, returnType)}, annotations = {angle_for(__env, annotations)}, modifiers = {angle_for(__env, modifiers)}, typeParams = {angle_for(__env, typeParams)}, loc = {angle_for(__env, loc)}, throws_ = {angle_for(__env, throws_)}, location = {angle_for(__env, location)} }}", MethodDeclaration

  @staticmethod
  def angle_query(*, name: Optional["JavaQName"] = None, parameters: Optional[Tuple[()]] = None, returnType: Optional["JavaType"] = None, annotations: Optional[Tuple[()]] = None, modifiers: Optional[Tuple[()]] = None, typeParams: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None, throws_: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaMethodDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, fqn: ast.Expr, signature: ast.Expr, simple: ast.Expr, path: ast.Expr) -> Tuple[str, Struct]:
    return f"java.QName.6 {{ name = {angle_for(__env, name)}, fqn = {angle_for(__env, fqn)}, signature = {angle_for(__env, signature)}, simple = {angle_for(__env, simple)}, path = {angle_for(__env, path)} }}", QName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, fqn: Optional[Tuple[()]] = None, signature: Optional[Tuple[()]] = None, simple: Optional[Tuple[()]] = None, path: Optional[Tuple[()]] = None) -> "JavaQName":
    raise Exception("this function can only be called from @angle_query")

class JavaVariableDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr, annotations: ast.Expr, modifiers: ast.Expr, loc: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.VariableDeclaration.6 {{ name = {angle_for(__env, name)}, type = {angle_for(__env, type)}, annotations = {angle_for(__env, annotations)}, modifiers = {angle_for(__env, modifiers)}, loc = {angle_for(__env, loc)}, location = {angle_for(__env, location)} }}", VariableDeclaration

  @staticmethod
  def angle_query(*, name: Optional["JavaQName"] = None, type: Optional["JavaType"] = None, annotations: Optional[Tuple[()]] = None, modifiers: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaVariableDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaTypeVar(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], type: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.TypeVar.6 {{ type = {angle_for(__env, type)}, location = {angle_for(__env, location)} }}", TypeVar

  @staticmethod
  def angle_query(*, type: Optional[str] = None, location: Optional[Tuple[()]] = None) -> "JavaTypeVar":
    raise Exception("this function can only be called from @angle_query")

class JavaDeclaredType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], type: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.DeclaredType.6 {{ type = {angle_for(__env, type)}, location = {angle_for(__env, location)} }}", DeclaredType

  @staticmethod
  def angle_query(*, type: Optional["JavaQName"] = None, location: Optional[Tuple[()]] = None) -> "JavaDeclaredType":
    raise Exception("this function can only be called from @angle_query")

class JavaTypeArg(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"java.TypeArg.6 {angle_for(__env, arg)}", TypeArg

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "JavaTypeArg":
    raise Exception("this function can only be called from @angle_query")

class JavaFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xrefs: ast.Expr) -> Tuple[str, Struct]:
    return f"java.FileXRefs.6 {{ file = {angle_for(__env, file)}, xrefs = {angle_for(__env, xrefs)} }}", FileXRefs

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xrefs: Optional[Tuple[()]] = None) -> "JavaFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class JavaInheritance(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], base: ast.Expr, subclass: ast.Expr) -> Tuple[str, Struct]:
    return f"java.Inheritance.6 {{ base = {angle_for(__env, base)}, subclass = {angle_for(__env, subclass)} }}", Inheritance

  @staticmethod
  def angle_query(*, base: Optional["JavaType"] = None, subclass: Optional["JavaClassDeclaration"] = None) -> "JavaInheritance":
    raise Exception("this function can only be called from @angle_query")

class JavaJavaFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], package_: ast.Expr, import_: ast.Expr, definition: ast.Expr, file: ast.Expr) -> Tuple[str, Struct]:
    return f"java.JavaFile.6 {{ package_ = {angle_for(__env, package_)}, import_ = {angle_for(__env, import_)}, definition = {angle_for(__env, definition)}, file = {angle_for(__env, file)} }}", JavaFile

  @staticmethod
  def angle_query(*, package_: Optional[Tuple[()]] = None, import_: Optional[Tuple[()]] = None, definition: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None) -> "JavaJavaFile":
    raise Exception("this function can only be called from @angle_query")

class JavaPrimitiveType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], type: ast.Expr) -> Tuple[str, Struct]:
    return f"java.PrimitiveType.6 {{ type = {angle_for(__env, type)} }}", PrimitiveType

  @staticmethod
  def angle_query(*, type: Optional[str] = None) -> "JavaPrimitiveType":
    raise Exception("this function can only be called from @angle_query")

class JavaXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, ranges: ast.Expr, xtarget: ast.Expr) -> Tuple[str, Struct]:
    return f"java.XRef.6 {{ target = {angle_for(__env, target)}, ranges = {angle_for(__env, ranges)}, xtarget = {angle_for(__env, xtarget)} }}", XRef

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, ranges: Optional[Tuple[()]] = None, xtarget: Optional[Tuple[()]] = None) -> "JavaXRef":
    raise Exception("this function can only be called from @angle_query")

class JavaConstructorDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, parameters: ast.Expr, annotations: ast.Expr, modifiers: ast.Expr, typeParams: ast.Expr, loc: ast.Expr, throws_: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.ConstructorDeclaration.6 {{ name = {angle_for(__env, name)}, parameters = {angle_for(__env, parameters)}, annotations = {angle_for(__env, annotations)}, modifiers = {angle_for(__env, modifiers)}, typeParams = {angle_for(__env, typeParams)}, loc = {angle_for(__env, loc)}, throws_ = {angle_for(__env, throws_)}, location = {angle_for(__env, location)} }}", ConstructorDeclaration

  @staticmethod
  def angle_query(*, name: Optional["JavaQName"] = None, parameters: Optional[Tuple[()]] = None, annotations: Optional[Tuple[()]] = None, modifiers: Optional[Tuple[()]] = None, typeParams: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None, throws_: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaConstructorDeclaration":
    raise Exception("this function can only be called from @angle_query")

class JavaType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], type: ast.Expr, typeArgs: ast.Expr, stype: ast.Expr) -> Tuple[str, Struct]:
    return f"java.Type.6 {{ type = {angle_for(__env, type)}, typeArgs = {angle_for(__env, typeArgs)}, stype = {angle_for(__env, stype)} }}", Type

  @staticmethod
  def angle_query(*, type: Optional[str] = None, typeArgs: Optional[Tuple[()]] = None, stype: Optional[Tuple[()]] = None) -> "JavaType":
    raise Exception("this function can only be called from @angle_query")


