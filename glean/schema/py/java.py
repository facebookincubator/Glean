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
    return f"java.Name.6 { angle_for(__env, arg, None) or '_' }", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "JavaName":
    raise Exception("this function can only be called from @angle_query")



class JavaClassDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, modifiers: ast.Expr, extends_: ast.Expr, implements_: ast.Expr, annotations: ast.Expr, variables: ast.Expr, constructors: ast.Expr, methods: ast.Expr, typeParams: ast.Expr, loc: ast.Expr, innerDefinitions: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.ClassDeclaration.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, modifiers, 'modifiers'), angle_for(__env, extends_, 'extends_'), angle_for(__env, implements_, 'implements_'), angle_for(__env, annotations, 'annotations'), angle_for(__env, variables, 'variables'), angle_for(__env, constructors, 'constructors'), angle_for(__env, methods, 'methods'), angle_for(__env, typeParams, 'typeParams'), angle_for(__env, loc, 'loc'), angle_for(__env, innerDefinitions, 'innerDefinitions'), angle_for(__env, location, 'location')])) or '_' } }}", ClassDeclaration

  @staticmethod
  def angle_query(*, name: Optional["JavaQName"] = None, modifiers: Optional[List[Tuple[()]]] = None, extends_: Optional[Tuple[()]] = None, implements_: Optional[List["JavaType"]] = None, annotations: Optional[List["JavaAnnotation"]] = None, variables: Optional[List["JavaVariableDeclaration"]] = None, constructors: Optional[List["JavaConstructorDeclaration"]] = None, methods: Optional[List["JavaMethodDeclaration"]] = None, typeParams: Optional[List["JavaTypeParam"]] = None, loc: Optional[Tuple[()]] = None, innerDefinitions: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaClassDeclaration":
    raise Exception("this function can only be called from @angle_query")



class JavaPath(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], base: ast.Expr, container: ast.Expr) -> Tuple[str, Struct]:
    return f"java.Path.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, base, 'base'), angle_for(__env, container, 'container')])) or '_' } }}", Path

  @staticmethod
  def angle_query(*, base: Optional["JavaName"] = None, container: Optional[Tuple[()]] = None) -> "JavaPath":
    raise Exception("this function can only be called from @angle_query")



class JavaInterfaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, annotations: ast.Expr, modifiers: ast.Expr, extends_: ast.Expr, methods: ast.Expr, typeParams: ast.Expr, loc: ast.Expr, variables: ast.Expr, innerDefinitions: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.InterfaceDeclaration.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, annotations, 'annotations'), angle_for(__env, modifiers, 'modifiers'), angle_for(__env, extends_, 'extends_'), angle_for(__env, methods, 'methods'), angle_for(__env, typeParams, 'typeParams'), angle_for(__env, loc, 'loc'), angle_for(__env, variables, 'variables'), angle_for(__env, innerDefinitions, 'innerDefinitions'), angle_for(__env, location, 'location')])) or '_' } }}", InterfaceDeclaration

  @staticmethod
  def angle_query(*, name: Optional["JavaQName"] = None, annotations: Optional[List["JavaAnnotation"]] = None, modifiers: Optional[List[Tuple[()]]] = None, extends_: Optional[List["JavaType"]] = None, methods: Optional[List["JavaMethodDeclaration"]] = None, typeParams: Optional[List["JavaTypeParam"]] = None, loc: Optional[Tuple[()]] = None, variables: Optional[Tuple[()]] = None, innerDefinitions: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaInterfaceDeclaration":
    raise Exception("this function can only be called from @angle_query")



class JavaImportDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, import_on_demand: ast.Expr, static_member: ast.Expr, loc: ast.Expr, path: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.ImportDeclaration.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, import_on_demand, 'import_on_demand'), angle_for(__env, static_member, 'static_member'), angle_for(__env, loc, 'loc'), angle_for(__env, path, 'path'), angle_for(__env, location, 'location')])) or '_' } }}", ImportDeclaration

  @staticmethod
  def angle_query(*, name: Optional[str] = None, import_on_demand: Optional[bool] = None, static_member: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None, path: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaImportDeclaration":
    raise Exception("this function can only be called from @angle_query")



class JavaEnumDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, annotations: ast.Expr, modifiers: ast.Expr, implements_: ast.Expr, variables: ast.Expr, methods: ast.Expr, loc: ast.Expr, innerDefinitions: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.EnumDeclaration.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, annotations, 'annotations'), angle_for(__env, modifiers, 'modifiers'), angle_for(__env, implements_, 'implements_'), angle_for(__env, variables, 'variables'), angle_for(__env, methods, 'methods'), angle_for(__env, loc, 'loc'), angle_for(__env, innerDefinitions, 'innerDefinitions'), angle_for(__env, location, 'location')])) or '_' } }}", EnumDeclaration

  @staticmethod
  def angle_query(*, name: Optional["JavaQName"] = None, annotations: Optional[List["JavaAnnotation"]] = None, modifiers: Optional[List[Tuple[()]]] = None, implements_: Optional[List["JavaType"]] = None, variables: Optional[List["JavaVariableDeclaration"]] = None, methods: Optional[List["JavaMethodDeclaration"]] = None, loc: Optional[Tuple[()]] = None, innerDefinitions: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaEnumDeclaration":
    raise Exception("this function can only be called from @angle_query")



class JavaPackageDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, annotation: ast.Expr, loc: ast.Expr, path: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.PackageDeclaration.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, annotation, 'annotation'), angle_for(__env, loc, 'loc'), angle_for(__env, path, 'path'), angle_for(__env, location, 'location')])) or '_' } }}", PackageDeclaration

  @staticmethod
  def angle_query(*, name: Optional[str] = None, annotation: Optional[List["JavaAnnotation"]] = None, loc: Optional[Tuple[()]] = None, path: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaPackageDeclaration":
    raise Exception("this function can only be called from @angle_query")



class JavaArrayType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], contents: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.ArrayType.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, contents, 'contents'), angle_for(__env, location, 'location')])) or '_' } }}", ArrayType

  @staticmethod
  def angle_query(*, contents: Optional["JavaType"] = None, location: Optional[Tuple[()]] = None) -> "JavaArrayType":
    raise Exception("this function can only be called from @angle_query")



class JavaAnnotatedClass(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], annotation: ast.Expr, class_: ast.Expr) -> Tuple[str, Struct]:
    return f"java.AnnotatedClass.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, annotation, 'annotation'), angle_for(__env, class_, 'class_')])) or '_' } }}", AnnotatedClass

  @staticmethod
  def angle_query(*, annotation: Optional["JavaAnnotation"] = None, class_: Optional["JavaClassDeclaration"] = None) -> "JavaAnnotatedClass":
    raise Exception("this function can only be called from @angle_query")



class JavaTypeParam(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, extends_: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.TypeParam.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, extends_, 'extends_'), angle_for(__env, location, 'location')])) or '_' } }}", TypeParam

  @staticmethod
  def angle_query(*, name: Optional[str] = None, extends_: Optional[List["JavaType"]] = None, location: Optional[Tuple[()]] = None) -> "JavaTypeParam":
    raise Exception("this function can only be called from @angle_query")



class JavaAnnotation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.Annotation.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, location, 'location')])) or '_' } }}", Annotation

  @staticmethod
  def angle_query(*, name: Optional["JavaQName"] = None, location: Optional[Tuple[()]] = None) -> "JavaAnnotation":
    raise Exception("this function can only be called from @angle_query")



class JavaXRefFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], xref: ast.Expr, file: ast.Expr) -> Tuple[str, Struct]:
    return f"java.XRefFile.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, xref, 'xref'), angle_for(__env, file, 'file')])) or '_' } }}", XRefFile

  @staticmethod
  def angle_query(*, xref: Optional["JavaXRef"] = None, file: Optional["SrcFile"] = None) -> "JavaXRefFile":
    raise Exception("this function can only be called from @angle_query")



class JavaMethodDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, parameters: ast.Expr, returnType: ast.Expr, annotations: ast.Expr, modifiers: ast.Expr, typeParams: ast.Expr, loc: ast.Expr, throws_: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.MethodDeclaration.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, parameters, 'parameters'), angle_for(__env, returnType, 'returnType'), angle_for(__env, annotations, 'annotations'), angle_for(__env, modifiers, 'modifiers'), angle_for(__env, typeParams, 'typeParams'), angle_for(__env, loc, 'loc'), angle_for(__env, throws_, 'throws_'), angle_for(__env, location, 'location')])) or '_' } }}", MethodDeclaration

  @staticmethod
  def angle_query(*, name: Optional["JavaQName"] = None, parameters: Optional[List["JavaVariableDeclaration"]] = None, returnType: Optional["JavaType"] = None, annotations: Optional[List["JavaAnnotation"]] = None, modifiers: Optional[List[Tuple[()]]] = None, typeParams: Optional[List["JavaTypeParam"]] = None, loc: Optional[Tuple[()]] = None, throws_: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaMethodDeclaration":
    raise Exception("this function can only be called from @angle_query")



class JavaQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, fqn: ast.Expr, signature: ast.Expr, simple: ast.Expr, path: ast.Expr) -> Tuple[str, Struct]:
    return f"java.QName.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, fqn, 'fqn'), angle_for(__env, signature, 'signature'), angle_for(__env, simple, 'simple'), angle_for(__env, path, 'path')])) or '_' } }}", QName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, fqn: Optional[Tuple[()]] = None, signature: Optional[Tuple[()]] = None, simple: Optional[Tuple[()]] = None, path: Optional[Tuple[()]] = None) -> "JavaQName":
    raise Exception("this function can only be called from @angle_query")



class JavaVariableDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr, annotations: ast.Expr, modifiers: ast.Expr, loc: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.VariableDeclaration.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, type, 'type'), angle_for(__env, annotations, 'annotations'), angle_for(__env, modifiers, 'modifiers'), angle_for(__env, loc, 'loc'), angle_for(__env, location, 'location')])) or '_' } }}", VariableDeclaration

  @staticmethod
  def angle_query(*, name: Optional["JavaQName"] = None, type: Optional["JavaType"] = None, annotations: Optional[List["JavaAnnotation"]] = None, modifiers: Optional[List[Tuple[()]]] = None, loc: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaVariableDeclaration":
    raise Exception("this function can only be called from @angle_query")



class JavaTypeVar(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], type: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.TypeVar.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, type, 'type'), angle_for(__env, location, 'location')])) or '_' } }}", TypeVar

  @staticmethod
  def angle_query(*, type: Optional[str] = None, location: Optional[Tuple[()]] = None) -> "JavaTypeVar":
    raise Exception("this function can only be called from @angle_query")



class JavaDeclaredType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], type: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.DeclaredType.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, type, 'type'), angle_for(__env, location, 'location')])) or '_' } }}", DeclaredType

  @staticmethod
  def angle_query(*, type: Optional["JavaQName"] = None, location: Optional[Tuple[()]] = None) -> "JavaDeclaredType":
    raise Exception("this function can only be called from @angle_query")



class JavaTypeArg(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], type: ast.Expr, wildcard: ast.Expr) -> Tuple[str, Struct]:
    return f"java.TypeArg.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, type, 'type'), angle_for(__env, wildcard, 'wildcard')])) or '_' } }}", TypeArg

  @staticmethod
  def angle_query_type(*, type: "JavaType") -> "JavaTypeArg":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_wildcard(*, wildcard: Tuple[()]) -> "JavaTypeArg":
    raise Exception("this function can only be called from @angle_query")




class JavaFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xrefs: ast.Expr) -> Tuple[str, Struct]:
    return f"java.FileXRefs.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xrefs, 'xrefs')])) or '_' } }}", FileXRefs

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xrefs: Optional[List["JavaXRef"]] = None) -> "JavaFileXRefs":
    raise Exception("this function can only be called from @angle_query")



class JavaInheritance(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], base: ast.Expr, subclass: ast.Expr) -> Tuple[str, Struct]:
    return f"java.Inheritance.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, base, 'base'), angle_for(__env, subclass, 'subclass')])) or '_' } }}", Inheritance

  @staticmethod
  def angle_query(*, base: Optional["JavaType"] = None, subclass: Optional["JavaClassDeclaration"] = None) -> "JavaInheritance":
    raise Exception("this function can only be called from @angle_query")



class JavaJavaFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], package_: ast.Expr, import_: ast.Expr, definition: ast.Expr, file: ast.Expr) -> Tuple[str, Struct]:
    return f"java.JavaFile.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, package_, 'package_'), angle_for(__env, import_, 'import_'), angle_for(__env, definition, 'definition'), angle_for(__env, file, 'file')])) or '_' } }}", JavaFile

  @staticmethod
  def angle_query(*, package_: Optional[Tuple[()]] = None, import_: Optional[List["JavaImportDeclaration"]] = None, definition: Optional[List[Tuple[()]]] = None, file: Optional["SrcFile"] = None) -> "JavaJavaFile":
    raise Exception("this function can only be called from @angle_query")



class JavaPrimitiveType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], type: ast.Expr) -> Tuple[str, Struct]:
    return f"java.PrimitiveType.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, type, 'type')])) or '_' } }}", PrimitiveType

  @staticmethod
  def angle_query(*, type: Optional[str] = None) -> "JavaPrimitiveType":
    raise Exception("this function can only be called from @angle_query")



class JavaXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, ranges: ast.Expr, xtarget: ast.Expr) -> Tuple[str, Struct]:
    return f"java.XRef.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, ranges, 'ranges'), angle_for(__env, xtarget, 'xtarget')])) or '_' } }}", XRef

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, ranges: Optional[List[Tuple[()]]] = None, xtarget: Optional[Tuple[()]] = None) -> "JavaXRef":
    raise Exception("this function can only be called from @angle_query")



class JavaConstructorDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, parameters: ast.Expr, annotations: ast.Expr, modifiers: ast.Expr, typeParams: ast.Expr, loc: ast.Expr, throws_: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"java.ConstructorDeclaration.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, parameters, 'parameters'), angle_for(__env, annotations, 'annotations'), angle_for(__env, modifiers, 'modifiers'), angle_for(__env, typeParams, 'typeParams'), angle_for(__env, loc, 'loc'), angle_for(__env, throws_, 'throws_'), angle_for(__env, location, 'location')])) or '_' } }}", ConstructorDeclaration

  @staticmethod
  def angle_query(*, name: Optional["JavaQName"] = None, parameters: Optional[List["JavaVariableDeclaration"]] = None, annotations: Optional[List["JavaAnnotation"]] = None, modifiers: Optional[List[Tuple[()]]] = None, typeParams: Optional[List["JavaTypeParam"]] = None, loc: Optional[Tuple[()]] = None, throws_: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "JavaConstructorDeclaration":
    raise Exception("this function can only be called from @angle_query")



class JavaType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], type: ast.Expr, typeArgs: ast.Expr, stype: ast.Expr) -> Tuple[str, Struct]:
    return f"java.Type.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, type, 'type'), angle_for(__env, typeArgs, 'typeArgs'), angle_for(__env, stype, 'stype')])) or '_' } }}", Type

  @staticmethod
  def angle_query(*, type: Optional[str] = None, typeArgs: Optional[List["JavaTypeArg"]] = None, stype: Optional[Tuple[()]] = None) -> "JavaType":
    raise Exception("this function can only be called from @angle_query")




