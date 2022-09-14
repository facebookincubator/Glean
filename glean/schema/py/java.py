# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
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
    Definition,
    Wildcard,
    Modifier,
    BaseType,
    XRefTargetAmended,
    XRefTarget,
)


class JavaName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"java.Name.6 { query_fields if query_fields else '_' }", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "JavaName":
    raise Exception("this function can only be called from @angle_query")



class JavaClassDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, modifiers: ast.Expr, extends_: ast.Expr, implements_: ast.Expr, annotations: ast.Expr, variables: ast.Expr, constructors: ast.Expr, methods: ast.Expr, typeParams: ast.Expr, loc: ast.Expr, innerDefinitions: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, modifiers, 'modifiers'), angle_for(__env, extends_, 'extends_'), angle_for(__env, implements_, 'implements_'), angle_for(__env, annotations, 'annotations'), angle_for(__env, variables, 'variables'), angle_for(__env, constructors, 'constructors'), angle_for(__env, methods, 'methods'), angle_for(__env, typeParams, 'typeParams'), angle_for(__env, loc, 'loc'), angle_for(__env, innerDefinitions, 'innerDefinitions'), angle_for(__env, location, 'location')]))
    return f"java.ClassDeclaration.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ClassDeclaration

  @staticmethod
  def angle_query(*, name: Optional["JavaQName"] = None, modifiers: Optional[List["JavaModifier"]] = None, extends_: Optional[Union[Just["JavaType"], Just[None]]] = None, implements_: Optional[List["JavaType"]] = None, annotations: Optional[List["JavaAnnotation"]] = None, variables: Optional[List["JavaVariableDeclaration"]] = None, constructors: Optional[List["JavaConstructorDeclaration"]] = None, methods: Optional[List["JavaMethodDeclaration"]] = None, typeParams: Optional[List["JavaTypeParam"]] = None, loc: Optional["SrcLoc"] = None, innerDefinitions: Optional[Union[Just[List["JavaDefinition"]], Just[None]]] = None, location: Optional[Union[Just["SrcFileLocation"], Just[None]]] = None) -> "JavaClassDeclaration":
    raise Exception("this function can only be called from @angle_query")



class JavaPath(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], base: ast.Expr, container: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, base, 'base'), angle_for(__env, container, 'container')]))
    return f"java.Path.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Path

  @staticmethod
  def angle_query(*, base: Optional["JavaName"] = None, container: Optional[Union[Just["JavaPath"], Just[None]]] = None) -> "JavaPath":
    raise Exception("this function can only be called from @angle_query")



class JavaInterfaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, annotations: ast.Expr, modifiers: ast.Expr, extends_: ast.Expr, methods: ast.Expr, typeParams: ast.Expr, loc: ast.Expr, variables: ast.Expr, innerDefinitions: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, annotations, 'annotations'), angle_for(__env, modifiers, 'modifiers'), angle_for(__env, extends_, 'extends_'), angle_for(__env, methods, 'methods'), angle_for(__env, typeParams, 'typeParams'), angle_for(__env, loc, 'loc'), angle_for(__env, variables, 'variables'), angle_for(__env, innerDefinitions, 'innerDefinitions'), angle_for(__env, location, 'location')]))
    return f"java.InterfaceDeclaration.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", InterfaceDeclaration

  @staticmethod
  def angle_query(*, name: Optional["JavaQName"] = None, annotations: Optional[List["JavaAnnotation"]] = None, modifiers: Optional[List["JavaModifier"]] = None, extends_: Optional[List["JavaType"]] = None, methods: Optional[List["JavaMethodDeclaration"]] = None, typeParams: Optional[List["JavaTypeParam"]] = None, loc: Optional["SrcLoc"] = None, variables: Optional[Union[Just[List["JavaVariableDeclaration"]], Just[None]]] = None, innerDefinitions: Optional[Union[Just[List["JavaDefinition"]], Just[None]]] = None, location: Optional[Union[Just["SrcFileLocation"], Just[None]]] = None) -> "JavaInterfaceDeclaration":
    raise Exception("this function can only be called from @angle_query")



class JavaImportDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, import_on_demand: ast.Expr, static_member: ast.Expr, loc: ast.Expr, path: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, import_on_demand, 'import_on_demand'), angle_for(__env, static_member, 'static_member'), angle_for(__env, loc, 'loc'), angle_for(__env, path, 'path'), angle_for(__env, location, 'location')]))
    return f"java.ImportDeclaration.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ImportDeclaration

  @staticmethod
  def angle_query(*, name: Optional[str] = None, import_on_demand: Optional[bool] = None, static_member: Optional[Union[Just[str], Just[None]]] = None, loc: Optional["SrcLoc"] = None, path: Optional[Union[Just["JavaPath"], Just[None]]] = None, location: Optional[Union[Just["SrcFileLocation"], Just[None]]] = None) -> "JavaImportDeclaration":
    raise Exception("this function can only be called from @angle_query")



class JavaEnumDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, annotations: ast.Expr, modifiers: ast.Expr, implements_: ast.Expr, variables: ast.Expr, methods: ast.Expr, loc: ast.Expr, innerDefinitions: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, annotations, 'annotations'), angle_for(__env, modifiers, 'modifiers'), angle_for(__env, implements_, 'implements_'), angle_for(__env, variables, 'variables'), angle_for(__env, methods, 'methods'), angle_for(__env, loc, 'loc'), angle_for(__env, innerDefinitions, 'innerDefinitions'), angle_for(__env, location, 'location')]))
    return f"java.EnumDeclaration.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", EnumDeclaration

  @staticmethod
  def angle_query(*, name: Optional["JavaQName"] = None, annotations: Optional[List["JavaAnnotation"]] = None, modifiers: Optional[List["JavaModifier"]] = None, implements_: Optional[List["JavaType"]] = None, variables: Optional[List["JavaVariableDeclaration"]] = None, methods: Optional[List["JavaMethodDeclaration"]] = None, loc: Optional["SrcLoc"] = None, innerDefinitions: Optional[Union[Just[List["JavaDefinition"]], Just[None]]] = None, location: Optional[Union[Just["SrcFileLocation"], Just[None]]] = None) -> "JavaEnumDeclaration":
    raise Exception("this function can only be called from @angle_query")



class JavaPackageDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, annotation: ast.Expr, loc: ast.Expr, path: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, annotation, 'annotation'), angle_for(__env, loc, 'loc'), angle_for(__env, path, 'path'), angle_for(__env, location, 'location')]))
    return f"java.PackageDeclaration.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", PackageDeclaration

  @staticmethod
  def angle_query(*, name: Optional[str] = None, annotation: Optional[List["JavaAnnotation"]] = None, loc: Optional["SrcLoc"] = None, path: Optional[Union[Just["JavaPath"], Just[None]]] = None, location: Optional[Union[Just["SrcFileLocation"], Just[None]]] = None) -> "JavaPackageDeclaration":
    raise Exception("this function can only be called from @angle_query")



class JavaArrayType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], contents: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, contents, 'contents'), angle_for(__env, location, 'location')]))
    return f"java.ArrayType.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ArrayType

  @staticmethod
  def angle_query(*, contents: Optional["JavaType"] = None, location: Optional[Union[Just["SrcFileLocation"], Just[None]]] = None) -> "JavaArrayType":
    raise Exception("this function can only be called from @angle_query")



class JavaAnnotatedClass(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], annotation: ast.Expr, class_: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, annotation, 'annotation'), angle_for(__env, class_, 'class_')]))
    return f"java.AnnotatedClass.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", AnnotatedClass

  @staticmethod
  def angle_query(*, annotation: Optional["JavaAnnotation"] = None, class_: Optional["JavaClassDeclaration"] = None) -> "JavaAnnotatedClass":
    raise Exception("this function can only be called from @angle_query")



class JavaTypeParam(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, extends_: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, extends_, 'extends_'), angle_for(__env, location, 'location')]))
    return f"java.TypeParam.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TypeParam

  @staticmethod
  def angle_query(*, name: Optional[str] = None, extends_: Optional[List["JavaType"]] = None, location: Optional[Union[Just["SrcFileLocation"], Just[None]]] = None) -> "JavaTypeParam":
    raise Exception("this function can only be called from @angle_query")



class JavaAnnotation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, location, 'location')]))
    return f"java.Annotation.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Annotation

  @staticmethod
  def angle_query(*, name: Optional["JavaQName"] = None, location: Optional[Union[Just["SrcFileLocation"], Just[None]]] = None) -> "JavaAnnotation":
    raise Exception("this function can only be called from @angle_query")



class JavaXRefFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], xref: ast.Expr, file: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, xref, 'xref'), angle_for(__env, file, 'file')]))
    return f"java.XRefFile.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", XRefFile

  @staticmethod
  def angle_query(*, xref: Optional["JavaXRef"] = None, file: Optional["SrcFile"] = None) -> "JavaXRefFile":
    raise Exception("this function can only be called from @angle_query")



class JavaMethodDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, parameters: ast.Expr, returnType: ast.Expr, annotations: ast.Expr, modifiers: ast.Expr, typeParams: ast.Expr, loc: ast.Expr, throws_: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, parameters, 'parameters'), angle_for(__env, returnType, 'returnType'), angle_for(__env, annotations, 'annotations'), angle_for(__env, modifiers, 'modifiers'), angle_for(__env, typeParams, 'typeParams'), angle_for(__env, loc, 'loc'), angle_for(__env, throws_, 'throws_'), angle_for(__env, location, 'location')]))
    return f"java.MethodDeclaration.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", MethodDeclaration

  @staticmethod
  def angle_query(*, name: Optional["JavaQName"] = None, parameters: Optional[List["JavaVariableDeclaration"]] = None, returnType: Optional["JavaType"] = None, annotations: Optional[List["JavaAnnotation"]] = None, modifiers: Optional[List["JavaModifier"]] = None, typeParams: Optional[List["JavaTypeParam"]] = None, loc: Optional["SrcLoc"] = None, throws_: Optional[Union[Just[List["JavaType"]], Just[None]]] = None, location: Optional[Union[Just["SrcFileLocation"], Just[None]]] = None) -> "JavaMethodDeclaration":
    raise Exception("this function can only be called from @angle_query")



class JavaQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, fqn: ast.Expr, signature: ast.Expr, simple: ast.Expr, path: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, fqn, 'fqn'), angle_for(__env, signature, 'signature'), angle_for(__env, simple, 'simple'), angle_for(__env, path, 'path')]))
    return f"java.QName.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", QName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, fqn: Optional[Union[Just[str], Just[None]]] = None, signature: Optional[Union[Just[str], Just[None]]] = None, simple: Optional[Union[Just["JavaName"], Just[None]]] = None, path: Optional[Union[Just["JavaPath"], Just[None]]] = None) -> "JavaQName":
    raise Exception("this function can only be called from @angle_query")



class JavaVariableDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr, annotations: ast.Expr, modifiers: ast.Expr, loc: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, type, 'type'), angle_for(__env, annotations, 'annotations'), angle_for(__env, modifiers, 'modifiers'), angle_for(__env, loc, 'loc'), angle_for(__env, location, 'location')]))
    return f"java.VariableDeclaration.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", VariableDeclaration

  @staticmethod
  def angle_query(*, name: Optional["JavaQName"] = None, type: Optional["JavaType"] = None, annotations: Optional[List["JavaAnnotation"]] = None, modifiers: Optional[List["JavaModifier"]] = None, loc: Optional["SrcLoc"] = None, location: Optional[Union[Just["SrcFileLocation"], Just[None]]] = None) -> "JavaVariableDeclaration":
    raise Exception("this function can only be called from @angle_query")



class JavaTypeVar(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], type: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, type, 'type'), angle_for(__env, location, 'location')]))
    return f"java.TypeVar.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TypeVar

  @staticmethod
  def angle_query(*, type: Optional[str] = None, location: Optional[Union[Just["SrcFileLocation"], Just[None]]] = None) -> "JavaTypeVar":
    raise Exception("this function can only be called from @angle_query")



class JavaDeclaredType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], type: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, type, 'type'), angle_for(__env, location, 'location')]))
    return f"java.DeclaredType.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DeclaredType

  @staticmethod
  def angle_query(*, type: Optional["JavaQName"] = None, location: Optional[Union[Just["SrcFileLocation"], Just[None]]] = None) -> "JavaDeclaredType":
    raise Exception("this function can only be called from @angle_query")



class JavaTypeArg(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], type: ast.Expr, wildcard: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, type, 'type'), angle_for(__env, wildcard, 'wildcard')]))
    return f"java.TypeArg.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TypeArg

  @staticmethod
  def angle_query_type(*, type: Optional["JavaType"] = None) -> "JavaTypeArg":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_wildcard(*, wildcard: Optional["JavaWildcard"] = None) -> "JavaTypeArg":
    raise Exception("this function can only be called from @angle_query")




class JavaFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xrefs: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xrefs, 'xrefs')]))
    return f"java.FileXRefs.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FileXRefs

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xrefs: Optional[List["JavaXRef"]] = None) -> "JavaFileXRefs":
    raise Exception("this function can only be called from @angle_query")



class JavaInheritance(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], base: ast.Expr, subclass: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, base, 'base'), angle_for(__env, subclass, 'subclass')]))
    return f"java.Inheritance.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Inheritance

  @staticmethod
  def angle_query(*, base: Optional["JavaType"] = None, subclass: Optional["JavaClassDeclaration"] = None) -> "JavaInheritance":
    raise Exception("this function can only be called from @angle_query")



class JavaJavaFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], package_: ast.Expr, import_: ast.Expr, definition: ast.Expr, file: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, package_, 'package_'), angle_for(__env, import_, 'import_'), angle_for(__env, definition, 'definition'), angle_for(__env, file, 'file')]))
    return f"java.JavaFile.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", JavaFile

  @staticmethod
  def angle_query(*, package_: Optional[Union[Just["JavaPackageDeclaration"], Just[None]]] = None, import_: Optional[List["JavaImportDeclaration"]] = None, definition: Optional[List["JavaDefinition"]] = None, file: Optional["SrcFile"] = None) -> "JavaJavaFile":
    raise Exception("this function can only be called from @angle_query")



class JavaPrimitiveType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], type: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, type, 'type')]))
    return f"java.PrimitiveType.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", PrimitiveType

  @staticmethod
  def angle_query(*, type: Optional[str] = None) -> "JavaPrimitiveType":
    raise Exception("this function can only be called from @angle_query")



class JavaXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, ranges: ast.Expr, xtarget: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, ranges, 'ranges'), angle_for(__env, xtarget, 'xtarget')]))
    return f"java.XRef.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", XRef

  @staticmethod
  def angle_query(*, target: Optional[Union[Just["JavaXRefTarget"], Just[None]]] = None, ranges: Optional[List["SrcByteSpan"]] = None, xtarget: Optional[Union[Just["JavaXRefTargetAmended"], Just[None]]] = None) -> "JavaXRef":
    raise Exception("this function can only be called from @angle_query")



class JavaConstructorDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, parameters: ast.Expr, annotations: ast.Expr, modifiers: ast.Expr, typeParams: ast.Expr, loc: ast.Expr, throws_: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, parameters, 'parameters'), angle_for(__env, annotations, 'annotations'), angle_for(__env, modifiers, 'modifiers'), angle_for(__env, typeParams, 'typeParams'), angle_for(__env, loc, 'loc'), angle_for(__env, throws_, 'throws_'), angle_for(__env, location, 'location')]))
    return f"java.ConstructorDeclaration.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ConstructorDeclaration

  @staticmethod
  def angle_query(*, name: Optional["JavaQName"] = None, parameters: Optional[List["JavaVariableDeclaration"]] = None, annotations: Optional[List["JavaAnnotation"]] = None, modifiers: Optional[List["JavaModifier"]] = None, typeParams: Optional[List["JavaTypeParam"]] = None, loc: Optional["SrcLoc"] = None, throws_: Optional[Union[Just[List["JavaType"]], Just[None]]] = None, location: Optional[Union[Just["SrcFileLocation"], Just[None]]] = None) -> "JavaConstructorDeclaration":
    raise Exception("this function can only be called from @angle_query")



class JavaType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], type: ast.Expr, typeArgs: ast.Expr, stype: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, type, 'type'), angle_for(__env, typeArgs, 'typeArgs'), angle_for(__env, stype, 'stype')]))
    return f"java.Type.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Type

  @staticmethod
  def angle_query(*, type: Optional[str] = None, typeArgs: Optional[List["JavaTypeArg"]] = None, stype: Optional[Union[Just["JavaBaseType"], Just[None]]] = None) -> "JavaType":
    raise Exception("this function can only be called from @angle_query")





class JavaDefinition(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], class_: ast.Expr, interface_: ast.Expr, enum_: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, class_, 'class_'), angle_for(__env, interface_, 'interface_'), angle_for(__env, enum_, 'enum_')]))
    return f"java.Definition.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Definition

  @staticmethod
  def angle_query_class_(*, class_: Optional["JavaClassDeclaration"] = None) -> "JavaDefinition":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_interface_(*, interface_: Optional["JavaInterfaceDeclaration"] = None) -> "JavaDefinition":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_enum_(*, enum_: Optional["JavaEnumDeclaration"] = None) -> "JavaDefinition":
    raise Exception("this function can only be called from @angle_query")




class JavaWildcard(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], extends_: ast.Expr, super_: ast.Expr, unbounded: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, extends_, 'extends_'), angle_for(__env, super_, 'super_'), angle_for(__env, unbounded, 'unbounded')]))
    return f"java.Wildcard.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Wildcard

  @staticmethod
  def angle_query_extends_(*, extends_: Optional["JavaType"] = None) -> "JavaWildcard":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_super_(*, super_: Optional["JavaType"] = None) -> "JavaWildcard":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_unbounded(*, unbounded: Optional[bool] = None) -> "JavaWildcard":
    raise Exception("this function can only be called from @angle_query")




class JavaModifier(Enum):
  abstract_ = 0
  default_ = 1
  final_ = 2
  native_ = 3
  private_ = 4
  protected_ = 5
  public_ = 6
  static_ = 7
  strictfp_ = 8
  synchronized_ = 9
  transient_ = 10
  volatile_ = 11

class JavaBaseType(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declared: ast.Expr, primitive: ast.Expr, variable: ast.Expr, array: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declared, 'declared'), angle_for(__env, primitive, 'primitive'), angle_for(__env, variable, 'variable'), angle_for(__env, array, 'array')]))
    return f"java.BaseType.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", BaseType

  @staticmethod
  def angle_query_declared(*, declared: Optional["JavaDeclaredType"] = None) -> "JavaBaseType":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_primitive(*, primitive: Optional["JavaPrimitiveType"] = None) -> "JavaBaseType":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_variable(*, variable: Optional["JavaTypeVar"] = None) -> "JavaBaseType":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_array(*, array: Optional["JavaArrayType"] = None) -> "JavaBaseType":
    raise Exception("this function can only be called from @angle_query")




class JavaXRefTargetAmended(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], class_or_interface_: ast.Expr, definition_: ast.Expr, ctor_: ast.Expr, method_: ast.Expr, field_: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, class_or_interface_, 'class_or_interface_'), angle_for(__env, definition_, 'definition_'), angle_for(__env, ctor_, 'ctor_'), angle_for(__env, method_, 'method_'), angle_for(__env, field_, 'field_')]))
    return f"java.XRefTargetAmended.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", XRefTargetAmended

  @staticmethod
  def angle_query_class_or_interface_(*, class_or_interface_: Optional["JavaQName"] = None) -> "JavaXRefTargetAmended":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_definition_(*, definition_: Optional["JavaQName"] = None) -> "JavaXRefTargetAmended":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_ctor_(*, ctor_: Optional["JavaQName"] = None) -> "JavaXRefTargetAmended":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_method_(*, method_: Optional["JavaQName"] = None) -> "JavaXRefTargetAmended":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_field_(*, field_: Optional["JavaQName"] = None) -> "JavaXRefTargetAmended":
    raise Exception("this function can only be called from @angle_query")




class JavaXRefTarget(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], class_or_interface_: ast.Expr, ctor_: ast.Expr, method_: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, class_or_interface_, 'class_or_interface_'), angle_for(__env, ctor_, 'ctor_'), angle_for(__env, method_, 'method_')]))
    return f"java.XRefTarget.6 { ('{ ' + query_fields + ' }') if query_fields else '_' }", XRefTarget

  @staticmethod
  def angle_query_class_or_interface_(*, class_or_interface_: Optional["JavaQName"] = None) -> "JavaXRefTarget":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_ctor_(*, ctor_: Optional["JavaQName"] = None) -> "JavaXRefTarget":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_method_(*, method_: Optional["JavaQName"] = None) -> "JavaXRefTarget":
    raise Exception("this function can only be called from @angle_query")





