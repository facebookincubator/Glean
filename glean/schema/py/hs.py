# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.src import *


from glean.schema.hs.types import (
    PackageId,
    Definition,
    ClassNameLowerCase,
    DefinitionNameLowerCase,
    ModuleName,
    Definition,
    DefinitionLocation,
    DefinitionName,
    Module,
    TargetUses,
    FunctionNameLowerCase,
    FileDefinition,
    SourceModule,
    FunctionDefinition,
    ClassInstance,
    Class,
    FunctionName,
    ModuleDefinitions,
    ModuleNameLowerCase,
    XRef,
    ClassName,
    Type,
    FileXRefMap,
    DefinitionEntity,
    XRefTarget,
    XReference,
)


class HsPackageId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.PackageId.1 { angle_for(__env, arg, None) or '_' }", PackageId

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HsPackageId":
    raise Exception("this function can only be called from @angle_query")



class HsDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.Definition.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, source, 'source')])) or '_' } }}", Definition

  @staticmethod
  def angle_query(*, name: Optional["HsDefinitionName"] = None, source: Optional["SrcFileLocation"] = None) -> "HsDefinition":
    raise Exception("this function can only be called from @angle_query")



class HsClassNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], nameLowerCase: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.ClassNameLowerCase.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, nameLowerCase, 'nameLowerCase'), angle_for(__env, name, 'name')])) or '_' } }}", ClassNameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: Optional[str] = None, name: Optional["HsClassName"] = None) -> "HsClassNameLowerCase":
    raise Exception("this function can only be called from @angle_query")



class HsDefinitionNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], nameLowerCase: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.DefinitionNameLowerCase.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, nameLowerCase, 'nameLowerCase'), angle_for(__env, name, 'name')])) or '_' } }}", DefinitionNameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: Optional[str] = None, name: Optional["HsDefinitionName"] = None) -> "HsDefinitionNameLowerCase":
    raise Exception("this function can only be called from @angle_query")



class HsModuleName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.ModuleName.1 { angle_for(__env, arg, None) or '_' }", ModuleName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HsModuleName":
    raise Exception("this function can only be called from @angle_query")



class HsDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.Definition.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, source, 'source')])) or '_' } }}", Definition

  @staticmethod
  def angle_query(*, name: Optional["HsDefinitionName"] = None, source: Optional["SrcRange"] = None) -> "HsDefinition":
    raise Exception("this function can only be called from @angle_query")



class HsDefinitionLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], defn: ast.Expr, name: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.DefinitionLocation.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, defn, 'defn'), angle_for(__env, name, 'name'), angle_for(__env, source, 'source')])) or '_' } }}", DefinitionLocation

  @staticmethod
  def angle_query(*, defn: Optional["HsDefinitionEntity"] = None, name: Optional[str] = None, source: Optional["SrcFileLocation"] = None) -> "HsDefinitionLocation":
    raise Exception("this function can only be called from @angle_query")



class HsDefinitionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.DefinitionName.1 { angle_for(__env, arg, None) or '_' }", DefinitionName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HsDefinitionName":
    raise Exception("this function can only be called from @angle_query")



class HsModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], packageId: ast.Expr, moduleName: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.Module.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, packageId, 'packageId'), angle_for(__env, moduleName, 'moduleName'), angle_for(__env, source, 'source')])) or '_' } }}", Module

  @staticmethod
  def angle_query(*, packageId: Optional["HsPackageId"] = None, moduleName: Optional["HsModuleName"] = None, source: Optional["SrcFile"] = None) -> "HsModule":
    raise Exception("this function can only be called from @angle_query")



class HsTargetUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, uses: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.TargetUses.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, file, 'file'), angle_for(__env, uses, 'uses')])) or '_' } }}", TargetUses

  @staticmethod
  def angle_query(*, target: Optional["HsDefinition"] = None, file: Optional["SrcFile"] = None, uses: Optional[List["SrcByteSpan"]] = None) -> "HsTargetUses":
    raise Exception("this function can only be called from @angle_query")



class HsFunctionNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], nameLowerCase: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.FunctionNameLowerCase.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, nameLowerCase, 'nameLowerCase'), angle_for(__env, name, 'name')])) or '_' } }}", FunctionNameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: Optional[str] = None, name: Optional["HsFunctionName"] = None) -> "HsFunctionNameLowerCase":
    raise Exception("this function can only be called from @angle_query")



class HsFileDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, defn: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.FileDefinition.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, defn, 'defn')])) or '_' } }}", FileDefinition

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, defn: Optional["HsDefinitionEntity"] = None) -> "HsFileDefinition":
    raise Exception("this function can only be called from @angle_query")



class HsSourceModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], moduleName: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.SourceModule.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, moduleName, 'moduleName'), angle_for(__env, source, 'source')])) or '_' } }}", SourceModule

  @staticmethod
  def angle_query(*, moduleName: Optional["HsModuleName"] = None, source: Optional["SrcFile"] = None) -> "HsSourceModule":
    raise Exception("this function can only be called from @angle_query")



class HsFunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.FunctionDefinition.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, source, 'source')])) or '_' } }}", FunctionDefinition

  @staticmethod
  def angle_query(*, name: Optional["HsFunctionName"] = None, source: Optional["SrcRange"] = None) -> "HsFunctionDefinition":
    raise Exception("this function can only be called from @angle_query")



class HsClassInstance(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], typeclass: ast.Expr, instance: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.ClassInstance.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, typeclass, 'typeclass'), angle_for(__env, instance, 'instance'), angle_for(__env, source, 'source')])) or '_' } }}", ClassInstance

  @staticmethod
  def angle_query(*, typeclass: Optional["HsClassName"] = None, instance: Optional["HsType"] = None, source: Optional["SrcRange"] = None) -> "HsClassInstance":
    raise Exception("this function can only be called from @angle_query")



class HsClass(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.Class.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, source, 'source')])) or '_' } }}", Class

  @staticmethod
  def angle_query(*, name: Optional["HsClassName"] = None, source: Optional["SrcRange"] = None) -> "HsClass":
    raise Exception("this function can only be called from @angle_query")



class HsFunctionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.FunctionName.1 { angle_for(__env, arg, None) or '_' }", FunctionName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HsFunctionName":
    raise Exception("this function can only be called from @angle_query")



class HsModuleDefinitions(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], module: ast.Expr, functionDefinitions: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.ModuleDefinitions.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, module, 'module'), angle_for(__env, functionDefinitions, 'functionDefinitions')])) or '_' } }}", ModuleDefinitions

  @staticmethod
  def angle_query(*, module: Optional["HsModule"] = None, functionDefinitions: Optional[List["HsFunctionDefinition"]] = None) -> "HsModuleDefinitions":
    raise Exception("this function can only be called from @angle_query")



class HsModuleNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], nameLowerCase: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.ModuleNameLowerCase.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, nameLowerCase, 'nameLowerCase'), angle_for(__env, name, 'name')])) or '_' } }}", ModuleNameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: Optional[str] = None, name: Optional["HsModuleName"] = None) -> "HsModuleNameLowerCase":
    raise Exception("this function can only be called from @angle_query")



class HsXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], loc: ast.Expr, ref: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.XRef.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, loc, 'loc'), angle_for(__env, ref, 'ref')])) or '_' } }}", XRef

  @staticmethod
  def angle_query(*, loc: Optional["SrcFileLocation"] = None, ref: Optional["HsXRefTarget"] = None) -> "HsXRef":
    raise Exception("this function can only be called from @angle_query")



class HsClassName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.ClassName.1 { angle_for(__env, arg, None) or '_' }", ClassName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HsClassName":
    raise Exception("this function can only be called from @angle_query")



class HsType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.Type.1 { angle_for(__env, arg, None) or '_' }", Type

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HsType":
    raise Exception("this function can only be called from @angle_query")



class HsFileXRefMap(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, refs: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.FileXRefMap.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, refs, 'refs')])) or '_' } }}", FileXRefMap

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, refs: Optional[List["HsXReference"]] = None) -> "HsFileXRefMap":
    raise Exception("this function can only be called from @angle_query")





class HsDefinitionEntity(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], definition: ast.Expr, function_: ast.Expr, class_: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.DefinitionEntity.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, definition, 'definition'), angle_for(__env, function_, 'function_'), angle_for(__env, class_, 'class_')])) or '_' } }}", DefinitionEntity

  @staticmethod
  def angle_query_definition(*, definition: "HsDefinition") -> "HsDefinitionEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_function_(*, function_: "HsFunctionDefinition") -> "HsDefinitionEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_class_(*, class_: "HsClass") -> "HsDefinitionEntity":
    raise Exception("this function can only be called from @angle_query")




class HsXRefTarget(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], definition: ast.Expr, typeclass: ast.Expr, hs_module: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.XRefTarget.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, definition, 'definition'), angle_for(__env, typeclass, 'typeclass'), angle_for(__env, hs_module, 'hs_module')])) or '_' } }}", XRefTarget

  @staticmethod
  def angle_query_definition(*, definition: "HsDefinitionName") -> "HsXRefTarget":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_typeclass(*, typeclass: "HsClassName") -> "HsXRefTarget":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_hs_module(*, hs_module: "HsModuleName") -> "HsXRefTarget":
    raise Exception("this function can only be called from @angle_query")




class HsXReference(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, spans: ast.Expr) -> Tuple[str, Struct]:
    return f"hs.XReference.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, spans, 'spans')])) or '_' } }}", XReference

  @staticmethod
  def angle_query(*, target: Optional["HsXRefTarget"] = None, spans: Optional[List["SrcByteSpan"]] = None) -> "HsXReference":
    raise Exception("this function can only be called from @angle_query")




