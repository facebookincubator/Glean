# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


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
)


class HsPackageId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hs.PackageId.1 {json.dumps(key)}", PackageId

  @staticmethod
  def angle_query(*, arg: str) -> "HsPackageId":
    raise Exception("this function can only be called from @angle_query")

class HsDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hs.Definition.2 {{ name = _, source = _ }}", Definition

  @staticmethod
  def angle_query(*, name: Tuple[()], source: Tuple[()]) -> "HsDefinition":
    raise Exception("this function can only be called from @angle_query")

class HsClassNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hs.ClassNameLowerCase.1 {{ nameLowerCase = _, name = _ }}", ClassNameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: str, name: Tuple[()]) -> "HsClassNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class HsDefinitionNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hs.DefinitionNameLowerCase.1 {{ nameLowerCase = _, name = _ }}", DefinitionNameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: str, name: Tuple[()]) -> "HsDefinitionNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class HsModuleName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hs.ModuleName.1 {json.dumps(key)}", ModuleName

  @staticmethod
  def angle_query(*, arg: str) -> "HsModuleName":
    raise Exception("this function can only be called from @angle_query")

class HsDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hs.Definition.1 {{ name = _, source = _ }}", Definition

  @staticmethod
  def angle_query(*, name: Tuple[()], source: Tuple[()]) -> "HsDefinition":
    raise Exception("this function can only be called from @angle_query")

class HsDefinitionLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hs.DefinitionLocation.2 {{ defn = _, name = _, source = _ }}", DefinitionLocation

  @staticmethod
  def angle_query(*, defn: Tuple[()], name: str, source: Tuple[()]) -> "HsDefinitionLocation":
    raise Exception("this function can only be called from @angle_query")

class HsDefinitionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hs.DefinitionName.1 {json.dumps(key)}", DefinitionName

  @staticmethod
  def angle_query(*, arg: str) -> "HsDefinitionName":
    raise Exception("this function can only be called from @angle_query")

class HsModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hs.Module.1 {{ packageId = _, moduleName = _, source = _ }}", Module

  @staticmethod
  def angle_query(*, packageId: Tuple[()], moduleName: Tuple[()], source: Tuple[()]) -> "HsModule":
    raise Exception("this function can only be called from @angle_query")

class HsTargetUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hs.TargetUses.2 {{ target = _, file = _, uses = _ }}", TargetUses

  @staticmethod
  def angle_query(*, target: Tuple[()], file: Tuple[()], uses: Tuple[()]) -> "HsTargetUses":
    raise Exception("this function can only be called from @angle_query")

class HsFunctionNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hs.FunctionNameLowerCase.1 {{ nameLowerCase = _, name = _ }}", FunctionNameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: str, name: Tuple[()]) -> "HsFunctionNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class HsFileDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hs.FileDefinition.2 {{ file = _, defn = _ }}", FileDefinition

  @staticmethod
  def angle_query(*, file: Tuple[()], defn: Tuple[()]) -> "HsFileDefinition":
    raise Exception("this function can only be called from @angle_query")

class HsSourceModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hs.SourceModule.1 {{ moduleName = _, source = _ }}", SourceModule

  @staticmethod
  def angle_query(*, moduleName: Tuple[()], source: Tuple[()]) -> "HsSourceModule":
    raise Exception("this function can only be called from @angle_query")

class HsFunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hs.FunctionDefinition.1 {{ name = _, source = _ }}", FunctionDefinition

  @staticmethod
  def angle_query(*, name: Tuple[()], source: Tuple[()]) -> "HsFunctionDefinition":
    raise Exception("this function can only be called from @angle_query")

class HsClassInstance(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hs.ClassInstance.1 {{ typeclass = _, instance = _, source = _ }}", ClassInstance

  @staticmethod
  def angle_query(*, typeclass: Tuple[()], instance: Tuple[()], source: Tuple[()]) -> "HsClassInstance":
    raise Exception("this function can only be called from @angle_query")

class HsClass(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hs.Class.1 {{ name = _, source = _ }}", Class

  @staticmethod
  def angle_query(*, name: Tuple[()], source: Tuple[()]) -> "HsClass":
    raise Exception("this function can only be called from @angle_query")

class HsFunctionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hs.FunctionName.1 {json.dumps(key)}", FunctionName

  @staticmethod
  def angle_query(*, arg: str) -> "HsFunctionName":
    raise Exception("this function can only be called from @angle_query")

class HsModuleDefinitions(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hs.ModuleDefinitions.1 {{ module = _, functionDefinitions = _ }}", ModuleDefinitions

  @staticmethod
  def angle_query(*, module: Tuple[()], functionDefinitions: Tuple[()]) -> "HsModuleDefinitions":
    raise Exception("this function can only be called from @angle_query")

class HsModuleNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hs.ModuleNameLowerCase.1 {{ nameLowerCase = _, name = _ }}", ModuleNameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: str, name: Tuple[()]) -> "HsModuleNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class HsXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hs.XRef.2 {{ loc = _, ref = _ }}", XRef

  @staticmethod
  def angle_query(*, loc: Tuple[()], ref: Tuple[()]) -> "HsXRef":
    raise Exception("this function can only be called from @angle_query")

class HsClassName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hs.ClassName.1 {json.dumps(key)}", ClassName

  @staticmethod
  def angle_query(*, arg: str) -> "HsClassName":
    raise Exception("this function can only be called from @angle_query")

class HsType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hs.Type.1 {json.dumps(key)}", Type

  @staticmethod
  def angle_query(*, arg: str) -> "HsType":
    raise Exception("this function can only be called from @angle_query")

class HsFileXRefMap(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hs.FileXRefMap.2 {{ file = _, refs = _ }}", FileXRefMap

  @staticmethod
  def angle_query(*, file: Tuple[()], refs: Tuple[()]) -> "HsFileXRefMap":
    raise Exception("this function can only be called from @angle_query")


