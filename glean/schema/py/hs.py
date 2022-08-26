# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


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
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hs.PackageId.1 {{ }}", PackageId
    return f"hs.PackageId.1 {key}", PackageId

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HsPackageId":
    raise Exception("this function can only be called from @angle_query")

class HsDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hs.Definition.2 {{ }}", Definition
    return f"hs.Definition.2 { concatenateFields(key) }", Definition

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "HsDefinition":
    raise Exception("this function can only be called from @angle_query")

class HsClassNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hs.ClassNameLowerCase.1 {{ }}", ClassNameLowerCase
    return f"hs.ClassNameLowerCase.1 { concatenateFields(key) }", ClassNameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: Optional[str] = None, name: Optional[Tuple[()]] = None) -> "HsClassNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class HsDefinitionNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hs.DefinitionNameLowerCase.1 {{ }}", DefinitionNameLowerCase
    return f"hs.DefinitionNameLowerCase.1 { concatenateFields(key) }", DefinitionNameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: Optional[str] = None, name: Optional[Tuple[()]] = None) -> "HsDefinitionNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class HsModuleName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hs.ModuleName.1 {{ }}", ModuleName
    return f"hs.ModuleName.1 {key}", ModuleName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HsModuleName":
    raise Exception("this function can only be called from @angle_query")

class HsDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hs.Definition.1 {{ }}", Definition
    return f"hs.Definition.1 { concatenateFields(key) }", Definition

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "HsDefinition":
    raise Exception("this function can only be called from @angle_query")

class HsDefinitionLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hs.DefinitionLocation.2 {{ }}", DefinitionLocation
    return f"hs.DefinitionLocation.2 { concatenateFields(key) }", DefinitionLocation

  @staticmethod
  def angle_query(*, defn: Optional[Tuple[()]] = None, name: Optional[str] = None, source: Optional[Tuple[()]] = None) -> "HsDefinitionLocation":
    raise Exception("this function can only be called from @angle_query")

class HsDefinitionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hs.DefinitionName.1 {{ }}", DefinitionName
    return f"hs.DefinitionName.1 {key}", DefinitionName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HsDefinitionName":
    raise Exception("this function can only be called from @angle_query")

class HsModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hs.Module.1 {{ }}", Module
    return f"hs.Module.1 { concatenateFields(key) }", Module

  @staticmethod
  def angle_query(*, packageId: Optional[Tuple[()]] = None, moduleName: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "HsModule":
    raise Exception("this function can only be called from @angle_query")

class HsTargetUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hs.TargetUses.2 {{ }}", TargetUses
    return f"hs.TargetUses.2 { concatenateFields(key) }", TargetUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, uses: Optional[Tuple[()]] = None) -> "HsTargetUses":
    raise Exception("this function can only be called from @angle_query")

class HsFunctionNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hs.FunctionNameLowerCase.1 {{ }}", FunctionNameLowerCase
    return f"hs.FunctionNameLowerCase.1 { concatenateFields(key) }", FunctionNameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: Optional[str] = None, name: Optional[Tuple[()]] = None) -> "HsFunctionNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class HsFileDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hs.FileDefinition.2 {{ }}", FileDefinition
    return f"hs.FileDefinition.2 { concatenateFields(key) }", FileDefinition

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, defn: Optional[Tuple[()]] = None) -> "HsFileDefinition":
    raise Exception("this function can only be called from @angle_query")

class HsSourceModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hs.SourceModule.1 {{ }}", SourceModule
    return f"hs.SourceModule.1 { concatenateFields(key) }", SourceModule

  @staticmethod
  def angle_query(*, moduleName: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "HsSourceModule":
    raise Exception("this function can only be called from @angle_query")

class HsFunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hs.FunctionDefinition.1 {{ }}", FunctionDefinition
    return f"hs.FunctionDefinition.1 { concatenateFields(key) }", FunctionDefinition

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "HsFunctionDefinition":
    raise Exception("this function can only be called from @angle_query")

class HsClassInstance(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hs.ClassInstance.1 {{ }}", ClassInstance
    return f"hs.ClassInstance.1 { concatenateFields(key) }", ClassInstance

  @staticmethod
  def angle_query(*, typeclass: Optional[Tuple[()]] = None, instance: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "HsClassInstance":
    raise Exception("this function can only be called from @angle_query")

class HsClass(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hs.Class.1 {{ }}", Class
    return f"hs.Class.1 { concatenateFields(key) }", Class

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "HsClass":
    raise Exception("this function can only be called from @angle_query")

class HsFunctionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hs.FunctionName.1 {{ }}", FunctionName
    return f"hs.FunctionName.1 {key}", FunctionName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HsFunctionName":
    raise Exception("this function can only be called from @angle_query")

class HsModuleDefinitions(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hs.ModuleDefinitions.1 {{ }}", ModuleDefinitions
    return f"hs.ModuleDefinitions.1 { concatenateFields(key) }", ModuleDefinitions

  @staticmethod
  def angle_query(*, module: Optional[Tuple[()]] = None, functionDefinitions: Optional[Tuple[()]] = None) -> "HsModuleDefinitions":
    raise Exception("this function can only be called from @angle_query")

class HsModuleNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hs.ModuleNameLowerCase.1 {{ }}", ModuleNameLowerCase
    return f"hs.ModuleNameLowerCase.1 { concatenateFields(key) }", ModuleNameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: Optional[str] = None, name: Optional[Tuple[()]] = None) -> "HsModuleNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class HsXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hs.XRef.2 {{ }}", XRef
    return f"hs.XRef.2 { concatenateFields(key) }", XRef

  @staticmethod
  def angle_query(*, loc: Optional[Tuple[()]] = None, ref: Optional[Tuple[()]] = None) -> "HsXRef":
    raise Exception("this function can only be called from @angle_query")

class HsClassName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hs.ClassName.1 {{ }}", ClassName
    return f"hs.ClassName.1 {key}", ClassName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HsClassName":
    raise Exception("this function can only be called from @angle_query")

class HsType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hs.Type.1 {{ }}", Type
    return f"hs.Type.1 {key}", Type

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HsType":
    raise Exception("this function can only be called from @angle_query")

class HsFileXRefMap(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hs.FileXRefMap.2 {{ }}", FileXRefMap
    return f"hs.FileXRefMap.2 { concatenateFields(key) }", FileXRefMap

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, refs: Optional[Tuple[()]] = None) -> "HsFileXRefMap":
    raise Exception("this function can only be called from @angle_query")


