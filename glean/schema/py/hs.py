# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSHsPackageId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hs.PackageId.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsPackageId":
    raise Exception("this function can only be called from @angle_query")

class GSHsDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hs.Definition.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSHsClassNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hs.ClassNameLowerCase.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsClassNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class GSHsDefinitionNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hs.DefinitionNameLowerCase.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsDefinitionNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class GSHsModuleName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hs.ModuleName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsModuleName":
    raise Exception("this function can only be called from @angle_query")

class GSHsDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hs.Definition.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSHsDefinitionLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hs.DefinitionLocation.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsDefinitionLocation":
    raise Exception("this function can only be called from @angle_query")

class GSHsDefinitionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hs.DefinitionName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsDefinitionName":
    raise Exception("this function can only be called from @angle_query")

class GSHsModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hs.Module.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsModule":
    raise Exception("this function can only be called from @angle_query")

class GSHsTargetUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hs.TargetUses.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsTargetUses":
    raise Exception("this function can only be called from @angle_query")

class GSHsFunctionNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hs.FunctionNameLowerCase.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsFunctionNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class GSHsFileDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hs.FileDefinition.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsFileDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSHsSourceModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hs.SourceModule.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsSourceModule":
    raise Exception("this function can only be called from @angle_query")

class GSHsFunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hs.FunctionDefinition.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsFunctionDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSHsClassInstance(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hs.ClassInstance.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsClassInstance":
    raise Exception("this function can only be called from @angle_query")

class GSHsClass(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hs.Class.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsClass":
    raise Exception("this function can only be called from @angle_query")

class GSHsFunctionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hs.FunctionName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsFunctionName":
    raise Exception("this function can only be called from @angle_query")

class GSHsModuleDefinitions(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hs.ModuleDefinitions.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsModuleDefinitions":
    raise Exception("this function can only be called from @angle_query")

class GSHsModuleNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hs.ModuleNameLowerCase.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsModuleNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class GSHsXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hs.XRef.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsXRef":
    raise Exception("this function can only be called from @angle_query")

class GSHsClassName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hs.ClassName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsClassName":
    raise Exception("this function can only be called from @angle_query")

class GSHsType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hs.Type.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsType":
    raise Exception("this function can only be called from @angle_query")

class GSHsFileXRefMap(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hs.FileXRefMap.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsFileXRefMap":
    raise Exception("this function can only be called from @angle_query")


