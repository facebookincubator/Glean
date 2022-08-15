# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class HsPackageId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.PackageId.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HsPackageId":
    raise Exception("this function can only be called from @angle_query")

class HsDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.Definition.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HsDefinition":
    raise Exception("this function can only be called from @angle_query")

class HsClassNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.ClassNameLowerCase.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HsClassNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class HsDefinitionNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.DefinitionNameLowerCase.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HsDefinitionNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class HsModuleName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.ModuleName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HsModuleName":
    raise Exception("this function can only be called from @angle_query")

class HsDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.Definition.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HsDefinition":
    raise Exception("this function can only be called from @angle_query")

class HsDefinitionLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.DefinitionLocation.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HsDefinitionLocation":
    raise Exception("this function can only be called from @angle_query")

class HsDefinitionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.DefinitionName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HsDefinitionName":
    raise Exception("this function can only be called from @angle_query")

class HsModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.Module.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HsModule":
    raise Exception("this function can only be called from @angle_query")

class HsTargetUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.TargetUses.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HsTargetUses":
    raise Exception("this function can only be called from @angle_query")

class HsFunctionNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.FunctionNameLowerCase.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HsFunctionNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class HsFileDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.FileDefinition.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HsFileDefinition":
    raise Exception("this function can only be called from @angle_query")

class HsSourceModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.SourceModule.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HsSourceModule":
    raise Exception("this function can only be called from @angle_query")

class HsFunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.FunctionDefinition.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HsFunctionDefinition":
    raise Exception("this function can only be called from @angle_query")

class HsClassInstance(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.ClassInstance.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HsClassInstance":
    raise Exception("this function can only be called from @angle_query")

class HsClass(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.Class.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HsClass":
    raise Exception("this function can only be called from @angle_query")

class HsFunctionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.FunctionName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HsFunctionName":
    raise Exception("this function can only be called from @angle_query")

class HsModuleDefinitions(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.ModuleDefinitions.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HsModuleDefinitions":
    raise Exception("this function can only be called from @angle_query")

class HsModuleNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.ModuleNameLowerCase.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HsModuleNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class HsXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.XRef.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HsXRef":
    raise Exception("this function can only be called from @angle_query")

class HsClassName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.ClassName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HsClassName":
    raise Exception("this function can only be called from @angle_query")

class HsType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.Type.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HsType":
    raise Exception("this function can only be called from @angle_query")

class HsFileXRefMap(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.FileXRefMap.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HsFileXRefMap":
    raise Exception("this function can only be called from @angle_query")


