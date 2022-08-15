# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSHsPackageId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.PackageId.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsPackageId":
    raise Exception("this function can only be called from @angle_query")

class GSHsDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.Definition.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSHsDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSHsClassNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.ClassNameLowerCase.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSHsClassNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class GSHsDefinitionNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.DefinitionNameLowerCase.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSHsDefinitionNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class GSHsModuleName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.ModuleName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsModuleName":
    raise Exception("this function can only be called from @angle_query")

class GSHsDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.Definition.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSHsDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSHsDefinitionLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.DefinitionLocation.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSHsDefinitionLocation":
    raise Exception("this function can only be called from @angle_query")

class GSHsDefinitionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.DefinitionName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsDefinitionName":
    raise Exception("this function can only be called from @angle_query")

class GSHsModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.Module.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSHsModule":
    raise Exception("this function can only be called from @angle_query")

class GSHsTargetUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.TargetUses.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSHsTargetUses":
    raise Exception("this function can only be called from @angle_query")

class GSHsFunctionNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.FunctionNameLowerCase.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSHsFunctionNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class GSHsFileDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.FileDefinition.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSHsFileDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSHsSourceModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.SourceModule.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSHsSourceModule":
    raise Exception("this function can only be called from @angle_query")

class GSHsFunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.FunctionDefinition.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSHsFunctionDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSHsClassInstance(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.ClassInstance.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSHsClassInstance":
    raise Exception("this function can only be called from @angle_query")

class GSHsClass(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.Class.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSHsClass":
    raise Exception("this function can only be called from @angle_query")

class GSHsFunctionName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.FunctionName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsFunctionName":
    raise Exception("this function can only be called from @angle_query")

class GSHsModuleDefinitions(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.ModuleDefinitions.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSHsModuleDefinitions":
    raise Exception("this function can only be called from @angle_query")

class GSHsModuleNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.ModuleNameLowerCase.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSHsModuleNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class GSHsXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.XRef.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSHsXRef":
    raise Exception("this function can only be called from @angle_query")

class GSHsClassName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.ClassName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsClassName":
    raise Exception("this function can only be called from @angle_query")

class GSHsType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.Type.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHsType":
    raise Exception("this function can only be called from @angle_query")

class GSHsFileXRefMap(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hs.FileXRefMap.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSHsFileXRefMap":
    raise Exception("this function can only be called from @angle_query")


