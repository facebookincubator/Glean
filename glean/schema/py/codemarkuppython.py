# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSCodemarkupPythonPythonEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.python.PythonEntityInfo.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupPythonPythonEntityInfo":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupPythonPythonEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.python.PythonEntityLocation.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupPythonPythonEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupPythonPythonResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.python.PythonResolveLocation.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupPythonPythonResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupPythonPythonFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.python.PythonFileEntityXRefLocations.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupPythonPythonFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupPythonPythonEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.python.PythonEntityKind.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupPythonPythonEntityKind":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupPythonPythonEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.python.PythonEntityUses.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupPythonPythonEntityUses":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupPythonNonImportPythonDeclarationInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.python.NonImportPythonDeclarationInfo.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupPythonNonImportPythonDeclarationInfo":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupPythonNonImportPythonDeclarationKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.python.NonImportPythonDeclarationKind.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupPythonNonImportPythonDeclarationKind":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupPythonPythonEntityNameAndLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.python.PythonEntityNameAndLocation.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupPythonPythonEntityNameAndLocation":
    raise Exception("this function can only be called from @angle_query")


