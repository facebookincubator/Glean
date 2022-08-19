# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.codemarkuppython.types import (
    PythonPythonEntityInfo,
    PythonPythonEntityLocation,
    PythonPythonResolveLocation,
    PythonPythonContainsChildEntity,
    PythonPythonFileEntityXRefLocations,
    PythonPythonEntityKind,
    PythonPythonEntityUses,
    PythonNonImportPythonDeclarationInfo,
    PythonNonImportPythonDeclarationKind,
    PythonPythonEntityNameAndLocation,
)


class CodemarkupPythonPythonEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.python.PythonEntityInfo.2 { { } }", PythonPythonEntityInfo

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupPythonPythonEntityInfo":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPythonPythonEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.python.PythonEntityLocation.2 { { } }", PythonPythonEntityLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupPythonPythonEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPythonPythonResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.python.PythonResolveLocation.2 { { } }", PythonPythonResolveLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupPythonPythonResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPythonPythonContainsChildEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.python.PythonContainsChildEntity.2 { { } }", PythonPythonContainsChildEntity

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupPythonPythonContainsChildEntity":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPythonPythonFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.python.PythonFileEntityXRefLocations.2 { { } }", PythonPythonFileEntityXRefLocations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupPythonPythonFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPythonPythonEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.python.PythonEntityKind.2 { { } }", PythonPythonEntityKind

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupPythonPythonEntityKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPythonPythonEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.python.PythonEntityUses.2 { { } }", PythonPythonEntityUses

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupPythonPythonEntityUses":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPythonNonImportPythonDeclarationInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.python.NonImportPythonDeclarationInfo.2 { { } }", PythonNonImportPythonDeclarationInfo

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupPythonNonImportPythonDeclarationInfo":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPythonNonImportPythonDeclarationKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.python.NonImportPythonDeclarationKind.2 { { } }", PythonNonImportPythonDeclarationKind

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupPythonNonImportPythonDeclarationKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPythonPythonEntityNameAndLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.python.PythonEntityNameAndLocation.2 { { } }", PythonPythonEntityNameAndLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupPythonPythonEntityNameAndLocation":
    raise Exception("this function can only be called from @angle_query")


