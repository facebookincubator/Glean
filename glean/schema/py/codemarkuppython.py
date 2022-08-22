# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
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
    return f"codemarkup.python.PythonEntityInfo.2 {{ entity = _, info = _ }}", PythonPythonEntityInfo

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, info: Optional[Tuple[()]] = None) -> "CodemarkupPythonPythonEntityInfo":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPythonPythonEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.python.PythonEntityLocation.2 {{ entity = _, location = _ }}", PythonPythonEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupPythonPythonEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPythonPythonResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.python.PythonResolveLocation.2 {{ location = _, entity = _ }}", PythonPythonResolveLocation

  @staticmethod
  def angle_query(*, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupPythonPythonResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPythonPythonContainsChildEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.python.PythonContainsChildEntity.2 {{ parent = _, child = _ }}", PythonPythonContainsChildEntity

  @staticmethod
  def angle_query(*, parent: Optional[Tuple[()]] = None, child: Optional[Tuple[()]] = None) -> "CodemarkupPythonPythonContainsChildEntity":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPythonPythonFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.python.PythonFileEntityXRefLocations.2 {{ file = _, xref = _, entity = _ }}", PythonPythonFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupPythonPythonFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPythonPythonEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.python.PythonEntityKind.2 {{ entity = _, kind = _ }}", PythonPythonEntityKind

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupPythonPythonEntityKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPythonPythonEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.python.PythonEntityUses.2 {{ target = _, file = _, span = _ }}", PythonPythonEntityUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "CodemarkupPythonPythonEntityUses":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPythonNonImportPythonDeclarationInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.python.NonImportPythonDeclarationInfo.2 {{ declaration = _, info = _ }}", PythonNonImportPythonDeclarationInfo

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, info: Optional[Tuple[()]] = None) -> "CodemarkupPythonNonImportPythonDeclarationInfo":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPythonNonImportPythonDeclarationKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.python.NonImportPythonDeclarationKind.2 {{ declaration = _, kind = _ }}", PythonNonImportPythonDeclarationKind

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupPythonNonImportPythonDeclarationKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPythonPythonEntityNameAndLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.python.PythonEntityNameAndLocation.2 {{ entity = _, name = _, file = _, span = _ }}", PythonPythonEntityNameAndLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, name: Optional[str] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "CodemarkupPythonPythonEntityNameAndLocation":
    raise Exception("this function can only be called from @angle_query")


