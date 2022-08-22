# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.codemarkuppp.types import (
    PpPpEntityInfo,
    PpPPEntityLocation,
    PpPpResolveLocation,
    PpPpIncludeXRefLocations,
    PpPpEntityKind,
    PpPpFileEntityXRefLocations,
    PpPpEntityTraceXRefLocations,
    PpPpResolveTraceLocation,
    PpPpFileEntityTraceXRefLocations,
    PpPpFileEntityTraceLocations,
)


class CodemarkupPpPpEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpEntityInfo.3 {{ entity = _, info = _ }}", PpPpEntityInfo

  @staticmethod
  def angle_query(*, entity: Tuple[()], info: Tuple[()]) -> "CodemarkupPpPpEntityInfo":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPPEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PPEntityLocation.3 {{ entity = _, location = _ }}", PpPPEntityLocation

  @staticmethod
  def angle_query(*, entity: Tuple[()], location: Tuple[()]) -> "CodemarkupPpPPEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpResolveLocation.3 {{ location = _, entity = _ }}", PpPpResolveLocation

  @staticmethod
  def angle_query(*, location: Tuple[()], entity: Tuple[()]) -> "CodemarkupPpPpResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpIncludeXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpIncludeXRefLocations.3 {{ trace = _, range = _, target = _ }}", PpPpIncludeXRefLocations

  @staticmethod
  def angle_query(*, trace: Tuple[()], range: Tuple[()], target: Tuple[()]) -> "CodemarkupPpPpIncludeXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpEntityKind.3 {{ entity = _, kind = _ }}", PpPpEntityKind

  @staticmethod
  def angle_query(*, entity: Tuple[()], kind: Tuple[()]) -> "CodemarkupPpPpEntityKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpFileEntityXRefLocations.3 {{ file = _, xref = _, entity = _ }}", PpPpFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Tuple[()], xref: Tuple[()], entity: Tuple[()]) -> "CodemarkupPpPpFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpEntityTraceXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpEntityTraceXRefLocations.3 {{ trace = _, xref = _, entity = _ }}", PpPpEntityTraceXRefLocations

  @staticmethod
  def angle_query(*, trace: Tuple[()], xref: Tuple[()], entity: Tuple[()]) -> "CodemarkupPpPpEntityTraceXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpResolveTraceLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpResolveTraceLocation.3 {{ trace = _, location = _, entity = _ }}", PpPpResolveTraceLocation

  @staticmethod
  def angle_query(*, trace: Tuple[()], location: Tuple[()], entity: Tuple[()]) -> "CodemarkupPpPpResolveTraceLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpFileEntityTraceXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpFileEntityTraceXRefLocations.3 {{ file = _, trace = _, xref = _, entity = _ }}", PpPpFileEntityTraceXRefLocations

  @staticmethod
  def angle_query(*, file: Tuple[()], trace: Tuple[()], xref: Tuple[()], entity: Tuple[()]) -> "CodemarkupPpPpFileEntityTraceXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpFileEntityTraceLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpFileEntityTraceLocations.3 {{ file = _, trace = _, location = _, entity = _ }}", PpPpFileEntityTraceLocations

  @staticmethod
  def angle_query(*, file: Tuple[()], trace: Tuple[()], location: Tuple[()], entity: Tuple[()]) -> "CodemarkupPpPpFileEntityTraceLocations":
    raise Exception("this function can only be called from @angle_query")


