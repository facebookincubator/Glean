# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
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
  def angle_query(*, entity: Optional[Tuple[()]] = None, info: Optional[Tuple[()]] = None) -> "CodemarkupPpPpEntityInfo":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPPEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PPEntityLocation.3 {{ entity = _, location = _ }}", PpPPEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupPpPPEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpResolveLocation.3 {{ location = _, entity = _ }}", PpPpResolveLocation

  @staticmethod
  def angle_query(*, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupPpPpResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpIncludeXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpIncludeXRefLocations.3 {{ trace = _, range = _, target = _ }}", PpPpIncludeXRefLocations

  @staticmethod
  def angle_query(*, trace: Optional[Tuple[()]] = None, range: Optional[Tuple[()]] = None, target: Optional[Tuple[()]] = None) -> "CodemarkupPpPpIncludeXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpEntityKind.3 {{ entity = _, kind = _ }}", PpPpEntityKind

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupPpPpEntityKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpFileEntityXRefLocations.3 {{ file = _, xref = _, entity = _ }}", PpPpFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupPpPpFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpEntityTraceXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpEntityTraceXRefLocations.3 {{ trace = _, xref = _, entity = _ }}", PpPpEntityTraceXRefLocations

  @staticmethod
  def angle_query(*, trace: Optional[Tuple[()]] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupPpPpEntityTraceXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpResolveTraceLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpResolveTraceLocation.3 {{ trace = _, location = _, entity = _ }}", PpPpResolveTraceLocation

  @staticmethod
  def angle_query(*, trace: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupPpPpResolveTraceLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpFileEntityTraceXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpFileEntityTraceXRefLocations.3 {{ file = _, trace = _, xref = _, entity = _ }}", PpPpFileEntityTraceXRefLocations

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, trace: Optional[Tuple[()]] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupPpPpFileEntityTraceXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpFileEntityTraceLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpFileEntityTraceLocations.3 {{ file = _, trace = _, location = _, entity = _ }}", PpPpFileEntityTraceLocations

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, trace: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupPpPpFileEntityTraceLocations":
    raise Exception("this function can only be called from @angle_query")


