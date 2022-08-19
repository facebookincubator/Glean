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
    return f"codemarkup.pp.PpEntityInfo.3 { { } }", PpPpEntityInfo

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupPpPpEntityInfo":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPPEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PPEntityLocation.3 { { } }", PpPPEntityLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupPpPPEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpResolveLocation.3 { { } }", PpPpResolveLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupPpPpResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpIncludeXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpIncludeXRefLocations.3 { { } }", PpPpIncludeXRefLocations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupPpPpIncludeXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpEntityKind.3 { { } }", PpPpEntityKind

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupPpPpEntityKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpFileEntityXRefLocations.3 { { } }", PpPpFileEntityXRefLocations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupPpPpFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpEntityTraceXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpEntityTraceXRefLocations.3 { { } }", PpPpEntityTraceXRefLocations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupPpPpEntityTraceXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpResolveTraceLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpResolveTraceLocation.3 { { } }", PpPpResolveTraceLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupPpPpResolveTraceLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpFileEntityTraceXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpFileEntityTraceXRefLocations.3 { { } }", PpPpFileEntityTraceXRefLocations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupPpPpFileEntityTraceXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpFileEntityTraceLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpFileEntityTraceLocations.3 { { } }", PpPpFileEntityTraceLocations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupPpPpFileEntityTraceLocations":
    raise Exception("this function can only be called from @angle_query")


