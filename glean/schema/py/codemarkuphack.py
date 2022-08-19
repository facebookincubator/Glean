# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.codemarkuphack.types import (
    HackHackEntityInfo,
    HackHackEntityLocation,
    HackHackVisibility,
    HackHackAnnotation,
    HackHackResolveLocation,
    HackHackContainsChildEntity,
    HackHackFileEntityXRefLocations,
    HackHackEntityUses,
    HackHackEntityKind,
    HackHackFileEntityXRefSpans,
)


class CodemarkupHackHackEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackEntityInfo.2 { { } }", HackHackEntityInfo

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupHackHackEntityInfo":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupHackHackEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackEntityLocation.2 { { } }", HackHackEntityLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupHackHackEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupHackHackVisibility(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackVisibility.2 { { } }", HackHackVisibility

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupHackHackVisibility":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupHackHackAnnotation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackAnnotation.2 { { } }", HackHackAnnotation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupHackHackAnnotation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupHackHackResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackResolveLocation.2 { { } }", HackHackResolveLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupHackHackResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupHackHackContainsChildEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackContainsChildEntity.2 { { } }", HackHackContainsChildEntity

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupHackHackContainsChildEntity":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupHackHackFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackFileEntityXRefLocations.2 { { } }", HackHackFileEntityXRefLocations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupHackHackFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupHackHackEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackEntityUses.2 { { } }", HackHackEntityUses

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupHackHackEntityUses":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupHackHackEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackEntityKind.2 { { } }", HackHackEntityKind

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupHackHackEntityKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupHackHackFileEntityXRefSpans(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackFileEntityXRefSpans.2 { { } }", HackHackFileEntityXRefSpans

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupHackHackFileEntityXRefSpans":
    raise Exception("this function can only be called from @angle_query")


