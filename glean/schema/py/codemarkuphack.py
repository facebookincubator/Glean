# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
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
    return f"codemarkup.hack.HackEntityInfo.2 {{ entity = _, info = _ }}", HackHackEntityInfo

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, info: Optional[Tuple[()]] = None) -> "CodemarkupHackHackEntityInfo":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupHackHackEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackEntityLocation.2 {{ entity = _, location = _ }}", HackHackEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupHackHackEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupHackHackVisibility(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackVisibility.2 {{ entity = _, visibility = _ }}", HackHackVisibility

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, visibility: Optional[Tuple[()]] = None) -> "CodemarkupHackHackVisibility":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupHackHackAnnotation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackAnnotation.2 {{ entity = _, anns = _ }}", HackHackAnnotation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, anns: Optional[Tuple[()]] = None) -> "CodemarkupHackHackAnnotation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupHackHackResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackResolveLocation.2 {{ location = _, entity = _ }}", HackHackResolveLocation

  @staticmethod
  def angle_query(*, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupHackHackResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupHackHackContainsChildEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackContainsChildEntity.2 {{ parent = _, child = _ }}", HackHackContainsChildEntity

  @staticmethod
  def angle_query(*, parent: Optional[Tuple[()]] = None, child: Optional[Tuple[()]] = None) -> "CodemarkupHackHackContainsChildEntity":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupHackHackFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackFileEntityXRefLocations.2 {{ file = _, xref = _, entity = _ }}", HackHackFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupHackHackFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupHackHackEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackEntityUses.2 {{ target = _, file = _, span = _ }}", HackHackEntityUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "CodemarkupHackHackEntityUses":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupHackHackEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackEntityKind.2 {{ entity = _, kind = _ }}", HackHackEntityKind

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupHackHackEntityKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupHackHackFileEntityXRefSpans(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackFileEntityXRefSpans.2 {{ file = _, span = _, entity = _ }}", HackHackFileEntityXRefSpans

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupHackHackFileEntityXRefSpans":
    raise Exception("this function can only be called from @angle_query")


