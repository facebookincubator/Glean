# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.codemarkuperlang.types import (
    ErlangErlangEntityInfo,
    ErlangErlangEntityLocation,
    ErlangErlangResolveLocation,
    ErlangErlangFileEntityXRefLocations,
    ErlangErlangEntityUses,
    ErlangErlangEntityKind,
)


class CodemarkupErlangErlangEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.erlang.ErlangEntityInfo.2 {{ }}", ErlangErlangEntityInfo
    return f"codemarkup.erlang.ErlangEntityInfo.2 {{ entity = _, info = _ }}", ErlangErlangEntityInfo

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, info: Optional[Tuple[()]] = None) -> "CodemarkupErlangErlangEntityInfo":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupErlangErlangEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.erlang.ErlangEntityLocation.2 {{ }}", ErlangErlangEntityLocation
    return f"codemarkup.erlang.ErlangEntityLocation.2 {{ entity = _, location = _ }}", ErlangErlangEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupErlangErlangEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupErlangErlangResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.erlang.ErlangResolveLocation.2 {{ }}", ErlangErlangResolveLocation
    return f"codemarkup.erlang.ErlangResolveLocation.2 {{ location = _, entity = _ }}", ErlangErlangResolveLocation

  @staticmethod
  def angle_query(*, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupErlangErlangResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupErlangErlangFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.erlang.ErlangFileEntityXRefLocations.2 {{ }}", ErlangErlangFileEntityXRefLocations
    return f"codemarkup.erlang.ErlangFileEntityXRefLocations.2 {{ file = _, xref = _, entity = _ }}", ErlangErlangFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupErlangErlangFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupErlangErlangEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.erlang.ErlangEntityUses.2 {{ }}", ErlangErlangEntityUses
    return f"codemarkup.erlang.ErlangEntityUses.2 {{ target = _, file = _, span = _ }}", ErlangErlangEntityUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "CodemarkupErlangErlangEntityUses":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupErlangErlangEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.erlang.ErlangEntityKind.2 {{ }}", ErlangErlangEntityKind
    return f"codemarkup.erlang.ErlangEntityKind.2 {{ entity = _, kind = _ }}", ErlangErlangEntityKind

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupErlangErlangEntityKind":
    raise Exception("this function can only be called from @angle_query")


