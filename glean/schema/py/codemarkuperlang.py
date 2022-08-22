# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
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
    return f"codemarkup.erlang.ErlangEntityInfo.2 {{ entity = _, info = _ }}", ErlangErlangEntityInfo

  @staticmethod
  def angle_query(*, entity: Tuple[()], info: Tuple[()]) -> "CodemarkupErlangErlangEntityInfo":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupErlangErlangEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.erlang.ErlangEntityLocation.2 {{ entity = _, location = _ }}", ErlangErlangEntityLocation

  @staticmethod
  def angle_query(*, entity: Tuple[()], location: Tuple[()]) -> "CodemarkupErlangErlangEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupErlangErlangResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.erlang.ErlangResolveLocation.2 {{ location = _, entity = _ }}", ErlangErlangResolveLocation

  @staticmethod
  def angle_query(*, location: Tuple[()], entity: Tuple[()]) -> "CodemarkupErlangErlangResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupErlangErlangFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.erlang.ErlangFileEntityXRefLocations.2 {{ file = _, xref = _, entity = _ }}", ErlangErlangFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Tuple[()], xref: Tuple[()], entity: Tuple[()]) -> "CodemarkupErlangErlangFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupErlangErlangEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.erlang.ErlangEntityUses.2 {{ target = _, file = _, span = _ }}", ErlangErlangEntityUses

  @staticmethod
  def angle_query(*, target: Tuple[()], file: Tuple[()], span: Tuple[()]) -> "CodemarkupErlangErlangEntityUses":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupErlangErlangEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.erlang.ErlangEntityKind.2 {{ entity = _, kind = _ }}", ErlangErlangEntityKind

  @staticmethod
  def angle_query(*, entity: Tuple[()], kind: Tuple[()]) -> "CodemarkupErlangErlangEntityKind":
    raise Exception("this function can only be called from @angle_query")


