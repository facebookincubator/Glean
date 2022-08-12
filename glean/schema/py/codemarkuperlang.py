# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSCodemarkupErlangErlangEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.erlang.ErlangEntityInfo.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupErlangErlangEntityInfo":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupErlangErlangEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.erlang.ErlangEntityLocation.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupErlangErlangEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupErlangErlangResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.erlang.ErlangResolveLocation.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupErlangErlangResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupErlangErlangFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.erlang.ErlangFileEntityXRefLocations.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupErlangErlangFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupErlangErlangEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.erlang.ErlangEntityUses.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupErlangErlangEntityUses":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupErlangErlangEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.erlang.ErlangEntityKind.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupErlangErlangEntityKind":
    raise Exception("this function can only be called from @angle_query")


