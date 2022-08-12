# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSCodemarkupRustRustEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.rust.RustEntityLocation.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupRustRustEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupRustRustResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.rust.RustResolveLocation.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupRustRustResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupRustRustFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.rust.RustFileEntityXRefLocations.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupRustRustFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupRustRustEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.rust.RustEntityUses.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupRustRustEntityUses":
    raise Exception("this function can only be called from @angle_query")


