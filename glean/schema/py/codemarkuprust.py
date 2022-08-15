# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSCodemarkupRustRustEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.rust.RustEntityLocation.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupRustRustEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupRustRustResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.rust.RustResolveLocation.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupRustRustResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupRustRustFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.rust.RustFileEntityXRefLocations.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupRustRustFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupRustRustEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.rust.RustEntityUses.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupRustRustEntityUses":
    raise Exception("this function can only be called from @angle_query")


