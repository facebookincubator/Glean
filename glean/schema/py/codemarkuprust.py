# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.codemarkuprust.types import (
    RustRustEntityLocation,
    RustRustResolveLocation,
    RustRustFileEntityXRefLocations,
    RustRustEntityUses,
)


class CodemarkupRustRustEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.rust.RustEntityLocation.2 { { } }", RustRustEntityLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupRustRustEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupRustRustResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.rust.RustResolveLocation.2 { { } }", RustRustResolveLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupRustRustResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupRustRustFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.rust.RustFileEntityXRefLocations.2 { { } }", RustRustFileEntityXRefLocations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupRustRustFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupRustRustEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.rust.RustEntityUses.2 { { } }", RustRustEntityUses

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupRustRustEntityUses":
    raise Exception("this function can only be called from @angle_query")


