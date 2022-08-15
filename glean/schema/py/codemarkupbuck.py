# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSCodemarkupBuckBuckEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.buck.BuckEntityLocation.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupBuckBuckEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupBuckBuckResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.buck.BuckResolveLocation.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupBuckBuckResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupBuckBuckFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.buck.BuckFileEntityXRefLocations.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupBuckBuckFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupBuckBuckEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.buck.BuckEntityUses.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupBuckBuckEntityUses":
    raise Exception("this function can only be called from @angle_query")


