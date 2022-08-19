# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.codemarkupbuck.types import (
    BuckBuckEntityLocation,
    BuckBuckResolveLocation,
    BuckBuckFileEntityXRefLocations,
    BuckBuckEntityUses,
)


class CodemarkupBuckBuckEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.buck.BuckEntityLocation.2 { { } }", BuckBuckEntityLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupBuckBuckEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupBuckBuckResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.buck.BuckResolveLocation.2 { { } }", BuckBuckResolveLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupBuckBuckResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupBuckBuckFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.buck.BuckFileEntityXRefLocations.2 { { } }", BuckBuckFileEntityXRefLocations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupBuckBuckFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupBuckBuckEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.buck.BuckEntityUses.2 { { } }", BuckBuckEntityUses

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupBuckBuckEntityUses":
    raise Exception("this function can only be called from @angle_query")


