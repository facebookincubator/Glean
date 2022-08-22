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
    return f"codemarkup.buck.BuckEntityLocation.2 {{ entity = _, location = _ }}", BuckBuckEntityLocation

  @staticmethod
  def angle_query(*, entity: Tuple[()], location: Tuple[()]) -> "CodemarkupBuckBuckEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupBuckBuckResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.buck.BuckResolveLocation.2 {{ location = _, entity = _ }}", BuckBuckResolveLocation

  @staticmethod
  def angle_query(*, location: Tuple[()], entity: Tuple[()]) -> "CodemarkupBuckBuckResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupBuckBuckFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.buck.BuckFileEntityXRefLocations.2 {{ file = _, xref = _, entity = _ }}", BuckBuckFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Tuple[()], xref: Tuple[()], entity: Tuple[()]) -> "CodemarkupBuckBuckFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupBuckBuckEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.buck.BuckEntityUses.2 {{ target = _, file = _, span = _ }}", BuckBuckEntityUses

  @staticmethod
  def angle_query(*, target: Tuple[()], file: Tuple[()], span: Tuple[()]) -> "CodemarkupBuckBuckEntityUses":
    raise Exception("this function can only be called from @angle_query")


