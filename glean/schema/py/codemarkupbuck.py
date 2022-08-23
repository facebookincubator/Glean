# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
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
    if key is None:
      return f"codemarkup.buck.BuckEntityLocation.2 {{ }}", BuckBuckEntityLocation
    return f"codemarkup.buck.BuckEntityLocation.2 {{ entity = _, location = _ }}", BuckBuckEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupBuckBuckEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupBuckBuckResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.buck.BuckResolveLocation.2 {{ }}", BuckBuckResolveLocation
    return f"codemarkup.buck.BuckResolveLocation.2 {{ location = _, entity = _ }}", BuckBuckResolveLocation

  @staticmethod
  def angle_query(*, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupBuckBuckResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupBuckBuckFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.buck.BuckFileEntityXRefLocations.2 {{ }}", BuckBuckFileEntityXRefLocations
    return f"codemarkup.buck.BuckFileEntityXRefLocations.2 {{ file = _, xref = _, entity = _ }}", BuckBuckFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupBuckBuckFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupBuckBuckEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.buck.BuckEntityUses.2 {{ }}", BuckBuckEntityUses
    return f"codemarkup.buck.BuckEntityUses.2 {{ target = _, file = _, span = _ }}", BuckBuckEntityUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "CodemarkupBuckBuckEntityUses":
    raise Exception("this function can only be called from @angle_query")


