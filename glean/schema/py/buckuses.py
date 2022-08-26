# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


from glean.schema.buckuses.types import (
    UsesOfTargetHeader,
    UsesOfTarget,
)


class BuckusesUsesOfTargetHeader(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buckuses.UsesOfTargetHeader.4 {{ }}", UsesOfTargetHeader
    return f"buckuses.UsesOfTargetHeader.4 { concatenateFields(key) }", UsesOfTargetHeader

  @staticmethod
  def angle_query(*, locator: Optional[Tuple[()]] = None, exportedHeader: Optional[Tuple[()]] = None) -> "BuckusesUsesOfTargetHeader":
    raise Exception("this function can only be called from @angle_query")

class BuckusesUsesOfTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buckuses.UsesOfTarget.4 {{ }}", UsesOfTarget
    return f"buckuses.UsesOfTarget.4 { concatenateFields(key) }", UsesOfTarget

  @staticmethod
  def angle_query(*, locator: Optional[Tuple[()]] = None, use_xref: Optional[Tuple[()]] = None, use_file: Optional[Tuple[()]] = None) -> "BuckusesUsesOfTarget":
    raise Exception("this function can only be called from @angle_query")


