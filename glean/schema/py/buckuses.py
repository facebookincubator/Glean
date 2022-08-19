# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.buckuses.types import (
    UsesOfTargetHeader,
    UsesOfTarget,
)


class BuckusesUsesOfTargetHeader(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buckuses.UsesOfTargetHeader.4 { { } }", UsesOfTargetHeader

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckusesUsesOfTargetHeader":
    raise Exception("this function can only be called from @angle_query")

class BuckusesUsesOfTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buckuses.UsesOfTarget.4 { { } }", UsesOfTarget

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckusesUsesOfTarget":
    raise Exception("this function can only be called from @angle_query")


