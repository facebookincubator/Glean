# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSBuckusesUsesOfTargetHeader(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buckuses.UsesOfTargetHeader.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckusesUsesOfTargetHeader":
    raise Exception("this function can only be called from @angle_query")

class GSBuckusesUsesOfTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buckuses.UsesOfTarget.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckusesUsesOfTarget":
    raise Exception("this function can only be called from @angle_query")


