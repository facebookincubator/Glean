# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSBuckusesUsesOfTargetHeader(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"buckuses.UsesOfTargetHeader.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSBuckusesUsesOfTargetHeader":
    raise Exception("this function can only be called from @angle_query")

class GSBuckusesUsesOfTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"buckuses.UsesOfTarget.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSBuckusesUsesOfTarget":
    raise Exception("this function can only be called from @angle_query")


