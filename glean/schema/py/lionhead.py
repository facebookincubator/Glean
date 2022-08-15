# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSLionheadCoveredHarness(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lionhead.CoveredHarness.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSLionheadCoveredHarness":
    raise Exception("this function can only be called from @angle_query")

class GSLionheadFbId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lionhead.FbId.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: int) -> "GSLionheadFbId":
    raise Exception("this function can only be called from @angle_query")


