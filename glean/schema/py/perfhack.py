# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSPerfHackLoopCounts(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"perf.hack.LoopCounts.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSPerfHackLoopCounts":
    raise Exception("this function can only be called from @angle_query")

class GSPerfHackFunctionData(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"perf.hack.FunctionData.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSPerfHackFunctionData":
    raise Exception("this function can only be called from @angle_query")

class GSPerfHackReturnPercentages(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"perf.hack.ReturnPercentages.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSPerfHackReturnPercentages":
    raise Exception("this function can only be called from @angle_query")


