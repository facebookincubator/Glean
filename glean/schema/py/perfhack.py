# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class PerfHackLoopCounts(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"perf.hack.LoopCounts.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PerfHackLoopCounts":
    raise Exception("this function can only be called from @angle_query")

class PerfHackFunctionData(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"perf.hack.FunctionData.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PerfHackFunctionData":
    raise Exception("this function can only be called from @angle_query")

class PerfHackReturnPercentages(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"perf.hack.ReturnPercentages.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PerfHackReturnPercentages":
    raise Exception("this function can only be called from @angle_query")


