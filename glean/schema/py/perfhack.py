# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.perfhack.types import (
    HackLoopCounts,
    HackFunctionData,
    HackReturnPercentages,
)


class PerfHackLoopCounts(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"perf.hack.LoopCounts.1 {{ file = _, loop_counts = _ }}", HackLoopCounts

  @staticmethod
  def angle_query(*, file: Tuple[()], loop_counts: Tuple[()]) -> "PerfHackLoopCounts":
    raise Exception("this function can only be called from @angle_query")

class PerfHackFunctionData(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"perf.hack.FunctionData.1 {{ function_definition = _, function_name = _, full_name = _, calls_per_request = _, inclusive_gcpu = _, exclusive_gcpu = _, total_samples = _, callers = _, products = _ }}", HackFunctionData

  @staticmethod
  def angle_query(*, function_definition: Tuple[()], function_name: Tuple[()], full_name: Tuple[()], calls_per_request: Tuple[()], inclusive_gcpu: Tuple[()], exclusive_gcpu: Tuple[()], total_samples: int, callers: Tuple[()], products: Tuple[()]) -> "PerfHackFunctionData":
    raise Exception("this function can only be called from @angle_query")

class PerfHackReturnPercentages(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"perf.hack.ReturnPercentages.1 {{ file = _, full_name = _, total_sample_count = _, return_counts = _ }}", HackReturnPercentages

  @staticmethod
  def angle_query(*, file: Tuple[()], full_name: str, total_sample_count: int, return_counts: Tuple[()]) -> "PerfHackReturnPercentages":
    raise Exception("this function can only be called from @angle_query")


