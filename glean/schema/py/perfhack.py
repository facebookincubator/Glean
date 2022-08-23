# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
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
    if key is None:
      return f"perf.hack.LoopCounts.1 {{ }}", HackLoopCounts
    return f"perf.hack.LoopCounts.1 {{ file = _, loop_counts = _ }}", HackLoopCounts

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, loop_counts: Optional[Tuple[()]] = None) -> "PerfHackLoopCounts":
    raise Exception("this function can only be called from @angle_query")

class PerfHackFunctionData(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"perf.hack.FunctionData.1 {{ }}", HackFunctionData
    return f"perf.hack.FunctionData.1 {{ function_definition = _, function_name = _, full_name = _, calls_per_request = _, inclusive_gcpu = _, exclusive_gcpu = _, total_samples = _, callers = _, products = _ }}", HackFunctionData

  @staticmethod
  def angle_query(*, function_definition: Optional[Tuple[()]] = None, function_name: Optional[Tuple[()]] = None, full_name: Optional[Tuple[()]] = None, calls_per_request: Optional[Tuple[()]] = None, inclusive_gcpu: Optional[Tuple[()]] = None, exclusive_gcpu: Optional[Tuple[()]] = None, total_samples: Optional[int] = None, callers: Optional[Tuple[()]] = None, products: Optional[Tuple[()]] = None) -> "PerfHackFunctionData":
    raise Exception("this function can only be called from @angle_query")

class PerfHackReturnPercentages(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"perf.hack.ReturnPercentages.1 {{ }}", HackReturnPercentages
    return f"perf.hack.ReturnPercentages.1 {{ file = _, full_name = _, total_sample_count = _, return_counts = _ }}", HackReturnPercentages

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, full_name: Optional[str] = None, total_sample_count: Optional[int] = None, return_counts: Optional[Tuple[()]] = None) -> "PerfHackReturnPercentages":
    raise Exception("this function can only be called from @angle_query")


