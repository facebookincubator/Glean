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
    return f"perf.hack.LoopCounts.1 { { } }", HackLoopCounts

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PerfHackLoopCounts":
    raise Exception("this function can only be called from @angle_query")

class PerfHackFunctionData(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"perf.hack.FunctionData.1 { { } }", HackFunctionData

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PerfHackFunctionData":
    raise Exception("this function can only be called from @angle_query")

class PerfHackReturnPercentages(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"perf.hack.ReturnPercentages.1 { { } }", HackReturnPercentages

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PerfHackReturnPercentages":
    raise Exception("this function can only be called from @angle_query")


