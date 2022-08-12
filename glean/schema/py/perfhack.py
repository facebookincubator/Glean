# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSPerfHackLoopCounts(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"perf.hack.LoopCounts.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSPerfHackLoopCounts":
    raise Exception("this function can only be called from @angle_query")

class GSPerfHackFunctionData(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"perf.hack.FunctionData.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSPerfHackFunctionData":
    raise Exception("this function can only be called from @angle_query")

class GSPerfHackReturnPercentages(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"perf.hack.ReturnPercentages.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSPerfHackReturnPercentages":
    raise Exception("this function can only be called from @angle_query")


