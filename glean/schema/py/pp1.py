# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSPp1Define(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"pp1.Define.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSPp1Define":
    raise Exception("this function can only be called from @angle_query")

class GSPp1Undef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"pp1.Undef.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSPp1Undef":
    raise Exception("this function can only be called from @angle_query")

class GSPp1Use(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"pp1.Use.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSPp1Use":
    raise Exception("this function can only be called from @angle_query")

class GSPp1Include(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"pp1.Include.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSPp1Include":
    raise Exception("this function can only be called from @angle_query")

class GSPp1Macro(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"pp1.Macro.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSPp1Macro":
    raise Exception("this function can only be called from @angle_query")


