# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSLionheadLionizerFindFunction(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lionhead.lionizer.FindFunction.11 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLionheadLionizerFindFunction":
    raise Exception("this function can only be called from @angle_query")

class GSLionheadLionizerFindFunctionWithDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lionhead.lionizer.FindFunctionWithDef.11 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLionheadLionizerFindFunctionWithDef":
    raise Exception("this function can only be called from @angle_query")


