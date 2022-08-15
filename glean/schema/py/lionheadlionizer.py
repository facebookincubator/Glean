# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSLionheadLionizerFindFunction(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lionhead.lionizer.FindFunction.11 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSLionheadLionizerFindFunction":
    raise Exception("this function can only be called from @angle_query")

class GSLionheadLionizerFindFunctionWithDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lionhead.lionizer.FindFunctionWithDef.11 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSLionheadLionizerFindFunctionWithDef":
    raise Exception("this function can only be called from @angle_query")


