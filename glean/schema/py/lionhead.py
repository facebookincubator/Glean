# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.lionhead.types import (
    CoveredHarness,
    FbId,
)


class LionheadCoveredHarness(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lionhead.CoveredHarness.1 { { } }", CoveredHarness

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LionheadCoveredHarness":
    raise Exception("this function can only be called from @angle_query")

class LionheadFbId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lionhead.FbId.1 { json.dumps(key) }", FbId

  @staticmethod
  def angle_query(*, name: int) -> "LionheadFbId":
    raise Exception("this function can only be called from @angle_query")


