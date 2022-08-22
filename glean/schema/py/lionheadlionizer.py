# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.lionheadlionizer.types import (
    LionizerFindFunction,
    LionizerFindFunctionWithDef,
)


class LionheadLionizerFindFunction(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lionhead.lionizer.FindFunction.11 {{ key = _, value = _, declaration = _ }}", LionizerFindFunction

  @staticmethod
  def angle_query(*, key: Tuple[()], value: Tuple[()], declaration: Tuple[()]) -> "LionheadLionizerFindFunction":
    raise Exception("this function can only be called from @angle_query")

class LionheadLionizerFindFunctionWithDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lionhead.lionizer.FindFunctionWithDef.11 {{ key = _, value = _, declaration = _, definition = _ }}", LionizerFindFunctionWithDef

  @staticmethod
  def angle_query(*, key: Tuple[()], value: Tuple[()], declaration: Tuple[()], definition: Tuple[()]) -> "LionheadLionizerFindFunctionWithDef":
    raise Exception("this function can only be called from @angle_query")


