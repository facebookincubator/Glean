# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


from glean.schema.lionheadlionizer.types import (
    LionizerFindFunction,
    LionizerFindFunctionWithDef,
)


class LionheadLionizerFindFunction(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lionhead.lionizer.FindFunction.11 {{ }}", LionizerFindFunction
    return f"lionhead.lionizer.FindFunction.11 { concatenateFields(key) }", LionizerFindFunction

  @staticmethod
  def angle_query(*, key: Optional[Tuple[()]] = None, value: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "LionheadLionizerFindFunction":
    raise Exception("this function can only be called from @angle_query")

class LionheadLionizerFindFunctionWithDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lionhead.lionizer.FindFunctionWithDef.11 {{ }}", LionizerFindFunctionWithDef
    return f"lionhead.lionizer.FindFunctionWithDef.11 { concatenateFields(key) }", LionizerFindFunctionWithDef

  @staticmethod
  def angle_query(*, key: Optional[Tuple[()]] = None, value: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None, definition: Optional[Tuple[()]] = None) -> "LionheadLionizerFindFunctionWithDef":
    raise Exception("this function can only be called from @angle_query")


