# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate


from glean.schema.builtin.types import (
    Unit,
)




class BuiltinUnit(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R]) -> Tuple[str, Struct]:
    return f"builtin.Unit.1 _", Unit

  @staticmethod
  def angle_query() -> "BuiltinUnit":
    raise Exception("this function can only be called from @angle_query")




