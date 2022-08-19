# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.pp1.types import (
    Define,
    Undef,
    Use,
    Include,
    Macro,
)


class Pp1Define(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"pp1.Define.1 { { } }", Define

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Pp1Define":
    raise Exception("this function can only be called from @angle_query")

class Pp1Undef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"pp1.Undef.1 { { } }", Undef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Pp1Undef":
    raise Exception("this function can only be called from @angle_query")

class Pp1Use(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"pp1.Use.1 { { } }", Use

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Pp1Use":
    raise Exception("this function can only be called from @angle_query")

class Pp1Include(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"pp1.Include.1 { { } }", Include

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "Pp1Include":
    raise Exception("this function can only be called from @angle_query")

class Pp1Macro(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"pp1.Macro.1 { json.dumps(key) }", Macro

  @staticmethod
  def angle_query(*, name: str) -> "Pp1Macro":
    raise Exception("this function can only be called from @angle_query")


