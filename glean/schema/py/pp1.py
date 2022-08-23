# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
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
    if key is None:
      return f"pp1.Define.1 {{ }}", Define
    return f"pp1.Define.1 {{ macro = _, source = _ }}", Define

  @staticmethod
  def angle_query(*, macro: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Pp1Define":
    raise Exception("this function can only be called from @angle_query")

class Pp1Undef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"pp1.Undef.1 {{ }}", Undef
    return f"pp1.Undef.1 {{ macro = _, source = _ }}", Undef

  @staticmethod
  def angle_query(*, macro: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Pp1Undef":
    raise Exception("this function can only be called from @angle_query")

class Pp1Use(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"pp1.Use.1 {{ }}", Use
    return f"pp1.Use.1 {{ macro = _, name = _, definition = _, expand = _, source = _ }}", Use

  @staticmethod
  def angle_query(*, macro: Optional[Tuple[()]] = None, name: Optional[Tuple[()]] = None, definition: Optional[Tuple[()]] = None, expand: Optional[bool] = None, source: Optional[Tuple[()]] = None) -> "Pp1Use":
    raise Exception("this function can only be called from @angle_query")

class Pp1Include(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"pp1.Include.1 {{ }}", Include
    return f"pp1.Include.1 {{ file = _, path = _, source = _ }}", Include

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, path: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Pp1Include":
    raise Exception("this function can only be called from @angle_query")

class Pp1Macro(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"pp1.Macro.1 {{ }}", Macro
    return f"pp1.Macro.1 {json.dumps(key)}", Macro

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "Pp1Macro":
    raise Exception("this function can only be called from @angle_query")


