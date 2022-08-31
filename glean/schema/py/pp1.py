# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.src import *


from glean.schema.pp1.types import (
    Define,
    Undef,
    Use,
    Include,
    Macro,
)


class Pp1Define(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], macro: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"pp1.Define.1 {{ macro = {angle_for(__env, macro)}, source = {angle_for(__env, source)} }}", Define

  @staticmethod
  def angle_query(*, macro: Optional["Pp1Macro"] = None, source: Optional[Tuple[()]] = None) -> "Pp1Define":
    raise Exception("this function can only be called from @angle_query")

class Pp1Undef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], macro: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"pp1.Undef.1 {{ macro = {angle_for(__env, macro)}, source = {angle_for(__env, source)} }}", Undef

  @staticmethod
  def angle_query(*, macro: Optional["Pp1Macro"] = None, source: Optional[Tuple[()]] = None) -> "Pp1Undef":
    raise Exception("this function can only be called from @angle_query")

class Pp1Use(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], macro: ast.Expr, name: ast.Expr, definition: ast.Expr, expand: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"pp1.Use.1 {{ macro = {angle_for(__env, macro)}, name = {angle_for(__env, name)}, definition = {angle_for(__env, definition)}, expand = {angle_for(__env, expand)}, source = {angle_for(__env, source)} }}", Use

  @staticmethod
  def angle_query(*, macro: Optional["Pp1Macro"] = None, name: Optional[Tuple[()]] = None, definition: Optional[Tuple[()]] = None, expand: Optional[bool] = None, source: Optional[Tuple[()]] = None) -> "Pp1Use":
    raise Exception("this function can only be called from @angle_query")

class Pp1Include(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, path: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"pp1.Include.1 {{ file = {angle_for(__env, file)}, path = {angle_for(__env, path)}, source = {angle_for(__env, source)} }}", Include

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, path: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "Pp1Include":
    raise Exception("this function can only be called from @angle_query")

class Pp1Macro(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"pp1.Macro.1 {angle_for(__env, arg)}", Macro

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "Pp1Macro":
    raise Exception("this function can only be called from @angle_query")


