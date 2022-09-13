# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
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
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, macro, 'macro'), angle_for(__env, source, 'source')]))
    return f"pp1.Define.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Define

  @staticmethod
  def angle_query(*, macro: Optional["Pp1Macro"] = None, source: Optional["SrcRange"] = None) -> "Pp1Define":
    raise Exception("this function can only be called from @angle_query")



class Pp1Undef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], macro: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, macro, 'macro'), angle_for(__env, source, 'source')]))
    return f"pp1.Undef.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Undef

  @staticmethod
  def angle_query(*, macro: Optional["Pp1Macro"] = None, source: Optional["SrcRange"] = None) -> "Pp1Undef":
    raise Exception("this function can only be called from @angle_query")



class Pp1Use(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], macro: ast.Expr, name: ast.Expr, definition: ast.Expr, expand: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, macro, 'macro'), angle_for(__env, name, 'name'), angle_for(__env, definition, 'definition'), angle_for(__env, expand, 'expand'), angle_for(__env, source, 'source')]))
    return f"pp1.Use.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Use

  @staticmethod
  def angle_query(*, macro: Optional["Pp1Macro"] = None, name: Optional["SrcByteRange"] = None, definition: Optional[Union[Just["SrcLoc"], Just[None]]] = None, expand: Optional[bool] = None, source: Optional["SrcRange"] = None) -> "Pp1Use":
    raise Exception("this function can only be called from @angle_query")



class Pp1Include(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, path: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, path, 'path'), angle_for(__env, source, 'source')]))
    return f"pp1.Include.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Include

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, path: Optional["SrcByteRange"] = None, source: Optional["SrcRange"] = None) -> "Pp1Include":
    raise Exception("this function can only be called from @angle_query")



class Pp1Macro(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"pp1.Macro.1 { query_fields if query_fields else '_' }", Macro

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "Pp1Macro":
    raise Exception("this function can only be called from @angle_query")






