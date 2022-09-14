# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.pp1 import *
from glean.schema.py.src import *


from glean.schema.code_pp.types import (
    Entity,
)




class CodePpEntity(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], define: ast.Expr, undef: ast.Expr, include_: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, define, 'define'), angle_for(__env, undef, 'undef'), angle_for(__env, include_, 'include_')]))
    return f"code.pp.Entity.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Entity

  @staticmethod
  def angle_query_define(*, define: Optional["Pp1Define"] = None) -> "CodePpEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_undef(*, undef: Optional["Pp1Undef"] = None) -> "CodePpEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_include_(*, include_: Optional["SrcFile"] = None) -> "CodePpEntity":
    raise Exception("this function can only be called from @angle_query")





