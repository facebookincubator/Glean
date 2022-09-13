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
    return f"code.pp.Entity.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, define, 'define'), angle_for(__env, undef, 'undef'), angle_for(__env, include_, 'include_')])) or '_' } }}", Entity

  @staticmethod
  def angle_query_define(*, define: "Pp1Define") -> "CodePpEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_undef(*, undef: "Pp1Undef") -> "CodePpEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_include_(*, include_: "SrcFile") -> "CodePpEntity":
    raise Exception("this function can only be called from @angle_query")





