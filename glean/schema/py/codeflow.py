# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate , Just, InnerGleanSchemaPredicate
from glean.client.py3.angle_query import angle_for, R
from glean.schema.py.flow import *


from glean.schema.code_flow.types import (
    Entity,
)




class CodeFlowEntity(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, module_: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, module_, 'module_')]))
    return f"code.flow.Entity.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Entity

  @staticmethod
  def angle_query_decl(*, decl: Optional["FlowSomeDeclaration"] = None) -> "CodeFlowEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_module_(*, module_: Optional["FlowModule"] = None) -> "CodeFlowEntity":
    raise Exception("this function can only be called from @angle_query")





