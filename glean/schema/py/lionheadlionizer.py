# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just
from glean.schema.py.cxx1 import *
from glean.schema.py.docmarkup import *


from glean.schema.lionheadlionizer.types import (
    lionizerFindFunction,
    lionizerFindFunctionWithDef,
)


class LionheadLionizerFindFunction(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], key: ast.Expr, value: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"lionhead.lionizer.FindFunction.11 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, key, 'key'), angle_for(__env, value, 'value'), angle_for(__env, declaration, 'declaration')])) or '_' } }}", lionizerFindFunction

  @staticmethod
  def angle_query(*, key: Optional["DocmarkupDocAttrKey"] = None, value: Optional[Tuple[()]] = None, declaration: Optional["Cxx1FunctionDeclaration"] = None) -> "LionheadLionizerFindFunction":
    raise Exception("this function can only be called from @angle_query")



class LionheadLionizerFindFunctionWithDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], key: ast.Expr, value: ast.Expr, declaration: ast.Expr, definition: ast.Expr) -> Tuple[str, Struct]:
    return f"lionhead.lionizer.FindFunctionWithDef.11 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, key, 'key'), angle_for(__env, value, 'value'), angle_for(__env, declaration, 'declaration'), angle_for(__env, definition, 'definition')])) or '_' } }}", lionizerFindFunctionWithDef

  @staticmethod
  def angle_query(*, key: Optional["DocmarkupDocAttrKey"] = None, value: Optional[Tuple[()]] = None, declaration: Optional["Cxx1FunctionDeclaration"] = None, definition: Optional["Cxx1FunctionDefinition"] = None) -> "LionheadLionizerFindFunctionWithDef":
    raise Exception("this function can only be called from @angle_query")




