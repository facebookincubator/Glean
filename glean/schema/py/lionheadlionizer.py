# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.cxx1 import *
from glean.schema.py.docmarkup import *


from glean.schema.lionheadlionizer.types import (
    lionizerFindFunction,
    lionizerFindFunctionWithDef,
)


class LionheadLionizerFindFunction(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], key: ast.Expr, value: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"lionhead.lionizer.FindFunction.11 {{ key = {angle_for(__env, key)}, value = {angle_for(__env, value)}, declaration = {angle_for(__env, declaration)} }}", lionizerFindFunction

  @staticmethod
  def angle_query(*, key: Optional["DocmarkupDocAttrKey"] = None, value: Optional[Tuple[()]] = None, declaration: Optional["Cxx1FunctionDeclaration"] = None) -> "LionheadLionizerFindFunction":
    raise Exception("this function can only be called from @angle_query")

class LionheadLionizerFindFunctionWithDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], key: ast.Expr, value: ast.Expr, declaration: ast.Expr, definition: ast.Expr) -> Tuple[str, Struct]:
    return f"lionhead.lionizer.FindFunctionWithDef.11 {{ key = {angle_for(__env, key)}, value = {angle_for(__env, value)}, declaration = {angle_for(__env, declaration)}, definition = {angle_for(__env, definition)} }}", lionizerFindFunctionWithDef

  @staticmethod
  def angle_query(*, key: Optional["DocmarkupDocAttrKey"] = None, value: Optional[Tuple[()]] = None, declaration: Optional["Cxx1FunctionDeclaration"] = None, definition: Optional["Cxx1FunctionDefinition"] = None) -> "LionheadLionizerFindFunctionWithDef":
    raise Exception("this function can only be called from @angle_query")


