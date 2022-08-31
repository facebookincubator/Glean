# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R


from glean.schema.searcherlang.types import (
    erlangSearchByFQN,
    erlangSearchByName,
)


class SearchErlangSearchByFQN(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], module: ast.Expr, name: ast.Expr, arity: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.erlang.SearchByFQN.4 {{ module = {angle_for(__env, module)}, name = {angle_for(__env, name)}, arity = {angle_for(__env, arity)}, entity = {angle_for(__env, entity)} }}", erlangSearchByFQN

  @staticmethod
  def angle_query(*, module: Optional[str] = None, name: Optional[str] = None, arity: Optional[int] = None, entity: Optional[Tuple[()]] = None) -> "SearchErlangSearchByFQN":
    raise Exception("this function can only be called from @angle_query")

class SearchErlangSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.erlang.SearchByName.4 {{ name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)} }}", erlangSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchErlangSearchByName":
    raise Exception("this function can only be called from @angle_query")


