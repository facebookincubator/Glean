# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate


from glean.schema.searcherlang.types import (
    erlangSearchByFQN,
    erlangSearchByName,
)


class SearchErlangSearchByFQN(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], module: ast.Expr, name: ast.Expr, arity: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.erlang.SearchByFQN.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, module, 'module'), angle_for(__env, name, 'name'), angle_for(__env, arity, 'arity'), angle_for(__env, entity, 'entity')])) or '_' } }}", erlangSearchByFQN

  @staticmethod
  def angle_query(*, module: Optional[str] = None, name: Optional[str] = None, arity: Optional[int] = None, entity: Optional["CodeErlangEntity"] = None) -> "SearchErlangSearchByFQN":
    raise Exception("this function can only be called from @angle_query")



class SearchErlangSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.erlang.SearchByName.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')])) or '_' } }}", erlangSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional["CodeErlangEntity"] = None) -> "SearchErlangSearchByName":
    raise Exception("this function can only be called from @angle_query")






