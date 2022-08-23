# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.searcherlang.types import (
    ErlangSearchByFQN,
    ErlangSearchByName,
)


class SearchErlangSearchByFQN(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.erlang.SearchByFQN.4 {{ }}", ErlangSearchByFQN
    return f"search.erlang.SearchByFQN.4 {{ module = _, name = _, arity = _, entity = _ }}", ErlangSearchByFQN

  @staticmethod
  def angle_query(*, module: Optional[str] = None, name: Optional[str] = None, arity: Optional[int] = None, entity: Optional[Tuple[()]] = None) -> "SearchErlangSearchByFQN":
    raise Exception("this function can only be called from @angle_query")

class SearchErlangSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.erlang.SearchByName.4 {{ }}", ErlangSearchByName
    return f"search.erlang.SearchByName.4 {{ name = _, entity = _ }}", ErlangSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchErlangSearchByName":
    raise Exception("this function can only be called from @angle_query")


