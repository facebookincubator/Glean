# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class SearchErlangSearchByFQN(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.erlang.SearchByFQN.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchErlangSearchByFQN":
    raise Exception("this function can only be called from @angle_query")

class SearchErlangSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.erlang.SearchByName.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchErlangSearchByName":
    raise Exception("this function can only be called from @angle_query")


