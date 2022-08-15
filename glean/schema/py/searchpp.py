# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class SearchPpSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.pp.SearchByName.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchPpSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchPpSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.pp.SearchByName.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchPpSearchByName":
    raise Exception("this function can only be called from @angle_query")


