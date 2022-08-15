# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSSearchHsSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.hs.SearchByName.9 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSSearchHsSearchByName":
    raise Exception("this function can only be called from @angle_query")


