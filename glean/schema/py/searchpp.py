# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSSearchPpSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"search.pp.SearchByName.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSSearchPpSearchByName":
    raise Exception("this function can only be called from @angle_query")

class GSSearchPpSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"search.pp.SearchByName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSSearchPpSearchByName":
    raise Exception("this function can only be called from @angle_query")


