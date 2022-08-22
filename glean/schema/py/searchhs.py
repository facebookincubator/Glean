# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.searchhs.types import (
    HsSearchByName,
)


class SearchHsSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.hs.SearchByName.9 {{ name = _, entity = _ }}", HsSearchByName

  @staticmethod
  def angle_query(*, name: str, entity: Tuple[()]) -> "SearchHsSearchByName":
    raise Exception("this function can only be called from @angle_query")


