# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.searchhs.types import (
    HsSearchByName,
)


class SearchHsSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.hs.SearchByName.9 {{ }}", HsSearchByName
    return f"search.hs.SearchByName.9 {{ name = _, entity = _ }}", HsSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchHsSearchByName":
    raise Exception("this function can only be called from @angle_query")


