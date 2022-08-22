# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.searchpp.types import (
    PpSearchByName,
    PpSearchByName,
)


class SearchPpSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.pp.SearchByName.2 {{ macro = _, entity = _ }}", PpSearchByName

  @staticmethod
  def angle_query(*, macro: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchPpSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchPpSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.pp.SearchByName.1 {{ name = _, entity = _ }}", PpSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchPpSearchByName":
    raise Exception("this function can only be called from @angle_query")


