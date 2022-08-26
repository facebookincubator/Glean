# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


from glean.schema.searchpp.types import (
    PpSearchByName,
    PpSearchByName,
)


class SearchPpSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.pp.SearchByName.2 {{ }}", PpSearchByName
    return f"search.pp.SearchByName.2 { concatenateFields(key) }", PpSearchByName

  @staticmethod
  def angle_query(*, macro: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchPpSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchPpSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.pp.SearchByName.1 {{ }}", PpSearchByName
    return f"search.pp.SearchByName.1 { concatenateFields(key) }", PpSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchPpSearchByName":
    raise Exception("this function can only be called from @angle_query")


