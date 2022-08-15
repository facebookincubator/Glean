# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class SearchHackQueryToScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.hack.QueryToScope.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchHackQueryToScope":
    raise Exception("this function can only be called from @angle_query")

class SearchHackSearchInEnum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.hack.SearchInEnum.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchHackSearchInEnum":
    raise Exception("this function can only be called from @angle_query")

class SearchHackSearchInContext(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.hack.SearchInContext.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchHackSearchInContext":
    raise Exception("this function can only be called from @angle_query")

class SearchHackSearchInNamespace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.hack.SearchInNamespace.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchHackSearchInNamespace":
    raise Exception("this function can only be called from @angle_query")

class SearchHackSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.hack.SearchByName.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchHackSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchHackSearchInContainerOrEnum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.hack.SearchInContainerOrEnum.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchHackSearchInContainerOrEnum":
    raise Exception("this function can only be called from @angle_query")

class SearchHackSearchInContainer(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.hack.SearchInContainer.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchHackSearchInContainer":
    raise Exception("this function can only be called from @angle_query")


