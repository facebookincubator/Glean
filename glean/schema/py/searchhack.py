# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.searchhack.types import (
    HackQueryToScope,
    HackSearchInEnum,
    HackSearchInContext,
    HackSearchInNamespace,
    HackSearchByName,
    HackSearchInContainerOrEnum,
    HackSearchInContainer,
)


class SearchHackQueryToScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.hack.QueryToScope.7 { { } }", HackQueryToScope

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchHackQueryToScope":
    raise Exception("this function can only be called from @angle_query")

class SearchHackSearchInEnum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.hack.SearchInEnum.7 { { } }", HackSearchInEnum

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchHackSearchInEnum":
    raise Exception("this function can only be called from @angle_query")

class SearchHackSearchInContext(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.hack.SearchInContext.7 { { } }", HackSearchInContext

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchHackSearchInContext":
    raise Exception("this function can only be called from @angle_query")

class SearchHackSearchInNamespace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.hack.SearchInNamespace.7 { { } }", HackSearchInNamespace

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchHackSearchInNamespace":
    raise Exception("this function can only be called from @angle_query")

class SearchHackSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.hack.SearchByName.7 { { } }", HackSearchByName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchHackSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchHackSearchInContainerOrEnum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.hack.SearchInContainerOrEnum.7 { { } }", HackSearchInContainerOrEnum

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchHackSearchInContainerOrEnum":
    raise Exception("this function can only be called from @angle_query")

class SearchHackSearchInContainer(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.hack.SearchInContainer.7 { { } }", HackSearchInContainer

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchHackSearchInContainer":
    raise Exception("this function can only be called from @angle_query")


