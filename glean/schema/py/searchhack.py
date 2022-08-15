# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSSearchHackQueryToScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.hack.QueryToScope.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSSearchHackQueryToScope":
    raise Exception("this function can only be called from @angle_query")

class GSSearchHackSearchInEnum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.hack.SearchInEnum.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSSearchHackSearchInEnum":
    raise Exception("this function can only be called from @angle_query")

class GSSearchHackSearchInContext(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.hack.SearchInContext.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSSearchHackSearchInContext":
    raise Exception("this function can only be called from @angle_query")

class GSSearchHackSearchInNamespace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.hack.SearchInNamespace.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSSearchHackSearchInNamespace":
    raise Exception("this function can only be called from @angle_query")

class GSSearchHackSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.hack.SearchByName.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSSearchHackSearchByName":
    raise Exception("this function can only be called from @angle_query")

class GSSearchHackSearchInContainerOrEnum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.hack.SearchInContainerOrEnum.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSSearchHackSearchInContainerOrEnum":
    raise Exception("this function can only be called from @angle_query")

class GSSearchHackSearchInContainer(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.hack.SearchInContainer.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSSearchHackSearchInContainer":
    raise Exception("this function can only be called from @angle_query")


