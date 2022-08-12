# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSSearchHackQueryToScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"search.hack.QueryToScope.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSSearchHackQueryToScope":
    raise Exception("this function can only be called from @angle_query")

class GSSearchHackSearchInEnum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"search.hack.SearchInEnum.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSSearchHackSearchInEnum":
    raise Exception("this function can only be called from @angle_query")

class GSSearchHackSearchInContext(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"search.hack.SearchInContext.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSSearchHackSearchInContext":
    raise Exception("this function can only be called from @angle_query")

class GSSearchHackSearchInNamespace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"search.hack.SearchInNamespace.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSSearchHackSearchInNamespace":
    raise Exception("this function can only be called from @angle_query")

class GSSearchHackSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"search.hack.SearchByName.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSSearchHackSearchByName":
    raise Exception("this function can only be called from @angle_query")

class GSSearchHackSearchInContainerOrEnum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"search.hack.SearchInContainerOrEnum.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSSearchHackSearchInContainerOrEnum":
    raise Exception("this function can only be called from @angle_query")

class GSSearchHackSearchInContainer(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"search.hack.SearchInContainer.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSSearchHackSearchInContainer":
    raise Exception("this function can only be called from @angle_query")


