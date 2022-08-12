# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSCodemarkupSearchSearchByNameAndKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.search.SearchByNameAndKind.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupSearchSearchByNameAndKind":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupSearchSearchEntityByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.search.SearchEntityByName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupSearchSearchEntityByName":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupSearchSearchEntityByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.search.SearchEntityByLowerCaseName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupSearchSearchEntityByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupSearchSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.search.SearchByName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupSearchSearchByName":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupSearchEntityLocationAndKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.search.EntityLocationAndKind.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupSearchEntityLocationAndKind":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupSearchSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.search.SearchByScope.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupSearchSearchByScope":
    raise Exception("this function can only be called from @angle_query")


