# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSCodemarkupSearchSearchByNameAndKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.search.SearchByNameAndKind.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupSearchSearchByNameAndKind":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupSearchSearchEntityByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.search.SearchEntityByName.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupSearchSearchEntityByName":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupSearchSearchEntityByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.search.SearchEntityByLowerCaseName.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupSearchSearchEntityByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupSearchSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.search.SearchByName.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupSearchSearchByName":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupSearchEntityLocationAndKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.search.EntityLocationAndKind.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupSearchEntityLocationAndKind":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupSearchSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.search.SearchByScope.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupSearchSearchByScope":
    raise Exception("this function can only be called from @angle_query")


