# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class CodemarkupSearchSearchByNameAndKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.search.SearchByNameAndKind.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupSearchSearchByNameAndKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchSearchEntityByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.search.SearchEntityByName.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupSearchSearchEntityByName":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchSearchEntityByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.search.SearchEntityByLowerCaseName.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupSearchSearchEntityByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.search.SearchByName.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupSearchSearchByName":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchEntityLocationAndKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.search.EntityLocationAndKind.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupSearchEntityLocationAndKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.search.SearchByScope.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupSearchSearchByScope":
    raise Exception("this function can only be called from @angle_query")


