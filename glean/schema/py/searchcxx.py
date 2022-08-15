# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class SearchCxxSearchBySelector(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.cxx.SearchBySelector.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCxxSearchBySelector":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.cxx.SearchByScope.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCxxSearchByScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxQueryToQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.cxx.QueryToQName.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCxxQueryToQName":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxGlobalDeclarationWithName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.cxx.GlobalDeclarationWithName.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCxxGlobalDeclarationWithName":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxDeclIsDefn(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.cxx.DeclIsDefn.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCxxDeclIsDefn":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxQueryToScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.cxx.QueryToScope.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCxxQueryToScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxSearchByNameAndScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.cxx.SearchByNameAndScope.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCxxSearchByNameAndScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.cxx.EntityUses.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCxxEntityUses":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxQueryToNSQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.cxx.QueryToNSQName.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCxxQueryToNSQName":
    raise Exception("this function can only be called from @angle_query")


