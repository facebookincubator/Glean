# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSSearchCxxSearchBySelector(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.cxx.SearchBySelector.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSSearchCxxSearchBySelector":
    raise Exception("this function can only be called from @angle_query")

class GSSearchCxxSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.cxx.SearchByScope.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSSearchCxxSearchByScope":
    raise Exception("this function can only be called from @angle_query")

class GSSearchCxxQueryToQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.cxx.QueryToQName.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSSearchCxxQueryToQName":
    raise Exception("this function can only be called from @angle_query")

class GSSearchCxxGlobalDeclarationWithName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.cxx.GlobalDeclarationWithName.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSSearchCxxGlobalDeclarationWithName":
    raise Exception("this function can only be called from @angle_query")

class GSSearchCxxDeclIsDefn(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.cxx.DeclIsDefn.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSSearchCxxDeclIsDefn":
    raise Exception("this function can only be called from @angle_query")

class GSSearchCxxQueryToScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.cxx.QueryToScope.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSSearchCxxQueryToScope":
    raise Exception("this function can only be called from @angle_query")

class GSSearchCxxSearchByNameAndScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.cxx.SearchByNameAndScope.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSSearchCxxSearchByNameAndScope":
    raise Exception("this function can only be called from @angle_query")

class GSSearchCxxEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.cxx.EntityUses.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSSearchCxxEntityUses":
    raise Exception("this function can only be called from @angle_query")

class GSSearchCxxQueryToNSQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"search.cxx.QueryToNSQName.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSSearchCxxQueryToNSQName":
    raise Exception("this function can only be called from @angle_query")


