# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.searchcxx.types import (
    CxxSearchBySelector,
    CxxSearchByScope,
    CxxQueryToQName,
    CxxGlobalDeclarationWithName,
    CxxDeclIsDefn,
    CxxQueryToScope,
    CxxSearchByNameAndScope,
    CxxEntityUses,
    CxxQueryToNSQName,
)


class SearchCxxSearchBySelector(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.cxx.SearchBySelector.5 { { } }", CxxSearchBySelector

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCxxSearchBySelector":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.cxx.SearchByScope.5 { { } }", CxxSearchByScope

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCxxSearchByScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxQueryToQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.cxx.QueryToQName.5 { { } }", CxxQueryToQName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCxxQueryToQName":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxGlobalDeclarationWithName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.cxx.GlobalDeclarationWithName.5 { { } }", CxxGlobalDeclarationWithName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCxxGlobalDeclarationWithName":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxDeclIsDefn(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.cxx.DeclIsDefn.5 { { } }", CxxDeclIsDefn

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCxxDeclIsDefn":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxQueryToScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.cxx.QueryToScope.5 { { } }", CxxQueryToScope

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCxxQueryToScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxSearchByNameAndScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.cxx.SearchByNameAndScope.5 { { } }", CxxSearchByNameAndScope

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCxxSearchByNameAndScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.cxx.EntityUses.5 { { } }", CxxEntityUses

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCxxEntityUses":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxQueryToNSQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.cxx.QueryToNSQName.5 { { } }", CxxQueryToNSQName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchCxxQueryToNSQName":
    raise Exception("this function can only be called from @angle_query")


