# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSSearchCxxSearchBySelector(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"search.cxx.SearchBySelector.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSSearchCxxSearchBySelector":
    raise Exception("this function can only be called from @angle_query")

class GSSearchCxxSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"search.cxx.SearchByScope.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSSearchCxxSearchByScope":
    raise Exception("this function can only be called from @angle_query")

class GSSearchCxxQueryToQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"search.cxx.QueryToQName.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSSearchCxxQueryToQName":
    raise Exception("this function can only be called from @angle_query")

class GSSearchCxxGlobalDeclarationWithName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"search.cxx.GlobalDeclarationWithName.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSSearchCxxGlobalDeclarationWithName":
    raise Exception("this function can only be called from @angle_query")

class GSSearchCxxDeclIsDefn(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"search.cxx.DeclIsDefn.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSSearchCxxDeclIsDefn":
    raise Exception("this function can only be called from @angle_query")

class GSSearchCxxQueryToScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"search.cxx.QueryToScope.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSSearchCxxQueryToScope":
    raise Exception("this function can only be called from @angle_query")

class GSSearchCxxSearchByNameAndScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"search.cxx.SearchByNameAndScope.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSSearchCxxSearchByNameAndScope":
    raise Exception("this function can only be called from @angle_query")

class GSSearchCxxEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"search.cxx.EntityUses.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSSearchCxxEntityUses":
    raise Exception("this function can only be called from @angle_query")

class GSSearchCxxQueryToNSQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"search.cxx.QueryToNSQName.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSSearchCxxQueryToNSQName":
    raise Exception("this function can only be called from @angle_query")


