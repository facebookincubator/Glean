# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
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
    if key is None:
      return f"search.cxx.SearchBySelector.5 {{ }}", CxxSearchBySelector
    return f"search.cxx.SearchBySelector.5 {{ selector = _, entity = _ }}", CxxSearchBySelector

  @staticmethod
  def angle_query(*, selector: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCxxSearchBySelector":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.cxx.SearchByScope.5 {{ }}", CxxSearchByScope
    return f"search.cxx.SearchByScope.5 {{ scope = _, entity = _ }}", CxxSearchByScope

  @staticmethod
  def angle_query(*, scope: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCxxSearchByScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxQueryToQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.cxx.QueryToQName.5 {{ }}", CxxQueryToQName
    return f"search.cxx.QueryToQName.5 {{ query = _, scope = _ }}", CxxQueryToQName

  @staticmethod
  def angle_query(*, query: Optional[Tuple[()]] = None, scope: Optional[Tuple[()]] = None) -> "SearchCxxQueryToQName":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxGlobalDeclarationWithName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.cxx.GlobalDeclarationWithName.5 {{ }}", CxxGlobalDeclarationWithName
    return f"search.cxx.GlobalDeclarationWithName.5 {{ name = _, decl = _ }}", CxxGlobalDeclarationWithName

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "SearchCxxGlobalDeclarationWithName":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxDeclIsDefn(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.cxx.DeclIsDefn.5 {{ }}", CxxDeclIsDefn
    return f"search.cxx.DeclIsDefn.5 {{ decl = _, defn = _ }}", CxxDeclIsDefn

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, defn: Optional[Tuple[()]] = None) -> "SearchCxxDeclIsDefn":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxQueryToScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.cxx.QueryToScope.5 {{ }}", CxxQueryToScope
    return f"search.cxx.QueryToScope.5 {{ query = _, scope = _ }}", CxxQueryToScope

  @staticmethod
  def angle_query(*, query: Optional[Tuple[()]] = None, scope: Optional[Tuple[()]] = None) -> "SearchCxxQueryToScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxSearchByNameAndScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.cxx.SearchByNameAndScope.5 {{ }}", CxxSearchByNameAndScope
    return f"search.cxx.SearchByNameAndScope.5 {{ name = _, scope = _, entity = _ }}", CxxSearchByNameAndScope

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, scope: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCxxSearchByNameAndScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.cxx.EntityUses.5 {{ }}", CxxEntityUses
    return f"search.cxx.EntityUses.5 {{ entity = _, uses = _ }}", CxxEntityUses

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, uses: Optional[Tuple[()]] = None) -> "SearchCxxEntityUses":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxQueryToNSQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.cxx.QueryToNSQName.5 {{ }}", CxxQueryToNSQName
    return f"search.cxx.QueryToNSQName.5 {{ query = _, scope = _ }}", CxxQueryToNSQName

  @staticmethod
  def angle_query(*, query: Optional[Tuple[()]] = None, scope: Optional[Tuple[()]] = None) -> "SearchCxxQueryToNSQName":
    raise Exception("this function can only be called from @angle_query")


