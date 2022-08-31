# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.cxx1 import *


from glean.schema.searchcxx.types import (
    cxxSearchBySelector,
    cxxSearchByScope,
    cxxQueryToQName,
    cxxGlobalDeclarationWithName,
    cxxDeclIsDefn,
    cxxQueryToScope,
    cxxSearchByNameAndScope,
    cxxEntityUses,
    cxxQueryToNSQName,
)


class SearchCxxSearchBySelector(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], selector: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.cxx.SearchBySelector.5 {{ selector = {angle_for(__env, selector)}, entity = {angle_for(__env, entity)} }}", cxxSearchBySelector

  @staticmethod
  def angle_query(*, selector: Optional["Cxx1ObjcSelector"] = None, entity: Optional[Tuple[()]] = None) -> "SearchCxxSearchBySelector":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], scope: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.cxx.SearchByScope.5 {{ scope = {angle_for(__env, scope)}, entity = {angle_for(__env, entity)} }}", cxxSearchByScope

  @staticmethod
  def angle_query(*, scope: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCxxSearchByScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxQueryToQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], query: ast.Expr, scope: ast.Expr) -> Tuple[str, Struct]:
    return f"search.cxx.QueryToQName.5 {{ query = {angle_for(__env, query)}, scope = {angle_for(__env, scope)} }}", cxxQueryToQName

  @staticmethod
  def angle_query(*, query: Optional[Tuple[()]] = None, scope: Optional["Cxx1QName"] = None) -> "SearchCxxQueryToQName":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxGlobalDeclarationWithName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"search.cxx.GlobalDeclarationWithName.5 {{ name = {angle_for(__env, name)}, decl = {angle_for(__env, decl)} }}", cxxGlobalDeclarationWithName

  @staticmethod
  def angle_query(*, name: Optional["Cxx1Name"] = None, decl: Optional[Tuple[()]] = None) -> "SearchCxxGlobalDeclarationWithName":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxDeclIsDefn(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, defn: ast.Expr) -> Tuple[str, Struct]:
    return f"search.cxx.DeclIsDefn.5 {{ decl = {angle_for(__env, decl)}, defn = {angle_for(__env, defn)} }}", cxxDeclIsDefn

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, defn: Optional[Tuple[()]] = None) -> "SearchCxxDeclIsDefn":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxQueryToScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], query: ast.Expr, scope: ast.Expr) -> Tuple[str, Struct]:
    return f"search.cxx.QueryToScope.5 {{ query = {angle_for(__env, query)}, scope = {angle_for(__env, scope)} }}", cxxQueryToScope

  @staticmethod
  def angle_query(*, query: Optional[Tuple[()]] = None, scope: Optional[Tuple[()]] = None) -> "SearchCxxQueryToScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxSearchByNameAndScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.cxx.SearchByNameAndScope.5 {{ name = {angle_for(__env, name)}, scope = {angle_for(__env, scope)}, entity = {angle_for(__env, entity)} }}", cxxSearchByNameAndScope

  @staticmethod
  def angle_query(*, name: Optional["Cxx1Name"] = None, scope: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchCxxSearchByNameAndScope":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, uses: ast.Expr) -> Tuple[str, Struct]:
    return f"search.cxx.EntityUses.5 {{ entity = {angle_for(__env, entity)}, uses = {angle_for(__env, uses)} }}", cxxEntityUses

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, uses: Optional["Cxx1TargetUses"] = None) -> "SearchCxxEntityUses":
    raise Exception("this function can only be called from @angle_query")

class SearchCxxQueryToNSQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], query: ast.Expr, scope: ast.Expr) -> Tuple[str, Struct]:
    return f"search.cxx.QueryToNSQName.5 {{ query = {angle_for(__env, query)}, scope = {angle_for(__env, scope)} }}", cxxQueryToNSQName

  @staticmethod
  def angle_query(*, query: Optional[Tuple[()]] = None, scope: Optional["Cxx1NamespaceQName"] = None) -> "SearchCxxQueryToNSQName":
    raise Exception("this function can only be called from @angle_query")


