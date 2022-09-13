# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.codecxx import *
from glean.schema.py.cxx1 import *


from glean.schema.search_cxx.types import (
    SearchBySelector,
    SearchByScope,
    QueryToQName,
    GlobalDeclarationWithName,
    DeclIsDefn,
    QueryToScope,
    SearchByNameAndScope,
    EntityUses,
    QueryToNSQName,
)


class SearchCxxSearchBySelector(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], selector: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.cxx.SearchBySelector.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, selector, 'selector'), angle_for(__env, entity, 'entity')])) or '_' } }}", SearchBySelector

  @staticmethod
  def angle_query(*, selector: Optional["Cxx1ObjcSelector"] = None, entity: Optional["CodeCxxEntity"] = None) -> "SearchCxxSearchBySelector":
    raise Exception("this function can only be called from @angle_query")



class SearchCxxSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], scope: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.cxx.SearchByScope.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, scope, 'scope'), angle_for(__env, entity, 'entity')])) or '_' } }}", SearchByScope

  @staticmethod
  def angle_query(*, scope: Optional["Cxx1Scope"] = None, entity: Optional["CodeCxxEntity"] = None) -> "SearchCxxSearchByScope":
    raise Exception("this function can only be called from @angle_query")



class SearchCxxQueryToQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], query: ast.Expr, scope: ast.Expr) -> Tuple[str, Struct]:
    return f"search.cxx.QueryToQName.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, query, 'query'), angle_for(__env, scope, 'scope')])) or '_' } }}", QueryToQName

  @staticmethod
  def angle_query(*, query: Optional[List[str]] = None, scope: Optional["Cxx1QName"] = None) -> "SearchCxxQueryToQName":
    raise Exception("this function can only be called from @angle_query")



class SearchCxxGlobalDeclarationWithName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"search.cxx.GlobalDeclarationWithName.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, decl, 'decl')])) or '_' } }}", GlobalDeclarationWithName

  @staticmethod
  def angle_query(*, name: Optional["Cxx1Name"] = None, decl: Optional["Cxx1Declaration"] = None) -> "SearchCxxGlobalDeclarationWithName":
    raise Exception("this function can only be called from @angle_query")



class SearchCxxDeclIsDefn(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, defn: ast.Expr) -> Tuple[str, Struct]:
    return f"search.cxx.DeclIsDefn.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, defn, 'defn')])) or '_' } }}", DeclIsDefn

  @staticmethod
  def angle_query(*, decl: Optional["Cxx1Declaration"] = None, defn: Optional["CodeCxxDefinition"] = None) -> "SearchCxxDeclIsDefn":
    raise Exception("this function can only be called from @angle_query")



class SearchCxxQueryToScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], query: ast.Expr, scope: ast.Expr) -> Tuple[str, Struct]:
    return f"search.cxx.QueryToScope.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, query, 'query'), angle_for(__env, scope, 'scope')])) or '_' } }}", QueryToScope

  @staticmethod
  def angle_query(*, query: Optional[List[str]] = None, scope: Optional["Cxx1Scope"] = None) -> "SearchCxxQueryToScope":
    raise Exception("this function can only be called from @angle_query")



class SearchCxxSearchByNameAndScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.cxx.SearchByNameAndScope.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, entity, 'entity')])) or '_' } }}", SearchByNameAndScope

  @staticmethod
  def angle_query(*, name: Optional["Cxx1Name"] = None, scope: Optional["Cxx1Scope"] = None, entity: Optional["CodeCxxEntity"] = None) -> "SearchCxxSearchByNameAndScope":
    raise Exception("this function can only be called from @angle_query")



class SearchCxxEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, uses: ast.Expr) -> Tuple[str, Struct]:
    return f"search.cxx.EntityUses.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, uses, 'uses')])) or '_' } }}", EntityUses

  @staticmethod
  def angle_query(*, entity: Optional["CodeCxxEntity"] = None, uses: Optional["Cxx1TargetUses"] = None) -> "SearchCxxEntityUses":
    raise Exception("this function can only be called from @angle_query")



class SearchCxxQueryToNSQName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], query: ast.Expr, scope: ast.Expr) -> Tuple[str, Struct]:
    return f"search.cxx.QueryToNSQName.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, query, 'query'), angle_for(__env, scope, 'scope')])) or '_' } }}", QueryToNSQName

  @staticmethod
  def angle_query(*, query: Optional[List[str]] = None, scope: Optional["Cxx1NamespaceQName"] = None) -> "SearchCxxQueryToNSQName":
    raise Exception("this function can only be called from @angle_query")






