# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate , Just, InnerGleanSchemaPredicate
from glean.client.py3.angle_query import angle_for, R
from glean.schema.py.hack import *


from glean.schema.search_hack.types import (
    QueryToScope,
    SearchInEnum,
    SearchInContext,
    SearchInNamespace,
    SearchByName,
    SearchInContainerOrEnum,
    SearchInContainer,
)


class SearchHackQueryToScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], query: ast.Expr, scopeName: ast.Expr, scopeNamespace: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, query, 'query'), angle_for(__env, scopeName, 'scopeName'), angle_for(__env, scopeNamespace, 'scopeNamespace')]))
    return f"search.hack.QueryToScope.7 { ('{ ' + query_fields + ' }') if query_fields else '_' }", QueryToScope

  @staticmethod
  def angle_query(*, query: Optional[List[str]] = None, scopeName: Optional["HackName"] = None, scopeNamespace: Optional[Union[Just["HackNamespaceQName"], Just[None]]] = None) -> "SearchHackQueryToScope":
    raise Exception("this function can only be called from @angle_query")



class SearchHackSearchInEnum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, enumName: ast.Expr, enumNamespace: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, enumName, 'enumName'), angle_for(__env, enumNamespace, 'enumNamespace'), angle_for(__env, decl, 'decl')]))
    return f"search.hack.SearchInEnum.7 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SearchInEnum

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, enumName: Optional["HackName"] = None, enumNamespace: Optional[Union[Just["HackNamespaceQName"], Just[None]]] = None, decl: Optional["HackDeclaration"] = None) -> "SearchHackSearchInEnum":
    raise Exception("this function can only be called from @angle_query")



class SearchHackSearchInContext(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, contextName: ast.Expr, contextNamespace: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, contextName, 'contextName'), angle_for(__env, contextNamespace, 'contextNamespace'), angle_for(__env, decl, 'decl')]))
    return f"search.hack.SearchInContext.7 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SearchInContext

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, contextName: Optional["HackName"] = None, contextNamespace: Optional[Union[Just["HackNamespaceQName"], Just[None]]] = None, decl: Optional["HackDeclaration"] = None) -> "SearchHackSearchInContext":
    raise Exception("this function can only be called from @angle_query")



class SearchHackSearchInNamespace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, namespace_: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, namespace_, 'namespace_'), angle_for(__env, decl, 'decl')]))
    return f"search.hack.SearchInNamespace.7 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SearchInNamespace

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, namespace_: Optional[Union[Just["HackNamespaceQName"], Just[None]]] = None, decl: Optional["HackDeclaration"] = None) -> "SearchHackSearchInNamespace":
    raise Exception("this function can only be called from @angle_query")



class SearchHackSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, decl, 'decl')]))
    return f"search.hack.SearchByName.7 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SearchByName

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, decl: Optional["HackDeclaration"] = None) -> "SearchHackSearchByName":
    raise Exception("this function can only be called from @angle_query")



class SearchHackSearchInContainerOrEnum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, contextName: ast.Expr, contextNamespace: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, contextName, 'contextName'), angle_for(__env, contextNamespace, 'contextNamespace'), angle_for(__env, decl, 'decl')]))
    return f"search.hack.SearchInContainerOrEnum.7 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SearchInContainerOrEnum

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, contextName: Optional["HackName"] = None, contextNamespace: Optional[Union[Just["HackNamespaceQName"], Just[None]]] = None, decl: Optional["HackDeclaration"] = None) -> "SearchHackSearchInContainerOrEnum":
    raise Exception("this function can only be called from @angle_query")



class SearchHackSearchInContainer(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, containerName: ast.Expr, containerNamespace: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, containerName, 'containerName'), angle_for(__env, containerNamespace, 'containerNamespace'), angle_for(__env, decl, 'decl')]))
    return f"search.hack.SearchInContainer.7 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SearchInContainer

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, containerName: Optional["HackName"] = None, containerNamespace: Optional[Union[Just["HackNamespaceQName"], Just[None]]] = None, decl: Optional["HackDeclaration"] = None) -> "SearchHackSearchInContainer":
    raise Exception("this function can only be called from @angle_query")






