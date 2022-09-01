# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.hack import *


from glean.schema.searchhack.types import (
    hackQueryToScope,
    hackSearchInEnum,
    hackSearchInContext,
    hackSearchInNamespace,
    hackSearchByName,
    hackSearchInContainerOrEnum,
    hackSearchInContainer,
)


class SearchHackQueryToScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], query: ast.Expr, scopeName: ast.Expr, scopeNamespace: ast.Expr) -> Tuple[str, Struct]:
    return f"search.hack.QueryToScope.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, query, 'query'), angle_for(__env, scopeName, 'scopeName'), angle_for(__env, scopeNamespace, 'scopeNamespace')])) or '_' } }}", hackQueryToScope

  @staticmethod
  def angle_query(*, query: Optional[Tuple[()]] = None, scopeName: Optional["HackName"] = None, scopeNamespace: Optional[Tuple[()]] = None) -> "SearchHackQueryToScope":
    raise Exception("this function can only be called from @angle_query")



class SearchHackSearchInEnum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, enumName: ast.Expr, enumNamespace: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"search.hack.SearchInEnum.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, enumName, 'enumName'), angle_for(__env, enumNamespace, 'enumNamespace'), angle_for(__env, decl, 'decl')])) or '_' } }}", hackSearchInEnum

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, enumName: Optional["HackName"] = None, enumNamespace: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "SearchHackSearchInEnum":
    raise Exception("this function can only be called from @angle_query")



class SearchHackSearchInContext(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, contextName: ast.Expr, contextNamespace: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"search.hack.SearchInContext.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, contextName, 'contextName'), angle_for(__env, contextNamespace, 'contextNamespace'), angle_for(__env, decl, 'decl')])) or '_' } }}", hackSearchInContext

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, contextName: Optional["HackName"] = None, contextNamespace: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "SearchHackSearchInContext":
    raise Exception("this function can only be called from @angle_query")



class SearchHackSearchInNamespace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, namespace_: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"search.hack.SearchInNamespace.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, namespace_, 'namespace_'), angle_for(__env, decl, 'decl')])) or '_' } }}", hackSearchInNamespace

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, namespace_: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "SearchHackSearchInNamespace":
    raise Exception("this function can only be called from @angle_query")



class SearchHackSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"search.hack.SearchByName.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, decl, 'decl')])) or '_' } }}", hackSearchByName

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, decl: Optional[Tuple[()]] = None) -> "SearchHackSearchByName":
    raise Exception("this function can only be called from @angle_query")



class SearchHackSearchInContainerOrEnum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, contextName: ast.Expr, contextNamespace: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"search.hack.SearchInContainerOrEnum.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, contextName, 'contextName'), angle_for(__env, contextNamespace, 'contextNamespace'), angle_for(__env, decl, 'decl')])) or '_' } }}", hackSearchInContainerOrEnum

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, contextName: Optional["HackName"] = None, contextNamespace: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "SearchHackSearchInContainerOrEnum":
    raise Exception("this function can only be called from @angle_query")



class SearchHackSearchInContainer(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, containerName: ast.Expr, containerNamespace: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"search.hack.SearchInContainer.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, containerName, 'containerName'), angle_for(__env, containerNamespace, 'containerNamespace'), angle_for(__env, decl, 'decl')])) or '_' } }}", hackSearchInContainer

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, containerName: Optional["HackName"] = None, containerNamespace: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "SearchHackSearchInContainer":
    raise Exception("this function can only be called from @angle_query")




