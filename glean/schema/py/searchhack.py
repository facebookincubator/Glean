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
    return f"search.hack.QueryToScope.7 {{ query = {angle_for(__env, query)}, scopeName = {angle_for(__env, scopeName)}, scopeNamespace = {angle_for(__env, scopeNamespace)} }}", hackQueryToScope

  @staticmethod
  def angle_query(*, query: Optional[Tuple[()]] = None, scopeName: Optional["HackName"] = None, scopeNamespace: Optional[Tuple[()]] = None) -> "SearchHackQueryToScope":
    raise Exception("this function can only be called from @angle_query")

class SearchHackSearchInEnum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, enumName: ast.Expr, enumNamespace: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"search.hack.SearchInEnum.7 {{ name = {angle_for(__env, name)}, enumName = {angle_for(__env, enumName)}, enumNamespace = {angle_for(__env, enumNamespace)}, decl = {angle_for(__env, decl)} }}", hackSearchInEnum

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, enumName: Optional["HackName"] = None, enumNamespace: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "SearchHackSearchInEnum":
    raise Exception("this function can only be called from @angle_query")

class SearchHackSearchInContext(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, contextName: ast.Expr, contextNamespace: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"search.hack.SearchInContext.7 {{ name = {angle_for(__env, name)}, contextName = {angle_for(__env, contextName)}, contextNamespace = {angle_for(__env, contextNamespace)}, decl = {angle_for(__env, decl)} }}", hackSearchInContext

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, contextName: Optional["HackName"] = None, contextNamespace: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "SearchHackSearchInContext":
    raise Exception("this function can only be called from @angle_query")

class SearchHackSearchInNamespace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, namespace_: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"search.hack.SearchInNamespace.7 {{ name = {angle_for(__env, name)}, namespace_ = {angle_for(__env, namespace_)}, decl = {angle_for(__env, decl)} }}", hackSearchInNamespace

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, namespace_: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "SearchHackSearchInNamespace":
    raise Exception("this function can only be called from @angle_query")

class SearchHackSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"search.hack.SearchByName.7 {{ name = {angle_for(__env, name)}, decl = {angle_for(__env, decl)} }}", hackSearchByName

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, decl: Optional[Tuple[()]] = None) -> "SearchHackSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchHackSearchInContainerOrEnum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, contextName: ast.Expr, contextNamespace: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"search.hack.SearchInContainerOrEnum.7 {{ name = {angle_for(__env, name)}, contextName = {angle_for(__env, contextName)}, contextNamespace = {angle_for(__env, contextNamespace)}, decl = {angle_for(__env, decl)} }}", hackSearchInContainerOrEnum

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, contextName: Optional["HackName"] = None, contextNamespace: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "SearchHackSearchInContainerOrEnum":
    raise Exception("this function can only be called from @angle_query")

class SearchHackSearchInContainer(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, containerName: ast.Expr, containerNamespace: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"search.hack.SearchInContainer.7 {{ name = {angle_for(__env, name)}, containerName = {angle_for(__env, containerName)}, containerNamespace = {angle_for(__env, containerNamespace)}, decl = {angle_for(__env, decl)} }}", hackSearchInContainer

  @staticmethod
  def angle_query(*, name: Optional["HackName"] = None, containerName: Optional["HackName"] = None, containerNamespace: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "SearchHackSearchInContainer":
    raise Exception("this function can only be called from @angle_query")


