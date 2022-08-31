# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R


from glean.schema.codemarkupsearch.types import (
    searchSearchByNameAndKind,
    searchSearchEntityByName,
    searchSearchEntityByLowerCaseName,
    searchSearchByName,
    searchEntityLocationAndKind,
    searchSearchByScope,
)


class CodemarkupSearchSearchByNameAndKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], searchcase: ast.Expr, name: ast.Expr, entity: ast.Expr, location: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.search.SearchByNameAndKind.1 {{ searchcase = {angle_for(__env, searchcase)}, name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)}, location = {angle_for(__env, location)}, kind = {angle_for(__env, kind)} }}", searchSearchByNameAndKind

  @staticmethod
  def angle_query(*, searchcase: Optional[Tuple[()]] = None, name: Optional[str] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupSearchSearchByNameAndKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchSearchEntityByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr, location: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.search.SearchEntityByName.1 {{ name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)}, location = {angle_for(__env, location)}, kind = {angle_for(__env, kind)} }}", searchSearchEntityByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupSearchSearchEntityByName":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchSearchEntityByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr, location: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.search.SearchEntityByLowerCaseName.1 {{ name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)}, location = {angle_for(__env, location)}, kind = {angle_for(__env, kind)} }}", searchSearchEntityByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupSearchSearchEntityByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], searchcase: ast.Expr, name: ast.Expr, entity: ast.Expr, location: ast.Expr, kind: ast.Expr, language: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.search.SearchByName.1 {{ searchcase = {angle_for(__env, searchcase)}, name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)}, location = {angle_for(__env, location)}, kind = {angle_for(__env, kind)}, language = {angle_for(__env, language)} }}", searchSearchByName

  @staticmethod
  def angle_query(*, searchcase: Optional[Tuple[()]] = None, name: Optional[str] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None, language: Optional[Tuple[()]] = None) -> "CodemarkupSearchSearchByName":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchEntityLocationAndKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, location: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.search.EntityLocationAndKind.1 {{ entity = {angle_for(__env, entity)}, location = {angle_for(__env, location)}, kind = {angle_for(__env, kind)} }}", searchEntityLocationAndKind

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupSearchEntityLocationAndKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], searchcase: ast.Expr, name: ast.Expr, scope: ast.Expr, entity: ast.Expr, location: ast.Expr, kind: ast.Expr, language: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.search.SearchByScope.1 {{ searchcase = {angle_for(__env, searchcase)}, name = {angle_for(__env, name)}, scope = {angle_for(__env, scope)}, entity = {angle_for(__env, entity)}, location = {angle_for(__env, location)}, kind = {angle_for(__env, kind)}, language = {angle_for(__env, language)} }}", searchSearchByScope

  @staticmethod
  def angle_query(*, searchcase: Optional[Tuple[()]] = None, name: Optional[str] = None, scope: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None, language: Optional[Tuple[()]] = None) -> "CodemarkupSearchSearchByScope":
    raise Exception("this function can only be called from @angle_query")


