# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate


from glean.schema.codemarkupsearch.types import (
    searchSearchByNameAndKind,
    searchSearchEntityByName,
    searchSearchEntityByLowerCaseName,
    searchSearchByName,
    searchEntityLocationAndKind,
    searchSearchByScope,
    searchSearchCase,
)


class CodemarkupSearchSearchByNameAndKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], searchcase: ast.Expr, name: ast.Expr, entity: ast.Expr, location: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.search.SearchByNameAndKind.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, searchcase, 'searchcase'), angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location'), angle_for(__env, kind, 'kind')])) or '_' } }}", searchSearchByNameAndKind

  @staticmethod
  def angle_query(*, searchcase: Optional["CodemarkupSearchSearchCase"] = None, name: Optional[str] = None, entity: Optional["CodeEntity"] = None, location: Optional["CodemarkupTypesLocation"] = None, kind: Optional[Union[Just["CodemarkupTypesSymbolKind"], Just[None]]] = None) -> "CodemarkupSearchSearchByNameAndKind":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupSearchSearchEntityByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr, location: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.search.SearchEntityByName.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location'), angle_for(__env, kind, 'kind')])) or '_' } }}", searchSearchEntityByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional["CodeEntity"] = None, location: Optional["CodemarkupTypesLocation"] = None, kind: Optional[Union[Just["CodemarkupTypesSymbolKind"], Just[None]]] = None) -> "CodemarkupSearchSearchEntityByName":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupSearchSearchEntityByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr, location: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.search.SearchEntityByLowerCaseName.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location'), angle_for(__env, kind, 'kind')])) or '_' } }}", searchSearchEntityByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional["CodeEntity"] = None, location: Optional["CodemarkupTypesLocation"] = None, kind: Optional[Union[Just["CodemarkupTypesSymbolKind"], Just[None]]] = None) -> "CodemarkupSearchSearchEntityByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupSearchSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], searchcase: ast.Expr, name: ast.Expr, entity: ast.Expr, location: ast.Expr, kind: ast.Expr, language: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.search.SearchByName.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, searchcase, 'searchcase'), angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location'), angle_for(__env, kind, 'kind'), angle_for(__env, language, 'language')])) or '_' } }}", searchSearchByName

  @staticmethod
  def angle_query(*, searchcase: Optional["CodemarkupSearchSearchCase"] = None, name: Optional[str] = None, entity: Optional["CodeEntity"] = None, location: Optional["CodemarkupTypesLocation"] = None, kind: Optional[Union[Just["CodemarkupTypesSymbolKind"], Just[None]]] = None, language: Optional["CodeLanguage"] = None) -> "CodemarkupSearchSearchByName":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupSearchEntityLocationAndKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, location: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.search.EntityLocationAndKind.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location'), angle_for(__env, kind, 'kind')])) or '_' } }}", searchEntityLocationAndKind

  @staticmethod
  def angle_query(*, entity: Optional["CodeEntity"] = None, location: Optional["CodemarkupTypesLocation"] = None, kind: Optional[Union[Just["CodemarkupTypesSymbolKind"], Just[None]]] = None) -> "CodemarkupSearchEntityLocationAndKind":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupSearchSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], searchcase: ast.Expr, name: ast.Expr, scope: ast.Expr, entity: ast.Expr, location: ast.Expr, kind: ast.Expr, language: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.search.SearchByScope.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, searchcase, 'searchcase'), angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location'), angle_for(__env, kind, 'kind'), angle_for(__env, language, 'language')])) or '_' } }}", searchSearchByScope

  @staticmethod
  def angle_query(*, searchcase: Optional["CodemarkupSearchSearchCase"] = None, name: Optional[str] = None, scope: Optional[List[str]] = None, entity: Optional["CodeEntity"] = None, location: Optional["CodemarkupTypesLocation"] = None, kind: Optional[Union[Just["CodemarkupTypesSymbolKind"], Just[None]]] = None, language: Optional["CodeLanguage"] = None) -> "CodemarkupSearchSearchByScope":
    raise Exception("this function can only be called from @angle_query")





class CodemarkupSearchSearchCase(Enum):
  Sensitive = 0
  Insensitive = 1


