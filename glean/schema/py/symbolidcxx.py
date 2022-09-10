# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.cxx1 import *


from glean.schema.symbolidcxx.types import (
    cxxDefinitionOfDecl,
    cxxLookupFunctionDeclaration,
    cxxLookupNamespaceDeclaration,
    cxxLookupFunctionDefinition,
    cxxLookupDeclaration,
    cxxLookupEnumerator,
    cxxLookupDefinition,
    cxxLookupNamespaceDefinition,
)


class SymbolidCxxDefinitionOfDecl(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"symbolid.cxx.DefinitionOfDecl.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, entity, 'entity')])) or '_' } }}", cxxDefinitionOfDecl

  @staticmethod
  def angle_query(*, decl: Optional["Cxx1Declaration"] = None, entity: Optional["CodeCxxEntity"] = None) -> "SymbolidCxxDefinitionOfDecl":
    raise Exception("this function can only be called from @angle_query")



class SymbolidCxxLookupFunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"symbolid.cxx.LookupFunctionDeclaration.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, decl, 'decl')])) or '_' } }}", cxxLookupFunctionDeclaration

  @staticmethod
  def angle_query(*, name: Optional["Cxx1FunctionName"] = None, scope: Optional["Cxx1Scope"] = None, decl: Optional["Cxx1Declaration"] = None) -> "SymbolidCxxLookupFunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")



class SymbolidCxxLookupNamespaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, parent: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"symbolid.cxx.LookupNamespaceDeclaration.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, parent, 'parent'), angle_for(__env, decl, 'decl')])) or '_' } }}", cxxLookupNamespaceDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Union[Just["Cxx1Name"], Just[None]]] = None, parent: Optional[Union[Just["Cxx1NamespaceQName"], Just[None]]] = None, decl: Optional["Cxx1Declaration"] = None) -> "SymbolidCxxLookupNamespaceDeclaration":
    raise Exception("this function can only be called from @angle_query")



class SymbolidCxxLookupFunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"symbolid.cxx.LookupFunctionDefinition.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, entity, 'entity')])) or '_' } }}", cxxLookupFunctionDefinition

  @staticmethod
  def angle_query(*, name: Optional["Cxx1FunctionName"] = None, scope: Optional["Cxx1Scope"] = None, entity: Optional["CodeCxxEntity"] = None) -> "SymbolidCxxLookupFunctionDefinition":
    raise Exception("this function can only be called from @angle_query")



class SymbolidCxxLookupDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"symbolid.cxx.LookupDeclaration.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, decl, 'decl')])) or '_' } }}", cxxLookupDeclaration

  @staticmethod
  def angle_query(*, name: Optional["Cxx1Name"] = None, scope: Optional["Cxx1Scope"] = None, decl: Optional["Cxx1Declaration"] = None) -> "SymbolidCxxLookupDeclaration":
    raise Exception("this function can only be called from @angle_query")



class SymbolidCxxLookupEnumerator(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, parent: ast.Expr, scope: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"symbolid.cxx.LookupEnumerator.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, parent, 'parent'), angle_for(__env, scope, 'scope'), angle_for(__env, decl, 'decl')])) or '_' } }}", cxxLookupEnumerator

  @staticmethod
  def angle_query(*, name: Optional["Cxx1Name"] = None, parent: Optional["Cxx1Name"] = None, scope: Optional["Cxx1Scope"] = None, decl: Optional["Cxx1Enumerator"] = None) -> "SymbolidCxxLookupEnumerator":
    raise Exception("this function can only be called from @angle_query")



class SymbolidCxxLookupDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"symbolid.cxx.LookupDefinition.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, entity, 'entity')])) or '_' } }}", cxxLookupDefinition

  @staticmethod
  def angle_query(*, name: Optional["Cxx1Name"] = None, scope: Optional["Cxx1Scope"] = None, entity: Optional["CodeCxxEntity"] = None) -> "SymbolidCxxLookupDefinition":
    raise Exception("this function can only be called from @angle_query")



class SymbolidCxxLookupNamespaceDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, parent: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"symbolid.cxx.LookupNamespaceDefinition.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, parent, 'parent'), angle_for(__env, entity, 'entity')])) or '_' } }}", cxxLookupNamespaceDefinition

  @staticmethod
  def angle_query(*, name: Optional[Union[Just["Cxx1Name"], Just[None]]] = None, parent: Optional[Union[Just["Cxx1NamespaceQName"], Just[None]]] = None, entity: Optional["CodeCxxEntity"] = None) -> "SymbolidCxxLookupNamespaceDefinition":
    raise Exception("this function can only be called from @angle_query")





