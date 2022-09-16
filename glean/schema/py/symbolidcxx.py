# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate , Just, InnerGleanSchemaPredicate
from glean.client.py3.angle_query import angle_for, R
from glean.schema.py.codecxx import *
from glean.schema.py.cxx1 import *


from glean.schema.symbolid_cxx.types import (
    DefinitionOfDecl,
    LookupFunctionDeclaration,
    LookupNamespaceDeclaration,
    LookupFunctionDefinition,
    LookupDeclaration,
    LookupEnumerator,
    LookupDefinition,
    LookupNamespaceDefinition,
)


class SymbolidCxxDefinitionOfDecl(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, entity, 'entity')]))
    return f"symbolid.cxx.DefinitionOfDecl.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DefinitionOfDecl

  @staticmethod
  def angle_query(*, decl: Optional["Cxx1Declaration"] = None, entity: Optional["CodeCxxEntity"] = None) -> "SymbolidCxxDefinitionOfDecl":
    raise Exception("this function can only be called from @angle_query")



class SymbolidCxxLookupFunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, decl, 'decl')]))
    return f"symbolid.cxx.LookupFunctionDeclaration.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", LookupFunctionDeclaration

  @staticmethod
  def angle_query(*, name: Optional["Cxx1FunctionName"] = None, scope: Optional["Cxx1Scope"] = None, decl: Optional["Cxx1Declaration"] = None) -> "SymbolidCxxLookupFunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")



class SymbolidCxxLookupNamespaceDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, parent: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, parent, 'parent'), angle_for(__env, decl, 'decl')]))
    return f"symbolid.cxx.LookupNamespaceDeclaration.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", LookupNamespaceDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Union[Just["Cxx1Name"], Just[None]]] = None, parent: Optional[Union[Just["Cxx1NamespaceQName"], Just[None]]] = None, decl: Optional["Cxx1Declaration"] = None) -> "SymbolidCxxLookupNamespaceDeclaration":
    raise Exception("this function can only be called from @angle_query")



class SymbolidCxxLookupFunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, entity, 'entity')]))
    return f"symbolid.cxx.LookupFunctionDefinition.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", LookupFunctionDefinition

  @staticmethod
  def angle_query(*, name: Optional["Cxx1FunctionName"] = None, scope: Optional["Cxx1Scope"] = None, entity: Optional["CodeCxxEntity"] = None) -> "SymbolidCxxLookupFunctionDefinition":
    raise Exception("this function can only be called from @angle_query")



class SymbolidCxxLookupDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, decl, 'decl')]))
    return f"symbolid.cxx.LookupDeclaration.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", LookupDeclaration

  @staticmethod
  def angle_query(*, name: Optional["Cxx1Name"] = None, scope: Optional["Cxx1Scope"] = None, decl: Optional["Cxx1Declaration"] = None) -> "SymbolidCxxLookupDeclaration":
    raise Exception("this function can only be called from @angle_query")



class SymbolidCxxLookupEnumerator(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, parent: ast.Expr, scope: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, parent, 'parent'), angle_for(__env, scope, 'scope'), angle_for(__env, decl, 'decl')]))
    return f"symbolid.cxx.LookupEnumerator.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", LookupEnumerator

  @staticmethod
  def angle_query(*, name: Optional["Cxx1Name"] = None, parent: Optional["Cxx1Name"] = None, scope: Optional["Cxx1Scope"] = None, decl: Optional["Cxx1Enumerator"] = None) -> "SymbolidCxxLookupEnumerator":
    raise Exception("this function can only be called from @angle_query")



class SymbolidCxxLookupDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, scope: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, scope, 'scope'), angle_for(__env, entity, 'entity')]))
    return f"symbolid.cxx.LookupDefinition.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", LookupDefinition

  @staticmethod
  def angle_query(*, name: Optional["Cxx1Name"] = None, scope: Optional["Cxx1Scope"] = None, entity: Optional["CodeCxxEntity"] = None) -> "SymbolidCxxLookupDefinition":
    raise Exception("this function can only be called from @angle_query")



class SymbolidCxxLookupNamespaceDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, parent: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, parent, 'parent'), angle_for(__env, entity, 'entity')]))
    return f"symbolid.cxx.LookupNamespaceDefinition.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", LookupNamespaceDefinition

  @staticmethod
  def angle_query(*, name: Optional[Union[Just["Cxx1Name"], Just[None]]] = None, parent: Optional[Union[Just["Cxx1NamespaceQName"], Just[None]]] = None, entity: Optional["CodeCxxEntity"] = None) -> "SymbolidCxxLookupNamespaceDefinition":
    raise Exception("this function can only be called from @angle_query")






