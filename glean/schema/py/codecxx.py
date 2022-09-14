# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.cxx1 import *


from glean.schema.code_cxx.types import (
    DeclToDef,
    Definition,
    Entity,
    Annotations,
)


class CodeCxxDeclToDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, defn: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, defn, 'defn')]))
    return f"code.cxx.DeclToDef.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DeclToDef

  @staticmethod
  def angle_query(*, decl: Optional["Cxx1Declaration"] = None, defn: Optional["CodeCxxDefinition"] = None) -> "CodeCxxDeclToDef":
    raise Exception("this function can only be called from @angle_query")





class CodeCxxDefinition(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], record_: ast.Expr, function_: ast.Expr, enum_: ast.Expr, objcMethod: ast.Expr, objcContainer: ast.Expr, variable: ast.Expr, namespace_: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, record_, 'record_'), angle_for(__env, function_, 'function_'), angle_for(__env, enum_, 'enum_'), angle_for(__env, objcMethod, 'objcMethod'), angle_for(__env, objcContainer, 'objcContainer'), angle_for(__env, variable, 'variable'), angle_for(__env, namespace_, 'namespace_')]))
    return f"code.cxx.Definition.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Definition

  @staticmethod
  def angle_query_record_(*, record_: "Cxx1RecordDefinition") -> "CodeCxxDefinition":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_function_(*, function_: "Cxx1FunctionDefinition") -> "CodeCxxDefinition":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_enum_(*, enum_: "Cxx1EnumDefinition") -> "CodeCxxDefinition":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_objcMethod(*, objcMethod: "Cxx1ObjcMethodDefinition") -> "CodeCxxDefinition":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_objcContainer(*, objcContainer: "Cxx1ObjcContainerDefinition") -> "CodeCxxDefinition":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_variable(*, variable: "Cxx1VariableDeclaration") -> "CodeCxxDefinition":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_namespace_(*, namespace_: "Cxx1NamespaceDefinition") -> "CodeCxxDefinition":
    raise Exception("this function can only be called from @angle_query")




class CodeCxxEntity(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, defn: ast.Expr, enumerator: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, defn, 'defn'), angle_for(__env, enumerator, 'enumerator')]))
    return f"code.cxx.Entity.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Entity

  @staticmethod
  def angle_query_decl(*, decl: "Cxx1Declaration") -> "CodeCxxEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_defn(*, defn: "CodeCxxDefinition") -> "CodeCxxEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_enumerator(*, enumerator: "Cxx1Enumerator") -> "CodeCxxEntity":
    raise Exception("this function can only be called from @angle_query")




class CodeCxxAnnotations(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], attributes: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, attributes, 'attributes')]))
    return f"code.cxx.Annotations.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Annotations

  @staticmethod
  def angle_query_attributes(*, attributes: List["Cxx1Attribute"]) -> "CodeCxxAnnotations":
    raise Exception("this function can only be called from @angle_query")





