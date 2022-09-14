# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.thrift import *


from glean.schema.code_thrift.types import (
    Annotations,
    Entity,
)




class CodeThriftAnnotations(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], annotations: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, annotations, 'annotations')]))
    return f"code.thrift.Annotations.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Annotations

  @staticmethod
  def angle_query_annotations(*, annotations: Optional[List["ThriftStructuredAnnotation"]] = None) -> "CodeThriftAnnotations":
    raise Exception("this function can only be called from @angle_query")




class CodeThriftEntity(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], include_: ast.Expr, named: ast.Expr, exception_: ast.Expr, service_: ast.Expr, constant: ast.Expr, enumValue: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, include_, 'include_'), angle_for(__env, named, 'named'), angle_for(__env, exception_, 'exception_'), angle_for(__env, service_, 'service_'), angle_for(__env, constant, 'constant'), angle_for(__env, enumValue, 'enumValue')]))
    return f"code.thrift.Entity.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Entity

  @staticmethod
  def angle_query_include_(*, include_: Optional["ThriftFile"] = None) -> "CodeThriftEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_named(*, named: Optional["ThriftNamedDecl"] = None) -> "CodeThriftEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_exception_(*, exception_: Optional["ThriftExceptionName"] = None) -> "CodeThriftEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_service_(*, service_: Optional["ThriftServiceName"] = None) -> "CodeThriftEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_constant(*, constant: Optional["ThriftConstant"] = None) -> "CodeThriftEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_enumValue(*, enumValue: Optional["ThriftEnumValue"] = None) -> "CodeThriftEntity":
    raise Exception("this function can only be called from @angle_query")





