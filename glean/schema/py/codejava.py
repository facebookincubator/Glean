# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate


from glean.schema.codejava.types import (
    javaAnnotations,
    javaEntity,
)




class CodeJavaAnnotations(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], annotations: ast.Expr) -> Tuple[str, Struct]:
    return f"code.java.Annotations.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, annotations, 'annotations')])) or '_' } }}", javaAnnotations

  @staticmethod
  def angle_query_annotations(*, annotations: List["JavaAnnotation"]) -> "CodeJavaAnnotations":
    raise Exception("this function can only be called from @angle_query")




class CodeJavaEntity(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], class_: ast.Expr, definition_: ast.Expr) -> Tuple[str, Struct]:
    return f"code.java.Entity.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, class_, 'class_'), angle_for(__env, definition_, 'definition_')])) or '_' } }}", javaEntity

  @staticmethod
  def angle_query_class_(*, class_: "JavaClassDeclaration") -> "CodeJavaEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_definition_(*, definition_: "JavaDefinition") -> "CodeJavaEntity":
    raise Exception("this function can only be called from @angle_query")





