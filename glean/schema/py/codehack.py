# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.hack import *


from glean.schema.code_hack.types import (
    Entity,
    Annotations,
)




class CodeHackEntity(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl')]))
    return f"code.hack.Entity.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Entity

  @staticmethod
  def angle_query_decl(*, decl: "HackDeclaration") -> "CodeHackEntity":
    raise Exception("this function can only be called from @angle_query")




class CodeHackAnnotations(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], attributes: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, attributes, 'attributes')]))
    return f"code.hack.Annotations.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Annotations

  @staticmethod
  def angle_query_attributes(*, attributes: List["HackUserAttribute"]) -> "CodeHackAnnotations":
    raise Exception("this function can only be called from @angle_query")





