# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.buck import *
from glean.schema.py.src import *


from glean.schema.code_buck.types import (
    Entity,
)




class CodeBuckEntity(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], locator: ast.Expr, file: ast.Expr, definition: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, locator, 'locator'), angle_for(__env, file, 'file'), angle_for(__env, definition, 'definition')]))
    return f"code.buck.Entity.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Entity

  @staticmethod
  def angle_query_locator(*, locator: Optional["BuckLocator"] = None) -> "CodeBuckEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_file(*, file: Optional["SrcFile"] = None) -> "CodeBuckEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_definition(*, definition: Optional["BuckDefinition"] = None) -> "CodeBuckEntity":
    raise Exception("this function can only be called from @angle_query")





