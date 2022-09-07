# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate


from glean.schema.codebuck.types import (
    buckEntity,
)




class CodeBuckEntity(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], locator: ast.Expr, file: ast.Expr, definition: ast.Expr) -> Tuple[str, Struct]:
    return f"code.buck.Entity.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, locator, 'locator'), angle_for(__env, file, 'file'), angle_for(__env, definition, 'definition')])) or '_' } }}", buckEntity

  @staticmethod
  def angle_query_locator(*, locator: "BuckLocator") -> "CodeBuckEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_file(*, file: "SrcFile") -> "CodeBuckEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_definition(*, definition: "BuckDefinition") -> "CodeBuckEntity":
    raise Exception("this function can only be called from @angle_query")





