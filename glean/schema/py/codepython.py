# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate


from glean.schema.codepython.types import (
    pythonAnnotations,
    pythonEntity,
)




class CodePythonAnnotations(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decorators: ast.Expr) -> Tuple[str, Struct]:
    return f"code.python.Annotations.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, decorators, 'decorators')])) or '_' } }}", pythonAnnotations

  @staticmethod
  def angle_query_decorators(*, decorators: List["PythonDecorator"]) -> "CodePythonAnnotations":
    raise Exception("this function can only be called from @angle_query")




class CodePythonEntity(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr) -> Tuple[str, Struct]:
    return f"code.python.Entity.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl')])) or '_' } }}", pythonEntity

  @staticmethod
  def angle_query_decl(*, decl: "PythonDeclaration") -> "CodePythonEntity":
    raise Exception("this function can only be called from @angle_query")





