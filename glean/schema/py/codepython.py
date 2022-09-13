# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.python import *


from glean.schema.code_python.types import (
    Annotations,
    Entity,
)




class CodePythonAnnotations(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decorators: ast.Expr) -> Tuple[str, Struct]:
    return f"code.python.Annotations.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, decorators, 'decorators')])) or '_' } }}", Annotations

  @staticmethod
  def angle_query_decorators(*, decorators: Optional[List["PythonDecorator"]] = None) -> "CodePythonAnnotations":
    raise Exception("this function can only be called from @angle_query")




class CodePythonEntity(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr) -> Tuple[str, Struct]:
    return f"code.python.Entity.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl')])) or '_' } }}", Entity

  @staticmethod
  def angle_query_decl(*, decl: Optional["PythonDeclaration"] = None) -> "CodePythonEntity":
    raise Exception("this function can only be called from @angle_query")





