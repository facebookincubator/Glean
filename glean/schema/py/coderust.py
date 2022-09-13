# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.rust import *


from glean.schema.code_rust.types import (
    Entity,
)




class CodeRustEntity(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], definition: ast.Expr) -> Tuple[str, Struct]:
    return f"code.rust.Entity.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, definition, 'definition')])) or '_' } }}", Entity

  @staticmethod
  def angle_query_definition(*, definition: "RustDef") -> "CodeRustEntity":
    raise Exception("this function can only be called from @angle_query")





