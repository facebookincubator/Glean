# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.code import *
from glean.schema.py.src import *


from glean.schema.deletthis.types import (
    FileReverseDeps,
)


class DeletthisFileReverseDeps(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, referenced_by: ast.Expr, via: ast.Expr) -> Tuple[str, Struct]:
    return f"deletthis.FileReverseDeps.15 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, referenced_by, 'referenced_by'), angle_for(__env, via, 'via')])) or '_' } }}", FileReverseDeps

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, referenced_by: Optional["SrcFile"] = None, via: Optional["CodeEntity"] = None) -> "DeletthisFileReverseDeps":
    raise Exception("this function can only be called from @angle_query")






