# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R


from glean.schema.sys.types import (
    Blob,
)


class SysBlob(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"sys.Blob.1 {angle_for(__env, arg)}", Blob

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "SysBlob":
    raise Exception("this function can only be called from @angle_query")


