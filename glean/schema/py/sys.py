# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate


from glean.schema.sys.types import (
    Blob,
)


class SysBlob(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"sys.Blob.1 { angle_for(__env, arg, None) or '_' }", Blob

  @staticmethod
  def angle_query(*, arg: Optional[bytes] = None) -> "SysBlob":
    raise Exception("this function can only be called from @angle_query")






