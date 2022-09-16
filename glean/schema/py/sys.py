# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate , Just, InnerGleanSchemaPredicate
from glean.client.py3.angle_query import angle_for, R


from glean.schema.sys.types import (
    Blob,
)


class SysBlob(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"sys.Blob.1 { query_fields if query_fields else '_' }", Blob

  @staticmethod
  def angle_query(*, arg: Optional[bytes] = None) -> "SysBlob":
    raise Exception("this function can only be called from @angle_query")






