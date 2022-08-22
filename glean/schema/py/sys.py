# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.sys.types import (
    Blob,
)


class SysBlob(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"sys.Blob.1 {json.dumps(key)}", Blob

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "SysBlob":
    raise Exception("this function can only be called from @angle_query")


