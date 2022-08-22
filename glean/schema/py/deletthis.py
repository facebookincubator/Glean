# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.deletthis.types import (
    FileReverseDeps,
)


class DeletthisFileReverseDeps(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"deletthis.FileReverseDeps.15 {{ file = _, referenced_by = _, via = _ }}", FileReverseDeps

  @staticmethod
  def angle_query(*, file: Tuple[()], referenced_by: Tuple[()], via: Tuple[()]) -> "DeletthisFileReverseDeps":
    raise Exception("this function can only be called from @angle_query")


