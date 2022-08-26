# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


from glean.schema.deletthis.types import (
    FileReverseDeps,
)


class DeletthisFileReverseDeps(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"deletthis.FileReverseDeps.15 {{ }}", FileReverseDeps
    return f"deletthis.FileReverseDeps.15 { concatenateFields(key) }", FileReverseDeps

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, referenced_by: Optional[Tuple[()]] = None, via: Optional[Tuple[()]] = None) -> "DeletthisFileReverseDeps":
    raise Exception("this function can only be called from @angle_query")


