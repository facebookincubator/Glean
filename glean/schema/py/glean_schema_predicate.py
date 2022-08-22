# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
from thrift.py3 import Struct

class GleanSchemaPredicate:
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query(*, arg: str) -> "GleanSchemaPredicate":
    raise Exception("this function can only be called from @angle_query")

