# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union

class GleanSchemaPredicate:
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query(*, name: str) -> "GleanSchemaPredicate":
    raise Exception("this function can only be called from @angle_query")

