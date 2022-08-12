# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union

class GleanSchemaPredicate:
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query(*, name: str) -> "GleanSchemaPredicate":
    raise Exception("this function can only be called from @angle_query")

