# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSCodeEntityLanguageLSIF(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"code.EntityLanguageLSIF.24 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodeEntityLanguageLSIF":
    raise Exception("this function can only be called from @angle_query")

class GSCodeEntityLanguage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"code.EntityLanguage.24 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodeEntityLanguage":
    raise Exception("this function can only be called from @angle_query")


