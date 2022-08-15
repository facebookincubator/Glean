# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class CodeEntityLanguageLSIF(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"code.EntityLanguageLSIF.24 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodeEntityLanguageLSIF":
    raise Exception("this function can only be called from @angle_query")

class CodeEntityLanguage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"code.EntityLanguage.24 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodeEntityLanguage":
    raise Exception("this function can only be called from @angle_query")


