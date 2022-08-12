# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSCodeEntityLanguageLSIF(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"code.EntityLanguageLSIF.24 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodeEntityLanguageLSIF":
    raise Exception("this function can only be called from @angle_query")

class GSCodeEntityLanguage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"code.EntityLanguage.24 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodeEntityLanguage":
    raise Exception("this function can only be called from @angle_query")


