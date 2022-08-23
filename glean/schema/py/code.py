# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.code.types import (
    EntityLanguageLSIF,
    EntityLanguage,
)


class CodeEntityLanguageLSIF(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"code.EntityLanguageLSIF.24 {{ }}", EntityLanguageLSIF
    return f"code.EntityLanguageLSIF.24 {{ entity = _, language = _ }}", EntityLanguageLSIF

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, language: Optional[Tuple[()]] = None) -> "CodeEntityLanguageLSIF":
    raise Exception("this function can only be called from @angle_query")

class CodeEntityLanguage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"code.EntityLanguage.24 {{ }}", EntityLanguage
    return f"code.EntityLanguage.24 {{ entity = _, language = _ }}", EntityLanguage

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, language: Optional[Tuple[()]] = None) -> "CodeEntityLanguage":
    raise Exception("this function can only be called from @angle_query")


