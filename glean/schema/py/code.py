# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R


from glean.schema.code.types import (
    EntityLanguageLSIF,
    EntityLanguage,
)


class CodeEntityLanguageLSIF(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, language: ast.Expr) -> Tuple[str, Struct]:
    return f"code.EntityLanguageLSIF.24 {{ entity = {angle_for(__env, entity)}, language = {angle_for(__env, language)} }}", EntityLanguageLSIF

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, language: Optional[Tuple[()]] = None) -> "CodeEntityLanguageLSIF":
    raise Exception("this function can only be called from @angle_query")

class CodeEntityLanguage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, language: ast.Expr) -> Tuple[str, Struct]:
    return f"code.EntityLanguage.24 {{ entity = {angle_for(__env, entity)}, language = {angle_for(__env, language)} }}", EntityLanguage

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, language: Optional[Tuple[()]] = None) -> "CodeEntityLanguage":
    raise Exception("this function can only be called from @angle_query")


