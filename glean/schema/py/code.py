# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just


from glean.schema.code.types import (
    EntityLanguageLSIF,
    EntityLanguage,
)


class CodeEntityLanguageLSIF(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, language: ast.Expr) -> Tuple[str, Struct]:
    return f"code.EntityLanguageLSIF.24 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, language, 'language')])) or '_' } }}", EntityLanguageLSIF

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, language: Optional[Tuple[()]] = None) -> "CodeEntityLanguageLSIF":
    raise Exception("this function can only be called from @angle_query")



class CodeEntityLanguage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, language: ast.Expr) -> Tuple[str, Struct]:
    return f"code.EntityLanguage.24 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, language, 'language')])) or '_' } }}", EntityLanguage

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, language: Optional[Tuple[()]] = None) -> "CodeEntityLanguage":
    raise Exception("this function can only be called from @angle_query")




