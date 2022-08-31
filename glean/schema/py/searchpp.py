# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.pp1 import *


from glean.schema.searchpp.types import (
    ppSearchByName,
    ppSearchByName,
)


class SearchPpSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], macro: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.pp.SearchByName.2 {{ macro = {angle_for(__env, macro)}, entity = {angle_for(__env, entity)} }}", ppSearchByName

  @staticmethod
  def angle_query(*, macro: Optional["Pp1Macro"] = None, entity: Optional["Pp1Define"] = None) -> "SearchPpSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchPpSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"search.pp.SearchByName.1 {{ name = {angle_for(__env, name)}, entity = {angle_for(__env, entity)} }}", ppSearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional["Pp1Define"] = None) -> "SearchPpSearchByName":
    raise Exception("this function can only be called from @angle_query")


