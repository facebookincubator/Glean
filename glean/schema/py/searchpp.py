# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.pp1 import *


from glean.schema.search_pp.types import (
    SearchByName,
    SearchByName,
)


class SearchPpSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], macro: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, macro, 'macro'), angle_for(__env, entity, 'entity')]))
    return f"search.pp.SearchByName.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SearchByName

  @staticmethod
  def angle_query(*, macro: Optional["Pp1Macro"] = None, entity: Optional["Pp1Define"] = None) -> "SearchPpSearchByName":
    raise Exception("this function can only be called from @angle_query")



class SearchPpSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')]))
    return f"search.pp.SearchByName.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional["Pp1Define"] = None) -> "SearchPpSearchByName":
    raise Exception("this function can only be called from @angle_query")






